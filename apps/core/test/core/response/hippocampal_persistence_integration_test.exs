defmodule Core.Response.HippocampalPersistenceIntegrationTest do
  @moduledoc """
  Exercises a real PostgreSQL episode write followed by a newly started,
  independent Hippocampus process. A cold worker must rehydrate enough
  provenance for same-session conversation recall without turning that episode
  into verified external evidence or exposing it to a different chat session.

  The worker is isolated to avoid killing the production singleton shared by
  concurrent umbrella tests.
  """
  use ExUnit.Case, async: false

  import Ecto.Query, only: [from: 2]

  alias Brain.Hippocampus
  alias Core.Response.{Context, LlmChatHistory, LlmPrompt, LlmSynthesis, ResponseFrame}

  defmodule MemoryProbeLlm do
    use GenServer

    def start_link({owner, expected}),
      do: GenServer.start_link(__MODULE__, {owner, expected}, name: __MODULE__)

    @impl true
    def init(state), do: {:ok, state}

    def chat(messages, _opts), do: GenServer.call(__MODULE__, {:chat, messages})

    @impl true
    def handle_call({:chat, messages}, _from, {owner, expected} = state) do
      send(owner, {:persistent_memory_prompt, messages})
      visible = Enum.map_join(messages, "\n", &Map.get(&1, "content", ""))

      result =
        if String.contains?(visible, expected),
          do: "I recall the nickname " <> expected <> ".",
          else: "I do not have that nickname in context."

      {:reply, {:ok, %{content: result}}, state}
    end
  end

  setup do
    session_id = "durable-hippo-#{System.unique_integer([:positive])}"
    marker = "orchard#{System.unique_integer([:positive])}"

    on_exit(fn ->
      # The test uses the shared test database; clean up only its own rows.
      Db.delete_all(from(e in Db.Episode, where: e.session_id == ^session_id))
    end)

    %{session_id: session_id, marker: marker}
  end

  defp start_isolated_hippocampus do
    # start_link/1 runs the same init/1 and database warm-start as the
    # supervised production Hippocampus, but with a private registered name.
    name = {:via, Registry, {Brain.Registry, {:persist_test, make_ref()}}}
    {:ok, pid} = Hippocampus.start_link(name: name)
    pid
  end

  defp input(session_id, marker) do
    %{
      session_id: session_id,
      sentence: "What nickname did we give #{marker} handshake?",
      tokens: [
        %{phrase: marker},
        %{phrase: "handshake"}
      ],
      evidence: %{}
    }
  end

  defp project_response(si) do
    turn_context = Context.from_si(si)

    context =
      Context.from_response_parts(
        si.sentence,
        %{intent: :question, session_id: si.session_id, turn_context: turn_context},
        %{tone: :neutral, mode: :collaborator, action: :answer},
        %{},
        %{wm_items: []}
      )

    prompt =
      context
      |> Map.put(:response_frame, ResponseFrame.build(context))
      |> LlmPrompt.build_system_prompt()

    {turn_context, prompt}
  end

  test "cold start restores persisted same-session episode into model-visible recall", %{
    session_id: session_id, marker: marker
  } do
    nickname = "copper-fern-orbit-#{marker}"
    remembered = "We called the #{marker} handshake #{nickname}."
    slate = %{
      winners: [
        %{id: "#{marker}|noun|0", lemma: marker},
        %{id: "handshake|noun|0", lemma: "handshake"}
      ]
    }

    writer = start_isolated_hippocampus()
    assert :ok = GenServer.call(writer, {:encode, slate,
      %{session_id: session_id, sentence: remembered, kind: :conversation}})

    # A successful in-memory encode alone does not prove persistence.
    row =
      Db.one(from(e in Db.Episode,
        where: e.session_id == ^session_id,
        where: e.sentence == ^remembered,
        order_by: [desc: e.inserted_at],
        limit: 1
      ))

    assert %Db.Episode{} = row
    assert Enum.sort(row.tokens) == Enum.sort([marker, "handshake"])

    # Simulate an application restart with a new Hippocampus process. Its
    # state can only recover the episode from PostgreSQL, not prior RAM.
    :ok = GenServer.stop(writer, :normal)
    reader = start_isolated_hippocampus()

    # First prove the new worker loaded our exact PostgreSQL row. This
    # distinguishes cold-start failures from recall/scoring failures.
    restored = GenServer.call(reader, :snapshot)
    assert Enum.any?(restored.window, fn {_at, episode} ->
      episode.meta.episode_id == row.id
    end), "The fresh hippocampal worker did not rehydrate the persisted episode"

    current = input(session_id, marker)

    # The public attach_episodes/2 API derives lexical cues before calling
    # GenServer. Since this isolated worker uses a private name, explicitly
    # pass the same normalized cues to the internal attachment message.
    recalled = GenServer.call(reader, {:attach, current,
      %{cues: [marker, "handshake"], ignore_head: :never, limit: 3, min_jaccard: 0.1}})

    assert [%{episode: recovered, score: score} | _] =
      get_in(recalled, [:evidence, :episodes])

    assert score > 0
    assert recovered.meta.session_id == session_id
    assert recovered.meta.sentence == remembered
    assert recovered.meta.episode_id == row.id

    assert LlmChatHistory.messages(session_id) == []
    {context, prompt} = project_response(recalled)

    assert context.episodic_recall == [
      %{text: remembered, source: :hippocampal_episode}
    ]
    assert prompt =~ nickname
    assert prompt =~ "recollections, not authoritative evidence"
    assert prompt =~ "verified_evidence=none"

    # The model sees only the generated messages, not the database or the
    # hippocampal GenServer. This proves a cold-start recall affects an answer.
    previous_client = Application.get_env(:core, :llm_client)
    previous_retrieval = Application.get_env(:core, :web_retrieval, [])
    Application.put_env(:core, :llm_client, MemoryProbeLlm)
    Application.put_env(:core, :web_retrieval,
      Keyword.put(previous_retrieval, :enabled?, false))

    on_exit(fn ->
      if previous_client == nil,
        do: Application.delete_env(:core, :llm_client),
        else: Application.put_env(:core, :llm_client, previous_client)

      Application.put_env(:core, :web_retrieval, previous_retrieval)
    end)

    start_supervised!({MemoryProbeLlm, {self(), nickname}})

    assert {:ok, answer, _reflection} =
      LlmSynthesis.generate(current.sentence,
        %{session_id: session_id, intent: :question, turn_context: context},
        %{tone: :neutral, mode: :collaborator, action: :answer},
        %{}, nil)

    assert_receive {:persistent_memory_prompt, messages}
    assert Enum.count(messages, &(&1["role"] == "user")) == 1
    assert Enum.count(messages, &(&1["role"] == "assistant")) == 0
    assert hd(messages)["content"] =~ nickname
    assert answer =~ nickname

    :ok = GenServer.stop(reader, :normal)
  end

  test "a different chat session cannot promote another session's persisted episode to prompt context", %{
    session_id: session_id, marker: marker
  } do
    remembered = "The private #{marker} handshake is copper-fern-orbit."
    assert {:ok, %Db.Episode{} = row} =
      Db.Episode.insert(%{
        slate: %{winners: [%{lemma: marker}, %{lemma: "handshake"}]},
        meta: %{session_id: session_id, sentence: remembered, kind: :conversation},
        norms: [marker, "handshake"]
      })

    reader = start_isolated_hippocampus()
    restored = GenServer.call(reader, :snapshot)
    assert Enum.any?(restored.window, fn {_at, episode} ->
      episode.meta.episode_id == row.id
    end), "The fresh hippocampal worker did not rehydrate the persisted episode"
    different_session = session_id <> "-different"
    current = input(different_session, marker)

    # Backend retrieval may see a lexical candidate, but presentation must
    # enforce exact session provenance in Context.from_si/1.
    recalled = GenServer.call(reader, {:attach, current,
      %{cues: [marker, "handshake"], ignore_head: :never, limit: 3, min_jaccard: 0.1}})

    assert [%{score: score} | _] = get_in(recalled, [:evidence, :episodes])
    assert score > 0

    {context, prompt} = project_response(recalled)
    assert context[:episodic_recall] in [nil, []]
    refute prompt =~ "copper-fern-orbit"

    :ok = GenServer.stop(reader, :normal)
  end
end
