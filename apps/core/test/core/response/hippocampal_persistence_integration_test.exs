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
  alias Core.Response.{Context, LlmChatHistory, LlmPrompt, ResponseFrame}

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

    current = input(session_id, marker)
    recalled = GenServer.call(reader, {:attach, current,
      %{ignore_head: :never, limit: 3, min_jaccard: 0.1}})

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

    :ok = GenServer.stop(reader, :normal)
  end

  test "a different chat session cannot promote another session's persisted episode to prompt context", %{
    session_id: session_id, marker: marker
  } do
    remembered = "The private #{marker} handshake is copper-fern-orbit."
    assert {:ok, %Db.Episode{}} =
      Db.Episode.insert(%{
        slate: %{winners: [%{lemma: marker}, %{lemma: "handshake"}]},
        meta: %{session_id: session_id, sentence: remembered, kind: :conversation},
        norms: [marker, "handshake"]
      })

    reader = start_isolated_hippocampus()
    different_session = session_id <> "-different"
    current = input(different_session, marker)

    # Backend retrieval may see a lexical candidate, but presentation must
    # enforce exact session provenance in Context.from_si/1.
    recalled = GenServer.call(reader, {:attach, current,
      %{ignore_head: :never, limit: 3, min_jaccard: 0.1}})

    assert [%{score: score} | _] = get_in(recalled, [:evidence, :episodes])
    assert score > 0

    {context, prompt} = project_response(recalled)
    assert context[:episodic_recall] in [nil, []]
    refute prompt =~ "copper-fern-orbit"

    :ok = GenServer.stop(reader, :normal)
  end
end
