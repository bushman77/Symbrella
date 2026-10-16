defmodule Core.Response.HippocampalAnswerInfluenceTest do
  @moduledoc """
  Regression contract: a recalled episode must affect response synthesis when
  ordinary LLM chat history is empty. A deterministic LLM double reads only
  the actual messages sent to the model; it cannot access Brain.Hippocampus.
  """
  use ExUnit.Case, async: false

  alias Brain.Hippocampus
  alias Core.Brain.Episodes
  alias Core.Response.{Context, LlmChatHistory, LlmSynthesis}

  @question "What was the nickname for our orchard handshake?"
  @nickname "copper-fern-orbit"
  @remembered_reply "We called the orchard handshake #{@nickname}."

  defmodule PromptOnlyLlm do
    use GenServer

    def start_link(owner),
      do: GenServer.start_link(__MODULE__, owner, name: __MODULE__)

    @impl true
    def init(owner), do: {:ok, owner}

    def chat(messages, _opts), do: GenServer.call(__MODULE__, {:chat, messages})

    @impl true
    def handle_call({:chat, messages}, _from, owner) do
      send(owner, {:llm_messages, messages})
      all_text = Enum.map_join(messages, "\n", &Map.get(&1, "content", ""))

      reply =
        if String.contains?(all_text, "copper-fern-orbit") do
          "I recall the nickname copper-fern-orbit."
        else
          "I don't have that nickname available."
        end

      {:reply, {:ok, %{content: reply}}, owner}
    end
  end

  setup do
    old_llm_client = Application.get_env(:core, :llm_client)
    old_web_retrieval = Application.get_env(:core, :web_retrieval, [])

    Application.put_env(:core, :llm_client, PromptOnlyLlm)
    Application.put_env(:core, :web_retrieval,
      Keyword.put(old_web_retrieval, :enabled?, false))

    start_supervised!({PromptOnlyLlm, self()})
    Process.whereis(Hippocampus) || start_supervised!(Hippocampus)
    Hippocampus.reset()

    on_exit(fn ->
      Hippocampus.reset()

      if is_nil(old_llm_client),
        do: Application.delete_env(:core, :llm_client),
        else: Application.put_env(:core, :llm_client, old_llm_client)

      Application.put_env(:core, :web_retrieval, old_web_retrieval)
    end)

    {:ok, session_id: "hippo-answer-#{System.unique_integer([:positive])}"}
  end

  defp generate(session_id, turn_context \\ %{}) do
    features = %{
      session_id: session_id,
      intent: :question,
      confidence: 0.95,
      turn_context: turn_context
    }

    LlmSynthesis.generate(@question, features,
      %{tone: :neutral, mode: :collaborator, action: :answer}, %{}, nil)
  end

  defp seed_episode(session_id) do
    slate = %{
      winners: [
        %{id: "orchard|noun|0", lemma: "orchard"},
        %{id: "handshake|noun|0", lemma: "handshake"}
      ]
    }

    # Deliberately not a verified fact: episodic recollection is contextual
    # memory, not an external source or proof of a geographic/numeric claim.
    Hippocampus.encode(slate,
      %{session_id: session_id, kind: :conversation, memory_note: @remembered_reply})
  end

  defp recall_context(session_id) do
    si = %{
      session_id: session_id,
      sentence: @question,
      source: :user,
      tokens: [
        %{phrase: "orchard", start: 30, stop: 37},
        %{phrase: "handshake", start: 38, stop: 47}
      ],
      evidence: %{}
    }

    attached = Episodes.attach(si, episodes: true)

    assert [%{episode: episode, score: score} | _] =
             get_in(attached, [:evidence, :episodes])

    assert score > 0
    assert episode.meta.memory_note == @remembered_reply
    Context.from_si(attached)
  end

  test "normal chat history still supplies an earlier nickname", %{session_id: sid} do
    LlmChatHistory.record_turn(sid, "Let's name the orchard handshake.",
      @remembered_reply)

    assert {:ok, response, _reflection} = generate(sid)
    assert_receive {:llm_messages, messages}
    assert Enum.any?(messages, &String.contains?(&1["content"], @nickname))
    assert response =~ @nickname
  end

  test "without history, a retrieved hippocampal episode changes the reply", %{
    session_id: sid
  } do
    seed_episode(sid)
    assert LlmChatHistory.messages(sid) == []

    # First prove actual hippocampal retrieval, not just a seeded window.
    turn_context = recall_context(sid)

    # Production synthesis must carry relevant recalled content into the
    # model-visible context, separately from the short chat-history window.
    assert {:ok, response, _reflection} = generate(sid, turn_context)
    assert_receive {:llm_messages, messages}
    assert Enum.count(messages, &(&1["role"] == "user")) == 1
    assert Enum.count(messages, &(&1["role"] == "assistant")) == 0

    system_prompt = hd(messages)["content"]
    assert system_prompt =~ @nickname,
           "A hippocampal hit was attached to SemanticInput but its content " <>
             "never reached response synthesis. Conversation history was empty."

    assert response =~ @nickname
  end

  test "with neither history nor episodic recall, the answer is not invented", %{
    session_id: sid
  } do
    assert LlmChatHistory.messages(sid) == []
    assert {:ok, response, _reflection} = generate(sid)
    assert_receive {:llm_messages, messages}
    refute Enum.any?(messages, &String.contains?(&1["content"], @nickname))
    refute response =~ @nickname
    assert response =~ "don't have"
  end
end
