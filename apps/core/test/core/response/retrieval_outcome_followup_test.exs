defmodule Core.Response.RetrievalOutcomeFollowupTest do
  use ExUnit.Case, async: false

  alias Core.Response.{LlmChatHistory, LlmSynthesis, ReflectionLoop}

  defmodule EmptyNews do
    def search(_query, _opts), do: {:ok, []}
  end

  defmodule FollowupLlm do
    use GenServer

    def start_link(test_pid),
      do: GenServer.start_link(__MODULE__, test_pid, name: __MODULE__)

    def chat(messages, _opts),
      do: GenServer.call(__MODULE__, {:chat, messages})

    @impl true
    def init(test_pid), do: {:ok, test_pid}

    @impl true
    def handle_call({:chat, messages}, _from, test_pid) do
      user =
        messages
        |> List.last()
        |> Map.get("content")

      send(test_pid, {:retrieval_llm_message, user})

      answer =
        case user do
          "Search the web for the latest developments." ->
            "I'll search the web for the latest developments in Japan's railgun technology."

          "Well did you?" ->
            "I found some information about Japan's recent railgun technology."

          _ ->
            "I do not know."
        end

      {:reply, {:ok, %{content: answer}}, test_pid}
    end
  end

  setup do
    before_client = Application.get_env(:core, :llm_client)
    before_web = Application.get_env(:core, :web_retrieval)
    before_synthesis = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(:core, :llm_client, FollowupLlm)
    Application.put_env(:core, :web_retrieval,
      enabled?: true,
      provider: Core.Tools.WebRetrieval.Wikipedia,
      news_provider: EmptyNews,
      max_attempts: 2,
      timeout_ms: 200
    )

    Application.put_env(:core, :llm_synthesis,
      Keyword.put(before_synthesis, :evidence_revision_enabled?, false))

    on_exit(fn ->
      restore(:llm_client, before_client)
      restore(:web_retrieval, before_web)
      restore(:llm_synthesis, before_synthesis)
    end)

    :ok
  end

  defp restore(key, nil), do: Application.delete_env(:core, key)
  defp restore(key, value), do: Application.put_env(:core, key, value)

  test "only actual retrieval results create same-session receipts" do
    session = "receipt-#{System.unique_integer([:positive])}"
    other = "other-receipt-#{System.unique_integer([:positive])}"

    assert LlmChatHistory.recent_retrieval(session) == nil
    assert :ok = LlmChatHistory.record_retrieval(session, nil)

    evidence = %{
      relation: :current_events, query: "Japan's railgun development",
      status: :not_found, resolution: :exhausted, attempted_count: 2,
      sources: []
    }

    assert :ok = LlmChatHistory.record_retrieval(session, evidence)

    assert %{
             query: "Japan's railgun development",
             relation: :current_events, status: :not_found,
             resolution: :exhausted, source_count: 0, attempted_count: 2
           } = LlmChatHistory.recent_retrieval(session)

    assert LlmChatHistory.recent_retrieval(other) == nil
    assert LlmChatHistory.recent_retrieval(:global) == nil
    assert :ok = LlmChatHistory.record_retrieval(other, %{relation: :current_events})
    assert LlmChatHistory.recent_retrieval(other) == nil
  end

  test "a completed empty news search and the follow-up are checked end to end" do
    start_supervised!({FollowupLlm, self()})
    session = "followup-#{System.unique_integer([:positive])}"
    opening = "What do you know about Japan's railgun development?"

    # History is stored by Core.Response.plan/2 in production, not by generate.
    LlmChatHistory.record_turn(session, opening, "I could not confirm it.")

    features = %{session_id: session, intent: :ask, confidence: 0.9}
    decision = %{action: :answer_user, tone: :neutral}

    {:ok, search_reply, search_reflection} =
      LlmSynthesis.generate(
        "Search the web for the latest developments.",
        features, decision, %{}, nil
      )

    assert_receive {:retrieval_llm_message, "Search the web for the latest developments."}
    assert search_reflection.status == :repair
    assert :news_lookup_misrepresented in search_reflection.issues
    assert search_reply =~ "no qualifying headlines were returned"
    refute search_reply =~ "I'll search"

    assert %{
             query: "Japan's railgun development", source_count: 0,
             resolution: :exhausted
           } = LlmChatHistory.recent_retrieval(session)

    LlmChatHistory.record_turn(
      session,
      "Search the web for the latest developments.",
      search_reply
    )

    {:ok, followup_reply, followup_reflection} =
      LlmSynthesis.generate(
        "Well did you?",
        features, decision, %{}, nil
      )

    assert_receive {:retrieval_llm_message, "Well did you?"}
    assert followup_reflection.status == :repair
    assert :prior_lookup_followup in followup_reflection.issues
    assert followup_reply =~ "Yes, I searched for dated news"
    assert followup_reply =~ "Japan's railgun development"
    assert followup_reply =~ "no qualifying headlines"
    refute followup_reply =~ "I found some information"
  end

  test "an isolated audit follow-up cannot invent a previous search" do
    session = "isolated-audit-#{System.unique_integer([:positive])}"

    {:ok, reply, review} =
      ReflectionLoop.review(
        "Well did you?",
        "I do not know what you are referring to.",
        %{session_id: session, features: %{intent: :ask, confidence: 0.9}}
      )

    refute :prior_lookup_followup in review.issues
    assert reply != "Yes, I searched for dated news about Japan's railgun development."
  end
end
