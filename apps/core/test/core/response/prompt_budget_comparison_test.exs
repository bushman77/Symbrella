defmodule Core.Response.PromptBudgetComparisonTest do
  use ExUnit.Case, async: false

  alias Core.Response.{
    LlmChatHistory, LlmSynthesis, PromptBudget, PromptHistoryPolicy
  }

  @memory "We called the orchard handshake copper-fern-orbit."
  @question "What nickname did we give our orchard handshake?"

  defmodule FakeBudgetLlm do
    use GenServer

    def start_link(owner),
      do: GenServer.start_link(__MODULE__, owner, name: __MODULE__)

    @impl true
    def init(owner), do: {:ok, owner}

    def chat(messages, opts), do: GenServer.call(__MODULE__, {:chat, messages, opts})

    @impl true
    def handle_call({:chat, messages, _opts}, _from, owner) do
      send(owner, {:budget_chat, messages})

      text = Enum.map_join(messages, "\n", &Map.get(&1, "content", ""))

      answer =
        if String.contains?(text, "copper-fern-orbit"),
          do: "I recall the nickname copper-fern-orbit.",
          else: "I cannot recall the nickname."

      {:reply, {:ok, %{content: answer}}, owner}
    end
  end

  setup do
    previous_client = Application.get_env(:core, :llm_client)
    previous_config = Application.get_env(:core, :llm_synthesis, [])
    previous_retrieval = Application.get_env(:core, :web_retrieval, [])

    Application.put_env(:core, :llm_client, FakeBudgetLlm)
    Application.put_env(:core, :web_retrieval,
      Keyword.put(previous_retrieval, :enabled?, false))

    start_supervised!({FakeBudgetLlm, self()})

    handler = "prompt-budget-comparison-#{System.unique_integer([:positive])}"
    :ok = Core.Telemetry.attach(handler, [:core, :response, :prompt_budget],
      fn _event, measurements, metadata, owner ->
        send(owner, {:prompt_budget, measurements, metadata})
      end, self())

    on_exit(fn ->
      Core.Telemetry.detach(handler)
      Application.put_env(:core, :llm_synthesis, previous_config)
      Application.put_env(:core, :web_retrieval, previous_retrieval)

      if previous_client == nil,
        do: Application.delete_env(:core, :llm_client),
        else: Application.put_env(:core, :llm_client, previous_client)
    end)

    %{session: "budget-test-#{System.unique_integer([:positive])}",
      old_config: previous_config}
  end

  defp episode, do: %{text: @memory, source: :hippocampal_episode}

  defp dialogue(n) do
    Enum.flat_map(1..n, fn index ->
      [
        %{"role" => "user", "content" => "Earlier unrelated topic #{index}"},
        %{"role" => "assistant", "content" => "Prior unrelated reply #{index}"}
      ]
    end)
  end

  test "history selection requires an episode and protects referential follow-ups" do
    history = dialogue(6)
    base = PromptHistoryPolicy.candidate(history, %{}, @question, nil)
    assert base.history == history
    assert base.reason == :no_relevant_episode

    context = %{episodic_recall: [episode()]}
    candidate = PromptHistoryPolicy.candidate(history, context, @question, nil)
    assert candidate.eligible?
    assert length(candidate.history) == 4
    assert candidate.retained_pairs == 2

    assert PromptHistoryPolicy.candidate(history, context,
      "What about that?", nil).reason == :conversational_dependency

    assert PromptHistoryPolicy.candidate(history, context,
      "You tell me", "Where is Japan?").reason == :prior_question_dependency

    assert PromptHistoryPolicy.active_history(history, context,
      @question, nil, mode: :full).actual_history == history

    assert PromptHistoryPolicy.active_history(history, context,
      @question, nil, mode: :selective_recall).actual_history == candidate.history
  end

  test "a 24-turn conversation remains stored while LLM context is bounded", %{
    session: sid, old_config: old_config
  } do
    Application.put_env(:core, :llm_synthesis,
      old_config
      |> Keyword.put(:history_mode, :full)
      |> Keyword.put(:history_turn_pairs, 32)
      |> Keyword.put(:max_item_chars, 900)
      |> Keyword.put(:max_input_estimated_tokens, 3_500))

    for n <- 1..24 do
      LlmChatHistory.record_turn(sid,
        "Turn #{n} describes a persistent object with context " <> String.duplicate("c", 200),
        "Acknowledged turn #{n} with state " <> String.duplicate("s", 160))
    end

    assert length(LlmChatHistory.messages(sid)) == 48

    features = %{session_id: sid, intent: :question, confidence: 0.92}
    decision = %{tone: :neutral, mode: :collaborator, action: :answer}

    assert {:ok, _, _} =
             LlmSynthesis.generate("What did I say most recently?", features, decision, %{}, nil)

    assert_receive {:budget_chat, assembled}
    assert_receive {:prompt_budget, measure, meta}
    actual_history = Enum.slice(assembled, 1, length(assembled) - 2)

    assert length(actual_history) > 12
    assert length(actual_history) < 48
    assert rem(length(actual_history), 2) == 0
    assert Enum.at(actual_history, -2)["content"] =~ "Turn 24"
    assert meta.actual_history_messages == length(actual_history)
    assert measure.estimated_input_tokens <= 3_500
    assert length(LlmChatHistory.messages(sid)) == 48
  end

  test "section estimates are additive and distinguish episodic from source evidence" do
    builder = fn ctx ->
      "Base instructions." <>
        (if Map.has_key?(ctx, :episodic_recall), do: " EPISODE: remembered nickname.", else: "") <>
        (if Map.has_key?(ctx, :web_evidence), do: " RETRIEVED: source excerpt.", else: "") <>
        (if get_in(ctx, [:response_frame, :evidence]), do: " VERIFIED: symbolic proof.", else: "")
    end

    context = %{
      episodic_recall: [episode()],
      web_evidence: %{status: :retrieved},
      response_frame: %{evidence: %{kind: :population, value: 100}}
    }
    history = dialogue(5)
    selective = Enum.take(history, -4)

    b = PromptBudget.measure(context, builder, history, selective, history, @question)

    assert b.estimator == "ceil_chars_div_4"
    assert b.actual == b.full
    assert b.section_estimated_tokens.episodic_recall > 0
    assert b.section_estimated_tokens.verified_evidence > 0
    assert b.section_estimated_tokens.retrieved_evidence > 0
    assert b.section_estimated_tokens.instructions > 0
    assert b.full.components.history > b.selective.components.history
    assert b.full.total == Enum.sum(Map.values(b.full.components))
    assert b.selective.total == Enum.sum(Map.values(b.selective.components))
    assert b.potential_savings_tokens == b.full.total - b.selective.total
  end

  test "the SAME history and recalled episode produce the same answer with fewer estimated tokens", %{
    session: sid, old_config: old_config
  } do
    Application.put_env(:core, :llm_synthesis,
      old_config |> Keyword.put(:history_mode, :full)
        |> Keyword.put(:history_turn_pairs, 6)
        |> Keyword.put(:selective_recent_pairs, 2))

    for n <- 1..6 do
      LlmChatHistory.record_turn(sid,
        "Background topic #{n} that was not relevant to the handshake.",
        "Background answer #{n} with additional conversation detail.")
    end

    features = %{
      session_id: sid, intent: :question, confidence: 0.92,
      turn_context: %{episodic_recall: [episode()]}
    }
    decision = %{tone: :neutral, mode: :collaborator, action: :answer}

    assert {:ok, full_answer, _} =
      LlmSynthesis.generate(@question, features, decision, %{}, nil)
    assert_receive {:budget_chat, full_messages}
    assert_receive {:prompt_budget, full_measure, full_meta}
    assert full_meta.history_mode == :full
    assert full_meta.selection_eligible?
    refute full_meta.selection_applied?
    assert full_measure.estimated_savings_tokens > 0
    assert length(full_messages) == 14

    Application.put_env(:core, :llm_synthesis,
      old_config |> Keyword.put(:history_mode, :selective_recall)
        |> Keyword.put(:history_turn_pairs, 6)
        |> Keyword.put(:selective_recent_pairs, 2))

    assert {:ok, selective_answer, _} =
      LlmSynthesis.generate(@question, features, decision, %{}, nil)
    assert_receive {:budget_chat, selective_messages}
    assert_receive {:prompt_budget, selective_measure, selective_meta}

    assert selective_meta.history_mode == :selective_recall
    assert selective_meta.selection_applied?
    assert length(selective_messages) == 6
    assert full_answer == selective_answer
    assert full_answer =~ "copper-fern-orbit"
    assert selective_measure.estimated_input_tokens < full_measure.estimated_input_tokens
    assert selective_measure.estimated_input_tokens ==
      full_measure.estimated_selective_tokens
    assert selective_measure.estimated_savings_tokens == full_measure.estimated_savings_tokens
    refute inspect(selective_meta) =~ "copper-fern-orbit"
  end
end
