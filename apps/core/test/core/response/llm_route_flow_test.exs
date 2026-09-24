defmodule Core.Response.LlmRouteFlowTest do
  use ExUnit.Case, async: false

  alias Core.Response

  defmodule FakeLlm do
    use GenServer

    def start_link(test_pid) do
      GenServer.start_link(__MODULE__, test_pid, name: __MODULE__)
    end

    def chat(messages, opts) do
      GenServer.call(__MODULE__, {:chat, messages, opts})
    end

    @impl true
    def init(test_pid), do: {:ok, test_pid}

    @impl true
    def handle_call({:chat, messages, opts}, _from, test_pid) do
      send(test_pid, {:llm_chat, messages, opts})

      user_text = messages |> List.last() |> Map.get("content", "")

      text =
        cond do
          String.contains?(user_text, "fine saturday") ->
            "I am currently in a neutral state, with steady pressure. My tone is balanced and unbiased. I am always ready to assist you, regardless of my current state. How may I help you today?"

          String.contains?(user_text, "fuzzy") ->
            "Please provide more information so I can assist you today."

          String.contains?(user_text, "note leak") ->
            "Here is the direct answer.\n\n(Note: The assistant's response is based on internal planning.)"

          String.contains?(user_text, "symbrella label leak") ->
            "Symbrella: Here is the answer."

          String.contains?(user_text, "symbrella continuation leak") ->
            """
            Here is the answer.

            Symbrella:
            What is your name?
            user
            what is my name?
            """

          String.contains?(user_text, "continuation leak") ->
            """
            The sky is blue because shorter blue wavelengths scatter more strongly in the atmosphere.

            user
            what is the capital of France?
            assistant
            The capital of France is Paris.
            user
            who is the president of the united states?
            assistant
            The current president of the United States is Joe Biden.
            """

          true ->
            "LLM verbalized the symbolic posture."
        end

      {:reply, {:ok, %{content: text}}, test_pid}
    end
  end

  setup do
    old_client = Application.get_env(:core, :llm_client)
    Application.put_env(:core, :llm_client, FakeLlm)
    start_supervised!({FakeLlm, self()})

    on_exit(fn ->
      case old_client do
        nil -> Application.delete_env(:core, :llm_client)
        client -> Application.put_env(:core, :llm_client, client)
      end
    end)

    :ok
  end

  test "normal helpful responses use LLM synthesis when a client is available" do
    si = %{
      intent: :refactor,
      confidence: 0.92,
      text: "help me refactor the response pipeline"
    }

    mood = %{mood: %{vigilance: 0.3, inhibition: 0.5, exploration: 0.6, plasticity: 0.5}}

    {_tone, text, meta} = Response.plan(si, mood)

    assert text == "LLM verbalized the symbolic posture."
    assert meta.mode == :collaborator
    assert meta.action == :act_first
    assert meta.response_source == :llm
    refute meta.response_fallback_reason

    assert_receive {:llm_chat, messages, opts}
    assert Keyword.get(opts, :timeout)

    system =
      messages
      |> Enum.find(fn msg -> msg["role"] == "system" end)
      |> Map.fetch!("content")

    assert system =~ "You are Symbrella."
  end

  test "short greetings route through LLM instead of inline social text" do
    si = %{intent: :unknown, confidence: 0.8, text: "hello there"}

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "LLM verbalized the symbolic posture."
    assert meta.intent_inferred == :greeting
    assert meta.response_source == :llm
    refute meta.response_fallback_reason
    refute text =~ "Quick picks"
    refute text =~ "Full file"
    assert_receive {:llm_chat, _messages, _opts}
  end

  test "casual self-state check-ins use the anti-label self-state prompt" do
    si = %{
      intent: :greet,
      confidence: 0.49,
      text: "hey symbrella how are you on this fine saturday morning"
    }

    mood = %{
      mood: %{exploration: 0.48, inhibition: 0.54, vigilance: 0.77, plasticity: 0.56},
      pressure_label: :steady_restraint
    }

    {_tone, text, meta} = Response.plan(si, mood)

    assert text =~ "Good morning."
    assert text =~ "I'm running steady right now"
    refute text =~ "neutral state"
    refute text =~ "steady pressure"
    refute text =~ "balanced and unbiased"
    refute text =~ "ready to assist"
    refute text =~ "How may I help you today"
    assert meta.intent_inferred == :smalltalk
    assert meta.mode == :explainer
    assert meta.chosen_skill == :self_state_feeling
    assert :self_state_feeling_answer in meta.overrides
    assert meta.response_source == :llm

    assert_receive {:llm_chat, messages, _opts}

    system =
      messages
      |> Enum.find(fn msg -> msg["role"] == "system" end)
      |> Map.fetch!("content")

    assert system =~ "This is a casual check-in."
    assert system =~ "Use internal state labels only as private shaping context"
    assert system =~ "Do not describe yourself as unbiased, neutral, or ready to assist"
    assert system =~ "Internal pressure label (do not quote): steady_restraint"
    refute system =~ "Current state:"
    refute system =~ "Pressure label:"
  end

  test "LLM trailing assistant explanation notes are stripped from chat text" do
    si = %{intent: :refactor, confidence: 0.92, text: "note leak in a refactor answer"}

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "Here is the direct answer."
    assert meta.response_source == :llm
    refute text =~ "Note:"
    refute text =~ "assistant's response is based"

    assert_receive {:llm_chat, _messages, _opts}
  end

  test "LLM generated transcript continuation is stripped from chat text and history" do
    session_id = "continuation-leak-#{System.unique_integer([:positive])}"

    si = %{
      intent: :question,
      confidence: 0.92,
      text: "why is the sky blue? continuation leak",
      session_id: session_id
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert text ==
             "The sky is blue because shorter blue wavelengths scatter more strongly in the atmosphere."

    assert meta.response_source == :llm
    refute text =~ "\nuser\n"
    refute text =~ "\nassistant\n"
    refute text =~ "capital of France"
    refute text =~ "Joe Biden"

    assert_receive {:llm_chat, _messages, _opts}

    assert [
             %{"role" => "user", "content" => "why is the sky blue? continuation leak"},
             %{
               "role" => "assistant",
               "content" =>
                 "The sky is blue because shorter blue wavelengths scatter more strongly in the atmosphere."
             }
           ] = Core.Response.LlmChatHistory.messages(session_id)
  end

  test "LLM generated Symbrella speaker labels are stripped" do
    si = %{intent: :refactor, confidence: 0.92, text: "symbrella label leak"}

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "Here is the answer."
    assert meta.response_source == :llm
    refute text =~ "Symbrella:"

    assert_receive {:llm_chat, _messages, _opts}
  end

  test "LLM generated Symbrella transcript continuation is stripped" do
    si = %{intent: :refactor, confidence: 0.92, text: "symbrella continuation leak"}

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "Here is the answer."
    assert meta.response_source == :llm
    refute text =~ "Symbrella:"
    refute text =~ "What is your name?"

    assert_receive {:llm_chat, _messages, _opts}
  end

  test "Core.Response.plan owns LLM history recording once per turn" do
    session_id = "single-owner-history-#{System.unique_integer([:positive])}"

    first = %{
      intent: :refactor,
      confidence: 0.92,
      text: "first history turn",
      session_id: session_id
    }

    second = %{
      intent: :refactor,
      confidence: 0.92,
      text: "second history turn",
      session_id: session_id
    }

    {_tone, "LLM verbalized the symbolic posture.", _meta} = Response.plan(first, %{})
    assert_receive {:llm_chat, _first_messages, _opts}

    {_tone, "LLM verbalized the symbolic posture.", _meta} = Response.plan(second, %{})
    assert_receive {:llm_chat, second_messages, _opts}

    history_messages =
      second_messages
      |> Enum.reject(&(&1["role"] == "system"))
      |> Enum.drop(-1)

    assert history_messages == [
             %{"role" => "user", "content" => "first history turn"},
             %{"role" => "assistant", "content" => "LLM verbalized the symbolic posture."}
           ]
  end

  test "LLM synthesis applies configured timeout, token, and history budgets" do
    old_config = Application.get_env(:core, :llm_synthesis, [])

    Application.put_env(
      :core,
      :llm_synthesis,
      Keyword.merge(old_config,
        timeout_ms: 1_234,
        ready_timeout_ms: 333,
        max_tokens: 44,
        history_turn_pairs: 1,
        max_item_chars: 12
      )
    )

    on_exit(fn -> Application.put_env(:core, :llm_synthesis, old_config) end)

    session_id = "resource-budget-#{System.unique_integer([:positive])}"

    Core.Response.LlmChatHistory.record_turn(
      session_id,
      "first history turn should be dropped",
      "first response should be dropped"
    )

    Core.Response.LlmChatHistory.record_turn(
      session_id,
      "recent",
      "assistant answer is intentionally long"
    )

    si = %{
      intent: :question,
      confidence: 0.92,
      text: "configured budget prompt",
      session_id: session_id
    }

    {_tone, "LLM verbalized the symbolic posture.", meta} = Response.plan(si, %{})

    assert meta.response_source == :llm
    assert_receive {:llm_chat, messages, opts}
    assert Keyword.get(opts, :timeout) == 1_234
    assert Keyword.get(opts, :call_timeout) == 2_234
    assert Keyword.get(opts, :ready_timeout) == 333
    assert Keyword.get(opts, :max_tokens) == 44

    history_messages =
      messages
      |> Enum.reject(&(&1["role"] == "system"))
      |> Enum.drop(-1)

    assert history_messages == [
             %{"role" => "user", "content" => "recent"},
             %{"role" => "assistant", "content" => "assistant an…"}
           ]
  end

  test "fuzzy LLM drafts are reflected into bounded clarification" do
    handler_id = "llm-route-reflection-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      Core.Telemetry.attach(
        handler_id,
        [:core, :response, :complete],
        fn event, measurements, metadata, _ ->
          send(parent, {:complete, event, measurements, metadata})
        end,
        nil
      )

    si = %{intent: :unknown, confidence: 0.1, text: "this fuzzy thing feels off"}

    {_tone, text, _meta} = Response.plan(si, %{})

    assert text =~ "What part should I focus on first?"

    assert_receive {:llm_chat, _messages, _opts}
    assert_receive {:complete, [:core, :response, :complete], _measurements, complete_meta}, 200

    assert complete_meta.reflection.status == :clarify
    assert :too_generic in complete_meta.reflection.issues
    assert complete_meta.reflection.applied? == true

    Core.Telemetry.detach(handler_id)
  end
end
