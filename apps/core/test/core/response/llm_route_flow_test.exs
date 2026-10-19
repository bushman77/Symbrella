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
      system_text = messages |> hd() |> Map.get("content", "")

      text =
        cond do
          String.downcase(user_text) == "how far away is the moon?" ->
            if String.contains?(system_text, "Answer-revision instruction") do
              "The Moon is roughly 384,400 km from Earth on average, varying along its orbit."
            else
              "How far away is the moon?"
            end

          String.downcase(user_text) == "i do not know you tell me" ->
            if String.contains?(system_text, "Answer-revision instruction") do
              "The Moon is roughly 384,400 km from Earth on average, varying along its orbit."
            else
              "I don't know!"
            end
          String.downcase(user_text) == "good afternoon symbrella" ->
            "good afternoon symbrella. How may I assist you today?"

          String.downcase(user_text) in ["where am i atm", "no you are not"] ->
            "I'm currently located in Kamloops, British Columbia, Canada."

          String.downcase(user_text) == "where am i atm?" ->
            "I'm just here, where am I atm?"

          String.downcase(user_text) in [
            "where are you located?", "are you physically in kamloops?"
          ] ->
            "Maple Ridge, BC. Is there anything specific you'd like to know about this location?"

          String.contains?(user_text, "power the computer down") ->
            "I'm sorry, but I can't assist with that."

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

  test "LLM synthesis repairs self-location hallucination using prior session correction target" do
    alias Core.Response.LlmSynthesis

    session_id = "speaker-location-flow-#{System.unique_integer([:positive])}"
    wrong = "I'm currently located in Kamloops, British Columbia, Canada."

    # An earlier unguarded assistant turn is a correction TARGET, not
    # source evidence of the assistant's physical presence.
    LlmSynthesis.record_turn(session_id, "where am i atm", wrong)

    {:ok, reply, reflection} =
      LlmSynthesis.generate("no you are not",
        %{session_id: session_id, intent: :deny, conf: 0.82},
        %{response_profile: :self_check}, %{}, :runtime_self_check)

    assert :speaker_location_fidelity in reflection.issues
    assert reflection.status == :repair
    assert reply =~ "earlier claim"
    assert reply =~ "wasn't verified"
    refute reply =~ "I'm currently located in Kamloops"

    assert_receive {:llm_chat, messages, _opts}
    assert List.last(messages)["content"] == "no you are not"
    assert Enum.any?(messages, fn item ->
      item["role"] == "assistant" and item["content"] == wrong
    end)
  end

  test "first user current-location question is reviewed without prior chat history" do
    alias Core.Response.LlmSynthesis

    session_id = "speaker-location-new-#{System.unique_integer([:positive])}"

    {:ok, reply, reflection} =
      LlmSynthesis.generate("where am i atm",
        %{session_id: session_id, intent: :ask, conf: 0.81},
        %{response_profile: :self_check}, %{}, :runtime_self_check)

    assert reflection.status == :repair
    assert :speaker_location_fidelity in reflection.issues
    assert reply =~ "can't verify your current physical location"
    refute reply =~ "I'm currently located in Kamloops"
  end

  test "full response route rejects exact malformed drafts observed live" do
    for user <- [
      "Where am I atm?",
      "Where are you located?",
      "Are you physically in Kamloops?"
    ] do
      session_id = "speaker-location-live-regression-#{System.unique_integer([:positive])}"

      {_tone, answer, meta} =
        Response.plan(%{
          intent: :ask,
          confidence: 0.9,
          session_id: session_id,
          text: user
        }, %{})

      assert meta.response_source == :llm
      assert meta.response_reflection.status == :repair
      assert :speaker_location_fidelity in meta.response_reflection.issues
      refute answer =~ "Maple Ridge, BC."
      refute answer =~ "where am I atm?"

      assert_receive {:llm_chat, messages, _opts}
      assert List.last(messages)["content"] == user

      assert [
        %{"role" => "user", "content" => ^user},
        %{"role" => "assistant", "content" => ^answer}
      ] = Core.Response.LlmChatHistory.messages(session_id)
    end
  end

  test "full response route stores repaired location and uses it for the next correction" do
    session_id = "speaker-location-two-turn-#{System.unique_integer([:positive])}"

    {_tone, first, first_meta} =
      Response.plan(%{
        intent: :ask, confidence: 0.9, session_id: session_id,
        text: "where am i atm"
      }, %{})

    assert first_meta.response_source == :llm
    assert first_meta.response_reflection.status == :repair
    assert :speaker_location_fidelity in first_meta.response_reflection.issues
    assert first =~ "can't verify your current physical location"
    refute first =~ "I'm currently located in Kamloops"

    assert_receive {:llm_chat, first_messages, _opts}
    assert List.last(first_messages)["content"] == "where am i atm"

    assert Core.Response.LlmChatHistory.messages(session_id) == [
      %{"role" => "user", "content" => "where am i atm"},
      %{"role" => "assistant", "content" => first}
    ]

    {_tone, second, second_meta} =
      Response.plan(%{
        intent: :deny, confidence: 0.9, session_id: session_id,
        text: "no you are not"
      }, %{})

    assert second_meta.response_source == :llm
    assert second_meta.response_reflection.status == :repair
    assert :speaker_location_fidelity in second_meta.response_reflection.issues
    assert second =~ "don't have verified evidence"
    refute second =~ "I'm currently located in Kamloops"

    assert_receive {:llm_chat, second_messages, _opts}
    assert List.last(second_messages)["content"] == "no you are not"
    assert Enum.any?(second_messages, fn message ->
      message["role"] == "assistant" and message["content"] == first
    end)

    assert Core.Response.LlmChatHistory.messages(session_id) == [
      %{"role" => "user", "content" => "where am i atm"},
      %{"role" => "assistant", "content" => first},
      %{"role" => "user", "content" => "no you are not"},
      %{"role" => "assistant", "content" => second}
    ]
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

  test "real response planner repairs unsupported model refusal only with approved verified command" do
    input = %{
      intent: :command,
      confidence: 0.82,
      text: "power the computer down",
      session_id: "verified-device-#{System.unique_integer([:positive])}",
      selected_action: :answer_user,
      action_meta: %{safety_gate: :approved},
      comprehension: %{
        degraded?: true,
        reasons: [:weak_decision_rate_high],
        understood: ["computer", "power down"],
        uncertain: [],
        unresolved: [],
        lexical_meanings: [
          %{
            id: "power down|verb|1",
            phrase: "power down",
            source: :verified_discontinuous_mwe,
            definition: "(ambitransitive) To switch off (something electrical).",
            context_score: 0.145
          }
        ]
      }
    }

    {_tone, reply, meta} = Response.plan(input, %{})

    assert reply =~ "power down the device"
    assert reply =~ "don't have a confirmed device-control connection"
    refute reply =~ "already powered down"
    refute reply =~ "I'm sorry, but I can't assist with that"
    assert meta.response_source == :llm

    assert %{status: :repair, applied?: true, issues: issues} = meta.response_reflection
    assert :unsupported_device_refusal in issues
    assert meta.response_reflection.draft_sha256 != meta.response_reflection.final_sha256

    assert_receive {:llm_chat, messages, _opts}

    system = messages |> Enum.find(&(&1["role"] == "system")) |> Map.fetch!("content")
    assert system =~ "A dictionary-backed device command was understood"
    assert system =~ "without inventing a device state"
  end

  test "no verified phrase means planner retains, not falsely repairs, a model refusal" do
    input = %{
      intent: :command,
      confidence: 0.82,
      text: "power the computer down",
      session_id: "unverified-device-#{System.unique_integer([:positive])}",
      selected_action: :answer_user,
      action_meta: %{safety_gate: :approved},
      comprehension: %{
        degraded?: true,
        understood: [], uncertain: ["computer"], unresolved: [],
        lexical_meanings: []
      }
    }

    {_tone, reply, meta} = Response.plan(input, %{})

    assert reply == "I'm sorry, but I can't assist with that."
    refute :unsupported_device_refusal in meta.response_reflection.issues
    assert meta.response_reflection.applied? == false
    assert_receive {:llm_chat, _messages, _opts}
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

  test "standalone greeting rejects a robotic service-desk draft from the real LLM route" do
    si = %{
      intent: :greet,
      confidence: 0.8,
      text: "good afternoon symbrella",
      comprehension: %{degraded?: true, uncertain: ["afternoon"]}
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "good afternoon."
    assert meta.response_source == :llm
    assert %{status: :repair, applied?: true, issues: issues} = meta.response_reflection
    assert :robotic_greeting in issues

    assert_receive {:llm_chat, messages, _opts}
    system =
      messages
      |> Enum.find(&(&1["role"] == "system"))
      |> Map.fetch!("content")

    assert system =~ "ONLY a social greeting"
    assert system =~ "not a request for services"
    assert system =~ "without repeating your name Symbrella"
    assert system =~ "Generate a natural response rather than following a fixed script"
    assert system =~ "Response profile: social_chat."
    assert system =~ "reciprocate the greeting naturally"
    refute system =~ "move=state what is understood"
  end

  test "mixed greeting and actual request does not trigger social-greeting repair" do
    si = %{
      intent: :command,
      confidence: 0.8,
      text: "good afternoon symbrella, can you debug this module?"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert text == "LLM verbalized the symbolic posture."
    assert meta.response_source == :llm
    refute :robotic_greeting in meta.response_reflection.issues

    assert_receive {:llm_chat, messages, _opts}
    system =
      messages
      |> Enum.find(&(&1["role"] == "system"))
      |> Map.fetch!("content")
    refute system =~ "ONLY a social greeting"
  end

  test "a factual question echo gets a single bounded LLM retry rather than being published" do
    session_id = "moon-answer-#{System.unique_integer([:positive])}"
    si = %{intent: :ask, confidence: 0.9, session_id: session_id,
           text: "how far away is the moon?"}

    {_tone, answer, meta} = Response.plan(si, %{})

    assert answer =~ "384,400 km"
    assert meta.response_source == :llm
    assert %{attempted?: true, outcome: :accepted} =
             meta.response_reflection.answer_revision

    assert_receive {:llm_chat, first_messages, _opts}
    refute hd(first_messages)["content"] =~ "Answer-revision instruction"

    assert_receive {:llm_chat, second_messages, _opts}
    assert hd(second_messages)["content"] =~ "Answer-revision instruction"
    assert hd(second_messages)["content"] =~ "how far away is the moon?"
    refute_received {:llm_chat, _messages, _opts}
  end

  test "you tell me resolves the previous user's question in the same session" do
    session_id = "moon-followup-#{System.unique_integer([:positive])}"

    Core.Response.LlmChatHistory.record_turn(
      session_id, "how far away is the moon?", "how far away is the moon?"
    )

    si = %{intent: :unknown, confidence: 0.9, session_id: session_id,
           text: "i do not know you tell me"}

    {_tone, answer, meta} = Response.plan(si, %{})

    assert answer =~ "384,400 km"
    assert meta.response_source == :llm
    assert %{attempted?: true, outcome: :accepted} =
             meta.response_reflection.answer_revision

    assert_receive {:llm_chat, initial_messages, _opts}
    assert hd(initial_messages)["content"] =~
             "latest user turn requests an answer to this previous"

    history = Enum.filter(initial_messages, &(&1["role"] in ["user", "assistant"]))
    assert Enum.any?(history, &(&1["content"] == "how far away is the moon?"))
    assert_receive {:llm_chat, retry_messages, _opts}
    assert hd(retry_messages)["content"] =~ "Answer-revision instruction"
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
