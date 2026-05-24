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

      text =
        if messages |> List.last() |> Map.get("content", "") |> String.contains?("fuzzy") do
          "Please provide more information so I can assist you today."
        else
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

  test "greetings use LLM synthesis instead of a canned menu when a client is available" do
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
