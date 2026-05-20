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
      {:reply, {:ok, %{content: "LLM verbalized the symbolic posture."}}, test_pid}
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
    refute text =~ "Quick picks"
    refute text =~ "Full file"

    assert_receive {:llm_chat, _messages, _opts}
  end
end
