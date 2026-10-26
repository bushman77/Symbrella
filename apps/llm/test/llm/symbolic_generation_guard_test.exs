defmodule Llm.SymbolicGenerationGuardTest do
  use ExUnit.Case, async: false

  setup do
    previous = Application.get_env(:llm, :inference_enabled?, true)
    Application.put_env(:llm, :inference_enabled?, false)

    on_exit(fn -> Application.put_env(:llm, :inference_enabled?, previous) end)
    :ok
  end

  test "disabled inference cannot start llama or reach chat and embeddings" do
    name = :"symbolic_llm_guard_#{System.unique_integer([:positive])}"

    {:ok, pid} =
      Llm.start_link(
        name: name,
        auto_start_on_boot?: false,
        allow_lazy_start?: true,
        auto_restart_on_crash?: false,
        heartbeat_ms: 0
      )

    on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

    assert {:error, :llm_disabled} = Llm.start_llama(name: name)
    assert {:error, :llm_disabled} = Llm.chat("hello", name: name)
    assert {:error, :llm_disabled} = Llm.embeddings("hello", name: name)
    assert {:error, :llm_disabled} = Llm.models(name: name)

    assert {:ok, %{status: :stopped, runner_os_pid: nil, reachable?: false}} =
             Llm.status(name: name)
  end
end
