defmodule Llm.BootGateTest do
  use ExUnit.Case, async: true

  test "disabled boot gate starts independently of the local model" do
    assert {:ok, pid} = Llm.BootGate.start_link(name: nil, enabled?: false)
    assert %{enabled?: false, status: :disabled} = :sys.get_state(pid)
    GenServer.stop(pid)
  end

  test "invalid enabled boot mode fails without starting the model" do
    assert {:error, {:invalid_llm_boot_mode, :invalid}} =
             Llm.BootGate.start_link(name: nil, enabled?: true, mode: :invalid)
  end
end
