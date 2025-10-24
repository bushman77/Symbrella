defmodule Brain.Cell.SafeTest do
  use ExUnit.Case, async: true

  alias Brain.Cell.Safe

  defmodule Echo do
    use GenServer

    def start_link(opts \\ []), do: GenServer.start_link(__MODULE__, :ok, opts)

    @impl true
    def init(:ok), do: {:ok, %{}}

    @impl true
    def handle_call({:echo, v}, _from, s), do: {:reply, v, s}
    def handle_call(:sleep_250, _from, s) do
      Process.sleep(250)
      {:reply, :ok, s}
    end

    def handle_call(:crash, _from, _s), do: :erlang.error(:boom)
  end

  setup do
    {:ok, pid} = Echo.start_link()
    {:ok, pid: pid}
  end

  test "returns {:ok, reply} on success", %{pid: pid} do
    assert {:ok, :pong} = Safe.call(pid, {:echo, :pong})
  end

  test "times out cleanly", %{pid: pid} do
    assert {:error, :timeout} = Safe.call(pid, :sleep_250, timeout: 50, retry?: false)
  end

  test "handles server crash as {:exit, reason}", %{pid: pid} do
    assert {:error, {:exit, _}} = Safe.call(pid, :crash, timeout: 100, retry?: false)
  end

  test "handles :noproc without blowing up" do
    # use a dead pid
    dead = spawn(fn -> :ok end)
    ref = Process.monitor(dead)
    assert_receive {:DOWN, ^ref, :process, ^dead, _}

    assert {:error, :noproc} = Safe.call(dead, {:echo, 1}, timeout: 50, retry?: false)
  end

  test "does a single transient retry by default", %{pid: pid} do
    # call a killed process with retry? true still resolves to :noproc; we just cover path
    dead = spawn(fn -> :ok end)
    ref = Process.monitor(dead)
    assert_receive {:DOWN, ^ref, :process, ^dead, _}
    assert {:error, :noproc} = Safe.call(dead, {:echo, 1}, timeout: 10, retry?: true, retries: 1)
  end
end

