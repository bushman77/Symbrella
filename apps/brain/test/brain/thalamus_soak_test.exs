defmodule Brain.ThalamusSoak_Test do
  use ExUnit.Case, async: false

  @th_event [:brain, :thalamus, :curiosity, :decision]

  def handle_decision(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:decision, meas, meta})
    :ok
  end

# test/brain/thalamus_soak_test.exs
defp recv(acc) do
  receive do
    {:decision, _meas, meta} -> recv([meta[:probe][:id] | acc])
  after
    50 -> Enum.reverse(acc)
  end
end


  setup_all do
    case Process.whereis(Brain) do
      nil -> start_supervised!(Brain)
      _pid -> :ok
    end

    case Process.whereis(Brain.Thalamus) do
      nil ->
        flunk("""
        Brain.Thalamus is not running.

        These tests assume the singleton is already started under the umbrella root.
        """)

      _pid ->
        :ok
    end

    :ok
  end

  setup do
    :ok = Brain.Thalamus.set_params(ofc_weight: 0.0, acc_alpha: 0.0)

    id = "th-soak-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, @th_event, &__MODULE__.handle_decision/4, self())
    on_exit(fn -> :telemetry.detach(id) end)
    :ok
  end

  test "burst of proposals yields a matching number of decision events" do
    n = 200

    for i <- 1..n do
      pid = "probe|soak|#{i}"

      :telemetry.execute([:curiosity, :proposal], %{score: rem(i, 10) / 10}, %{
        probe: %{id: pid, source: :soak}
      })
    end

    # Collect all decisions (allow a little time)
    recv = fn acc ->
      receive do
        {:decision, _meas, meta} -> recv([meta[:probe][:id] | acc])
      after
        500 -> acc
      end
    end

    ids = recv.([]) |> Enum.reject(&is_nil/1)
    uniq = MapSet.new(ids)
    assert length(ids) == n
    assert MapSet.size(uniq) == n
  end
end
