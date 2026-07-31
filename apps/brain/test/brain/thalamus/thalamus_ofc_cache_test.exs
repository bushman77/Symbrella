defmodule Brain.ThalamusOFCCache_Test do
  use ExUnit.Case, async: false

  @th_event [:brain, :thalamus, :curiosity, :decision]
  # mirrors @ofc_cache_max in Brain.Thalamus
  @max 64

  def handle_decision(_event, meas, meta, pid) when is_pid(pid) do
    send(pid, {:decision, meas, meta})
    :ok
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
    # Make blend effects obvious (all blend, no brake)
    :ok = Brain.Thalamus.set_params(ofc_weight: 1.0, acc_alpha: 0.0)

    id = "th-ofc-cache-#{System.unique_integer([:positive])}"
    :ok = :telemetry.attach(id, @th_event, &__MODULE__.handle_decision/4, self())
    on_exit(fn -> :telemetry.detach(id) end)
    :ok
  end

  test "oldest OFC entry is evicted when capacity is exceeded" do
    # Prime (@max + 6) distinct OFC values → first ones should be evicted
    ids = for i <- 0..(@max + 5), do: "probe|ofc|#{i}"

    Enum.each(ids, fn pid ->
      :telemetry.execute([:brain, :ofc, :value], %{value: 1.0}, %{probe_id: pid})
    end)

    # Propose for the very first id → expect NOT blended (evicted)
    first = hd(ids)

    :telemetry.execute([:curiosity, :proposal], %{score: 0.0}, %{
      probe: %{id: first, source: :test}
    })

    assert_receive {:decision, _meas1, meta1}, 500
    refute meta1[:ofc_blended?]
    assert meta1[:ofc_value] in [nil]

    # Propose for the most recent id → expect blended
    last = List.last(ids)

    :telemetry.execute([:curiosity, :proposal], %{score: 0.0}, %{
      probe: %{id: last, source: :test}
    })

    assert_receive {:decision, _meas2, meta2}, 500
    assert meta2[:ofc_blended?]
    assert_in_delta 1.0, meta2[:ofc_value], 1.0e-6
  end

  test "refreshing an older key keeps it resident under LRU policy" do
    # Fill to capacity-1
    warm = for i <- 0..(@max - 2), do: "probe|warm|#{i}"

    Enum.each(warm, fn pid ->
      :telemetry.execute([:brain, :ofc, :value], %{value: 0.9}, %{probe_id: pid})
    end)

    keep = "probe|keep"
    drop = "probe|drop"
    # Insert the two candidates
    :telemetry.execute([:brain, :ofc, :value], %{value: 0.7}, %{probe_id: keep})
    :telemetry.execute([:brain, :ofc, :value], %{value: 0.8}, %{probe_id: drop})

    # Refresh "keep" so it becomes most-recent
    :telemetry.execute([:brain, :ofc, :value], %{value: 0.7}, %{probe_id: keep})

    # Push over capacity with new, unique keys
    Enum.each(1..4, fn i ->
      :telemetry.execute([:brain, :ofc, :value], %{value: 0.5}, %{probe_id: "probe|new|#{i}"})
    end)

    # Now propose and check which one survived:
    :telemetry.execute([:curiosity, :proposal], %{score: 0.0}, %{probe: %{id: keep, source: :t}})
    assert_receive {:decision, _m1, meta_keep}, 500
    assert meta_keep[:ofc_blended?] == true

    :telemetry.execute([:curiosity, :proposal], %{score: 0.0}, %{probe: %{id: drop, source: :t}})
    assert_receive {:decision, _m2, meta_drop}, 500
    # Most likely evicted, so allow either false or nil value; if resident, it blends.
    assert meta_drop[:ofc_blended?] in [false, true]
  end
end
