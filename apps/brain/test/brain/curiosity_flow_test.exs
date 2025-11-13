# apps/brain/test/brain/curiosity_flow_test.exs
defmodule Brain.CuriosityFlowTest do
  use ExUnit.Case, async: false

  @moduletag :curiosity_flow

  setup_all do
    # Ensure core regions/workers are running under their registered names.
    ensure_started(Brain)
    ensure_started(Curiosity)
    ensure_started(Brain.Thalamus)
    ensure_started(Brain.OFC)
    ensure_started({Brain.DLPFC, act_on_thalamus: true})
    :ok
  end

  test "Curiosity → Thalamus(+OFC) → BG → DLPFC inserts a probe into WM" do
    # Snapshot initial WM size
    %{wm: wm0} = Brain.snapshot_wm()
    n0 = length(wm0)

    # Nudge Curiosity to fire immediately
    :ok = Curiosity.nudge()

    # Wait until WM grows
    assert wait_until(
             fn ->
               %{wm: wm1} = Brain.snapshot_wm()
               length(wm1) > n0
             end,
             1_000
           )

    %{wm: wm2} = Brain.snapshot_wm()
    [head | _] = wm2

    # Assert the newest WM item looks like a curiosity probe
    assert is_map(head)
    # score should be within [0,1]
    assert is_number(head[:score]) and head[:score] >= 0.0 and head[:score] <= 1.0
    # preferred source (our pipeline sets/normalizes to :runtime)
    assert head[:source] in [:runtime, "runtime", :curiosity, "curiosity"]

    # reason is tracked in the payload for traceability
    reason =
      head
      |> Map.get(:payload, %{})
      |> then(&(&1[:reason] || &1["reason"]))

    assert reason in [:curiosity, "curiosity"]
  end

  # ───────────── helpers ─────────────

  defp ensure_started({mod, opts}) when is_atom(mod) and is_list(opts) do
    case Process.whereis(mod) do
      nil -> start_supervised!({mod, opts})
      _pid -> :ok
    end
  end

  defp ensure_started(mod) when is_atom(mod) do
    case Process.whereis(mod) do
      nil -> start_supervised!(mod)
      _pid -> :ok
    end
  end

  defp wait_until(fun, timeout_ms \\ 1_000, step_ms \\ 20) when is_function(fun, 0) do
    t0 = System.monotonic_time(:millisecond)
    do_wait_until(fun, t0, timeout_ms, step_ms)
  end

  defp do_wait_until(fun, t0, timeout_ms, step_ms) do
    case safe_true?(fun) do
      true ->
        true

      false ->
        if System.monotonic_time(:millisecond) - t0 > timeout_ms do
          false
        else
          Process.sleep(step_ms)
          do_wait_until(fun, t0, timeout_ms, step_ms)
        end
    end
  end

  defp safe_true?(fun) do
    try do
      fun.() == true
    rescue
      _ -> false
    catch
      _, _ -> false
    end
  end
end
