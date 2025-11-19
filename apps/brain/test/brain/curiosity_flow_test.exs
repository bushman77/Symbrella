# apps/brain/test/brain/curiosity_flow_test.exs
defmodule Brain.CuriosityFlowTest do
  use ExUnit.Case, async: false

  @moduletag :curiosity_flow

  setup_all do
    # Ensure core regions/workers are running under their registered names.
    ensure_started(Brain)
    ensure_started(Brain.Curiosity)
    ensure_started(Brain.Thalamus)
    ensure_started(Brain.OFC)
    ensure_started(Brain.DLPFC)

    # Make sure DLPFC actually acts on Thalamus decisions for this flow test.
    :ok = Brain.DLPFC.set_opts(act_on_thalamus: true)

    :ok
  end

  test "Curiosity → Thalamus(+OFC) → BG → DLPFC inserts a curiosity-tagged probe into WM" do
    # Sanity: snapshot before nudge
    %{wm: wm0} = Brain.snapshot_wm()
    # There *should* be no curiosity-tagged items yet in a fresh run, but we don't
    # hard-assert it to keep the test robust if we reuse Curiosity elsewhere.
    initial_has_curiosity? = has_curiosity?(wm0)

    # Nudge Curiosity to fire
    :ok = Brain.Curiosity.nudge()

    # Wait until WM contains a curiosity-tagged item
    assert wait_until(
             fn ->
               %{wm: wm1} = Brain.snapshot_wm()
               has_curiosity?(wm1)
             end,
             1_000
           )

    # Optional: double-check after the wait for better failure messages
    %{wm: wm_final} = Brain.snapshot_wm()
    assert has_curiosity?(wm_final) or initial_has_curiosity?
  end

  # ───────────── helpers ─────────────

  defp has_curiosity?(wm) when is_list(wm) do
    Enum.any?(wm, fn item ->
      src = item[:source] || item["source"]
      payload = item[:payload] || %{}
      reason = payload[:reason] || payload["reason"]

      src in [:runtime, "runtime", :curiosity, "curiosity"] and
        reason in [:curiosity, "curiosity"]
    end)
  end

  defp has_curiosity?(_), do: false

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

