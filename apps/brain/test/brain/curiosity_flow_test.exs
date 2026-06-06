# apps/brain/test/brain/curiosity_flow_test.exs
defmodule Brain.CuriosityFlowTest do
  use ExUnit.Case, async: false

  @moduletag :curiosity_flow

  setup_all do
    # These singleton regions are owned by the umbrella-root supervisor.
    # Tests should assert their presence, not attempt to start local copies.
    require_running!(Brain)
    require_running!(Brain.Curiosity)
    require_running!(Brain.Thalamus)
    require_running!(Brain.OFC)
    require_running!(Brain.DLPFC)

    :ok
  end

  setup do
    :ok = Brain.Thalamus.reset()
    :ok = Brain.OFC.reset()
    :ok = Brain.DLPFC.reset()
    :ok = Brain.defocus(fn _ -> true end)

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
             3_000
           )

    # Optional: double-check after the wait for better failure messages
    %{wm: wm_final} = Brain.snapshot_wm()
    assert has_curiosity?(wm_final) or initial_has_curiosity?
  end

  # ───────────── helpers ─────────────

  defp has_curiosity?(wm) do
    Enum.any?(wm, fn item ->
      cond do
        # 1) Direct curiosity tag in payload
        match?(%{payload: %{reason: :curiosity}}, item) ->
          true

        # 2) Direct curiosity tag at top level
        match?(%{reason: :curiosity}, item) ->
          true

        # 3) Fallback: look for "probe|" id in payload or top-level
        true ->
          id =
            case item do
              %{payload: %{id: id}} when is_binary(id) -> id
              %{id: id} when is_binary(id) -> id
              _ -> nil
            end

          is_binary(id) and String.starts_with?(id, "probe|")
      end
    end)
  end

  defp require_running!(mod) when is_atom(mod) do
    case Process.whereis(mod) do
      pid when is_pid(pid) ->
        :ok

      nil ->
        flunk("""
        #{inspect(mod)} is not running.

        This test assumes singleton Brain regions are started under Symbrella.Application.
        Do not start ad hoc local copies from the test.
        """)
    end
  end

  defp wait_until(fun, timeout_ms, step_ms \\ 25) when is_function(fun, 0) do
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
