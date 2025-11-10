defmodule Brain.LIFG.ACCGate do
  @moduledoc """
  Optional ACC gate for LIFG.

  Exposes `assess/3`, returning `{si_with_trace, conflict :: float, acc_present? :: boolean}`.

  If `Brain.ACC` is present, we call one of:
    * `Brain.ACC.assess(si, choices, opts)`
    * `Brain.ACC.assess(si, choices)`
    * `Brain.ACC.score(si, choices)`

  and append a trace event `:acc` with the conflict score. When ACC is missing
  or errors, we fail open (`{si, 1.0, false}`).
  """

  @spec assess(map(), list(), keyword()) :: {map(), float(), boolean()}
  def assess(si, choices, opts) when is_map(si) and is_list(choices) do
    acc_present? =
      Code.ensure_loaded?(Brain.ACC) and
        (function_exported?(Brain.ACC, :assess, 3) or
           function_exported?(Brain.ACC, :assess, 2) or
           function_exported?(Brain.ACC, :score, 2))

    if acc_present? do
      now_ms = System.system_time(:millisecond)

      result =
        cond do
          function_exported?(Brain.ACC, :assess, 3) -> apply(Brain.ACC, :assess, [si, choices, opts])
          function_exported?(Brain.ACC, :assess, 2) -> apply(Brain.ACC, :assess, [si, choices])
          function_exported?(Brain.ACC, :score, 2)  -> apply(Brain.ACC, :score,  [si, choices])
          true -> :skip
        end

      case result do
        {:ok, %{si: si2, conflict: c} = payload} when is_number(c) ->
          ev = payload |> Map.drop([:si]) |> Map.merge(%{stage: :acc, ts_ms: now_ms})
          si3 = Map.update(si2, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si3, clamp01(c), true}

        {:ok, c} when is_number(c) ->
          ev = %{stage: :acc, conflict: c, ts_ms: now_ms}
          si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si2, clamp01(c), true}

        c when is_number(c) ->
          ev = %{stage: :acc, conflict: c, ts_ms: now_ms}
          si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
          safe_exec_telemetry([:brain, :acc, :assess], %{conflict: c}, %{})
          {si2, clamp01(c), true}

        _other ->
          # ACC present but returned something unexpected; fail open (no gate)
          {si, 1.0, true}
      end
    else
      {si, 1.0, false}
    end
  rescue
    _ ->
      # On any ACC error, fail open and don't block the pipeline.
      {si, 1.0, false}
  end

  # ── helpers ─────────────────────────────────────────────

  defp clamp01(x) when is_number(x) do
    x = x * 1.0
    cond do
      x < 0.0 -> 0.0
      x > 1.0 -> 1.0
      true -> x
    end
  end

  defp clamp01(_), do: 0.0

  defp safe_exec_telemetry(event, measurements, meta) do
    if Code.ensure_loaded?(:telemetry) and function_exported?(:telemetry, :execute, 3) do
      :telemetry.execute(event, measurements, meta)
    else
      :ok
    end
  end
end

