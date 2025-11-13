defmodule Brain.DLPFC.Policy do
  @moduledoc """
  Pure policy selector for DLPFC (separate from your DLPFC GenServer region).

  Input `ctx` (all optional):
    :acc_conflict, :wm_diversity, :novelty ∈ 0.0..1.0
    :top_k_base (default 3), :max_retries_base (default 1)

  Output: partial control map %{policy, top_k, max_retries}.

  Telemetry: [:brain, :dlpfc, :policy]
  """

  alias Brain.ControlSignals

  @type ctx :: map()

  @spec choose_policy(ctx) :: map()
  def choose_policy(ctx) when is_map(ctx) do
    acc_conflict = clamp01(Map.get(ctx, :acc_conflict, 0.0))
    wm_div = clamp01(Map.get(ctx, :wm_diversity, 0.0))
    novelty = clamp01(Map.get(ctx, :novelty, 0.0))

    base_top_k = Map.get(ctx, :top_k_base, 3) |> ensure_int_min(1)
    base_retries = Map.get(ctx, :max_retries_base, 1) |> ensure_int_min(1)

    policy =
      cond do
        acc_conflict < 0.25 and novelty < 0.25 and wm_div < 0.35 -> :strict_mwe
        acc_conflict > 0.65 or novelty > 0.6 -> :loose_single
        true -> :balanced
      end

    top_k = base_top_k + round(acc_conflict * 3)
    max_retries = base_retries + if(acc_conflict > 0.5, do: 1, else: 0)

    out = %{policy: policy, top_k: top_k, max_retries: max_retries}

    :telemetry.execute(
      [:brain, :dlpfc, :policy],
      %{acc_conflict: acc_conflict, wm_diversity: wm_div, novelty: novelty},
      %{policy: policy, top_k: top_k, max_retries: max_retries}
    )

    ControlSignals.combine([out]) |> Map.from_struct()
  end

  # helpers
  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0

  defp ensure_int_min(x, minv) when is_integer(x), do: if(x < minv, do: minv, else: x)
  defp ensure_int_min(_, minv), do: minv
end
