defmodule Brain.Semantics do
  @moduledoc """
  Semantic anchors & tiny heuristics used to influence gating decisions.

  Accepts either:
    • a lemma (string) → looks up anchor boost
    • a candidate map   → extracts lemma/id/pos and applies safe nudges

  Bias output is small and bounded. Unknown inputs return 0.0.
  """

  # Anchor table (positive-only, by lemma)
  @anchors %{
    # Action verbs
    "intend" => %{boost: 0.12, regions: [:lifg]},
    "read" => %{boost: 0.08, regions: [:pmtg]},
    "know" => %{boost: 0.10, regions: [:temporal]},
    "remember" => %{boost: 0.11, regions: [:hippocampus]},
    "think" => %{boost: 0.09, regions: [:frontal]},
    "mean" => %{boost: 0.13, regions: [:atl, :lifg]},

    # Objects + concepts
    "object" => %{boost: 0.07, regions: [:atl]},
    "person" => %{boost: 0.06, regions: [:temporal]},
    "story" => %{boost: 0.09, regions: [:pmtg]},
    "concept" => %{boost: 0.10, regions: [:atl]},
    "context" => %{boost: 0.07, regions: [:temporal]},
    "idea" => %{boost: 0.08, regions: [:frontal]},
    "event" => %{boost: 0.05, regions: [:hippocampus]}
  }

  # Public: get the full anchor map (useful for introspection/diagnostics)
  @spec anchors() :: map()
  def anchors, do: @anchors

  # ── Bias (primary API) ─────────────────────────────────────────────────────

  @doc """
  Returns a small bias for gating.

  Inputs:
    • binary lemma → anchor boost or 0.0
    • candidate map → anchor boost (+) tiny semantic nudges, clamped

  Bounds: [-0.10, +0.20]. (Anchors are non-negative; nudges may be slightly negative.)
  """
  @spec bias_for(map() | String.t() | any()) :: float()
  def bias_for(nil), do: 0.0

  def bias_for(lemma) when is_binary(lemma) do
    case Map.get(@anchors, String.downcase(lemma)) do
      %{boost: b} when is_number(b) -> b * 1.0
      _ -> 0.0
    end
  end

  def bias_for(%{} = cand) do
    lemma = cand_get(cand, [:lemma]) || lemma_from_id(cand_get(cand, [:id])) || ""
    id = cand_get(cand, [:id]) || ""
    pos = cand_get(cand, [:pos]) || cand_get_in(cand, [:features, :pos])
    source = cand_get(cand, [:source])
    reason = cand_get(cand, [:reason])
    kind = cand_get(cand, [:kind])

    anchor = bias_for(lemma)

    curiosity? =
      source == :curiosity or reason == :curiosity or
        kind in [:probe, :idle_probe, "probe", "idle_probe"]

    phrase_fallback? =
      is_binary(id) and String.ends_with?(id, "|phrase|fallback")

    proper? =
      case pos do
        nil -> false
        p when is_atom(p) -> String.contains?(Atom.to_string(p), "proper")
        p when is_binary(p) -> String.contains?(String.downcase(p), "proper")
        _ -> false
      end

    # Tiny, conservative nudges
    nudges =
      0.0
      |> add_if(curiosity?, +0.02)
      |> add_if(phrase_fallback?, -0.03)
      |> add_if(proper?, -0.01)

    clamp(anchor + nudges, -0.10, 0.20)
  end

  def bias_for(_), do: 0.0

  # ── Regions (secondary API) ─────────────────────────────────────────────────

  @doc """
  Returns semantic region tags for a lemma (or candidate map), or [] if not found.
  """
  @spec regions_for(map() | String.t() | any()) :: [atom()]
  def regions_for(nil), do: []

  def regions_for(lemma) when is_binary(lemma) do
    case Map.get(@anchors, String.downcase(lemma)) do
      %{regions: r} when is_list(r) -> r
      _ -> []
    end
  end

  def regions_for(%{} = cand) do
    lemma = cand_get(cand, [:lemma]) || lemma_from_id(cand_get(cand, [:id])) || ""
    regions_for(lemma)
  end

  def regions_for(_), do: []

  # ── Helpers ─────────────────────────────────────────────────────────────────

  defp add_if(x, true, d) when is_number(x) and is_number(d), do: x + d
  defp add_if(x, _cond, _d), do: x

  defp clamp(x, lo, hi) when is_number(x) do
    x
    |> max(lo)
    |> min(hi)
    |> Kernel.*(1.0)
  end

  defp lemma_from_id(nil), do: nil

  defp lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> String.downcase(w)
      _ -> nil
    end
  end

  defp lemma_from_id(_), do: nil

  # tolerant access (atom or string keys)
  defp cand_get(%{} = m, [k]), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp cand_get(_, _), do: nil

  defp cand_get_in(%{} = m, path) do
    case get_in(m, path) do
      nil ->
        path_s = Enum.map(path, fn k -> if is_atom(k), do: to_string(k), else: k end)
        get_in(m, path_s)

      v ->
        v
    end
  end

  defp cand_get_in(_, _), do: nil
end
