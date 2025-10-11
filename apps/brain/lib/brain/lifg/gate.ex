# apps/brain/lib/brain/lifg/gate.ex
defmodule Brain.LIFG.Gate do
  @moduledoc """
  Stage-1 → WM bridge.

  Transforms LIFG Stage-1 `choices` into **Working Memory candidates** with a
  consistent shape that downstream gating logic can rely on.

  ### Guarantees
  - Always emits candidates with a numeric **`:score`** in `[0.0, 1.0]`.
  - Populates **`:id`**, **`:token_index`**, **`:source` = `:lifg`**,
    **`:reason` = `:stage1_winner`**, **`:ts`**, and a best-effort **`:lemma`**.
  - Respects `min` threshold using the normalized score.
  - Order of results follows the order of `choices`.

  ### Score selection (in order of preference)
  1. `:p_top1`                          — probability of top-1 (if present)
  2. `probs[chosen_id]`                 — probability of the chosen id
  3. `scores[chosen_id]`                — normalized score (common path)
  4. `:score`                           — direct score, if a prior step set it
  5. `:prob_margin` or `:margin`        — fallbacks (clamped into [0,1])
  6. `0.0`                              — last resort
  """

  @type lifg_choice :: %{
          optional(:token_index) => non_neg_integer(),
          optional(:chosen_id) => String.t(),
          optional(:lemma) => String.t(),
          optional(:scores) => map(),
          optional(:probs) => map(),
          optional(:p_top1) => number(),
          optional(:prob_margin) => number(),
          optional(:margin) => number(),
          optional(:features) => map(),
          optional(:score) => number()
        }

  @spec stage1_wm_candidates([lifg_choice()] | any(), non_neg_integer(), float()) :: [map()]
  def stage1_wm_candidates(choices, now_ms, min) when is_list(choices) do
    choices
    |> Enum.flat_map(fn ch ->
      id = fetch_id(ch)
      score = score_from_choice(ch) |> clamp01()

      if id != nil and score >= min do
        [
          %{
            id: id,
            lemma: fetch_lemma(ch, id),
            token_index: fetch_token_index(ch),
            score: score,
            source: :lifg,
            reason: :stage1_winner,
            ts: now_ms
          }
        ]
      else
        []
      end
    end)
  end

  def stage1_wm_candidates(_, _now_ms, _min), do: []

  # ────────────────────────────── helpers ──────────────────────────────

  defp fetch_id(ch) do
    case get(ch, :chosen_id) do
      nil -> nil
      id -> to_string(id)
    end
  end

  defp fetch_token_index(ch) do
    case get(ch, :token_index) do
      i when is_integer(i) and i >= 0 -> i
      _ -> 0
    end
  end

  defp fetch_lemma(ch, id) do
    with nil <- get(ch, :lemma),
         # Sometimes a normalized form is present:
         nil <- get(ch, :norm),
         # Derive from id prefix before first "|"
         lemma <- lemma_from_id(id) do
      lemma
    else
      l when is_binary(l) and l != "" -> l
      _ -> nil
    end
  end

  defp lemma_from_id(nil), do: nil
  defp lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [w | _] -> w
      _ -> nil
    end
  end

  # Score selection in priority order (see moduledoc)
  defp score_from_choice(ch) do
    chosen_id = fetch_id(ch)
    probs = map_or_nil(get(ch, :probs))
    scores = map_or_nil(get(ch, :scores))

    cond do
      is_number(get(ch, :p_top1)) ->
        get(ch, :p_top1)

      chosen_id && is_map(probs) && is_number(Map.get(probs, chosen_id)) ->
        Map.get(probs, chosen_id)

      chosen_id && is_map(scores) && is_number(Map.get(scores, chosen_id)) ->
        Map.get(scores, chosen_id)

      is_number(get(ch, :score)) ->
        get(ch, :score)

      is_number(get(ch, :prob_margin)) ->
        get(ch, :prob_margin)

      is_number(get(ch, :margin)) ->
        get(ch, :margin)

      true ->
        # Last resort: try legacy features or raw values
        case get_in_maybe(ch, [:features, :score_norm]) || get_in_maybe(ch, [:features, :score_raw]) do
          s when is_number(s) -> s
          _ -> 0.0
        end
    end
  end

  defp clamp01(x) when is_number(x) do
    x
    |> max(0.0)
    |> min(1.0)
    |> Kernel.*(1.0)
  end

  defp clamp01(_), do: 0.0

  # tolerant fetches for map or keyword or struct-like maps with string keys mixed in
  defp get(m, k) when is_map(m), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp get(m, k) when is_list(m), do: Keyword.get(m, k)
  defp get(_, _), do: nil

  defp get_in_maybe(m, path) do
    case m do
      %{} ->
        Map.get(m, hd(path)) || Map.get(m, to_string(hd(path)))
        |> case do
          nil -> nil
          next when length(path) == 1 -> next
          next -> get_in_maybe(next, tl(path))
        end

      _ ->
        nil
    end
  end

  defp map_or_nil(%{} = m), do: m
  defp map_or_nil(_), do: nil
end

