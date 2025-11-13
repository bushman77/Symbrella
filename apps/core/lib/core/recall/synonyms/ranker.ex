defmodule Core.Recall.Synonyms.Ranker do
  @moduledoc """
  Lightweight ranking that prefers:
    1) exact/self lemma,
    2) higher provider priors,
    3) character-set overlap with the query (cheap Jaccard),
    4) stable lexicographic tie-break.

  Tunables:
    :self_boost  — weight added when lemma == query (default 0.5)
    :prior_w     — weight for provider prior (default 0.7)
    :jaccard_w   — weight for Jaccard overlap (default 0.3)
  """

  @type entry :: map()

  @spec rank(String.t(), [entry()], keyword()) :: [entry()]
  def rank(query, entries, opts \\ []) do
    qset = char_set(query)

    self_boost = Keyword.get(opts, :self_boost, 0.5)
    prior_w = Keyword.get(opts, :prior_w, 0.7)
    jacc_w = Keyword.get(opts, :jaccard_w, 0.3)

    entries
    |> Enum.map(fn e = %{lemma: l} ->
      prior = e[:prior] || 0.0
      jacc = jaccard(qset, char_set(l))
      self? = if l == query, do: 1.0, else: 0.0

      score = prior_w * prior + jacc_w * jacc + self_boost * self?
      Map.put(e, :score, Float.round(score, 6))
    end)
    |> Enum.sort_by(&{-(&1[:score] || 0.0), -(&1[:prior] || 0.0), &1.lemma})
    |> Enum.uniq_by(&{&1.lemma, &1[:pos]})
  end

  # ---- tiny helpers ---------------------------------------------------

  defp char_set(s) do
    s
    |> String.graphemes()
    |> MapSet.new()
  end

  defp jaccard(a, b) do
    inter = MapSet.size(MapSet.intersection(a, b))
    union = MapSet.size(MapSet.union(a, b))
    if union == 0, do: 0.0, else: inter / union
  end
end
