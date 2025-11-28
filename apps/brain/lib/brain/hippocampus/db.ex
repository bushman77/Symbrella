defmodule Brain.Hippocampus.DB do
  @moduledoc """
  DB-backed episodic recall (pgvector KNN) that returns the SAME shape your
  in-memory recall uses:

      [%{score: float(), at: non_neg_integer(), episode: %{slate, meta, norms}}]

  Scoring:
      score = similarity * recency * outcome_mult * overlap_mult

  Options (safe defaults):
    :embedding   – REQUIRED query vector (list(float) or %Pgvector{})
    :k           – neighbors to fetch (default 8)
    :tau_s       – recency half-life seconds (default 3600)
    :alpha       – good outcome gain (default 0.3)
    :beta        – bad outcome penalty (default 0.2)
    :gamma       – tag/token overlap boost (default 0.2)
    :min_sim     – minimum cosine similarity to consider (default 0.35)

    # filters (optional)
    :user_id     – limit by user
    :tokens_any  – require overlap with tokens via `tokens && ^list`
    :tags_any    – require overlap with tags via `tags && ^list`
    :since       – NaiveDateTime lower bound on inserted_at
    :now         – NaiveDateTime, defaults to utc_now()

    # soft lexical hint (optional; used if :tokens_any is not provided)
    :cues        – list/stringy cues (lemmas/norms/words) from Hippo
  """

  alias Db.Episode
  require Logger

  @type recall_r :: %{score: float(), at: non_neg_integer(), episode: map()}

  @spec recall(keyword()) :: [recall_r]
  def recall(opts) when is_list(opts) do
    # Required
    emb = Keyword.fetch!(opts, :embedding)

    # knobs
    now = Keyword.get(opts, :now, NaiveDateTime.utc_now())
    k = Keyword.get(opts, :k, 8)
    tau_s = max(1, Keyword.get(opts, :tau_s, 3600))
    a = clamp01(Keyword.get(opts, :alpha, 0.3))
    b = clamp01(Keyword.get(opts, :beta, 0.2))
    g = clamp01(Keyword.get(opts, :gamma, 0.2))
    ms = clamp01(Keyword.get(opts, :min_sim, 0.35))

    # lexical hints / filters
    cues = Keyword.get(opts, :cues, [])
    tokens_any = Keyword.get(opts, :tokens_any) || cues_to_tokens_any(cues)
    tags_any = Keyword.get(opts, :tags_any)

    # filters forwarded to Episode.knn/2
    knn_opts = [
      k: k,
      user_id: Keyword.get(opts, :user_id),
      tokens_any: tokens_any,
      tags_any: tags_any,
      since: Keyword.get(opts, :since)
    ]

    Episode.knn(emb, knn_opts)
    |> Enum.reduce([], fn ep, acc ->
      sim = 1.0 - (ep.distance || 1.0)

      if sim < ms do
        acc
      else
        age_s = max(1, NaiveDateTime.diff(now, ep.inserted_at, :second))
        rec = :math.exp(-age_s / half_life_to_lambda(tau_s))
        outm = outcome_mult(ep.si, a, b)
        tagm = overlap_mult(ep.si, ep.tags, tokens_any, g)

        score = sim * rec * outm * tagm

        norms = norms_from_si(ep.si)

        # Merge any stored meta from `si` so Hippo's priors/hints can see it.
        raw_meta = (ep.si[:meta] || ep.si["meta"] || %{}) |> Map.new()

        meta =
          raw_meta
          |> Map.put(:source, :db)
          |> Map.put(:id, ep.id)
          |> Map.put(:tags, ep.tags)

        at_ms =
          ep.inserted_at
          |> to_datetime!()
          |> DateTime.to_unix(:millisecond)

        [%{score: score, at: at_ms, episode: %{slate: ep.si, meta: meta, norms: norms}} | acc]
      end
    end)
    |> Enum.sort_by(& &1.score, :desc)
  rescue
    e ->
      Logger.error("Hippo.DB recall failed: #{Exception.message(e)}")
      []
  end

  # ── helpers ────────────────────────────────────────────────────────────────

  defp half_life_to_lambda(tau_s), do: tau_s / :math.log(2)

  defp outcome_mult(%{} = si, alpha, beta) do
    # Many callers embed outcome info under "meta" when persisting episodes.
    # Merge meta into si (si wins on conflicts) so we can read from both.
    meta = si[:meta] || si["meta"] || %{}
    src = Map.merge(meta, si)

    {good, bad} =
      cond do
        is_number(src[:outcome_good] || src["outcome_good"]) ->
          {num(src, :outcome_good, 0.0), num(src, :outcome_bad, 0.0)}

        is_number(src[:success] || src["success"]) ->
          {num(src, :success, 0.0), 0.0}

        is_number(src[:score] || src["score"]) ->
          s = num(src, :score, 0.0)
          {max(s, 0.0), max(-s, 0.0)}

        # Common pattern: a single outcome_score in [-1, 1] or similar
        is_number(src[:outcome_score] || src["outcome_score"]) ->
          s = num(src, :outcome_score, 0.0)
          {max(s, 0.0), max(-s, 0.0)}

        true ->
          {0.0, 0.0}
      end

    1.0 + alpha * good - beta * bad
  end

  defp outcome_mult(_, _a, _b), do: 1.0

  defp overlap_mult(%{} = si, tags, tokens_any, gamma) do
    key = decision_key(si)
    has_key = is_binary(key) and key != ""
    has_tag = has_key and is_list(tags) and Enum.any?(tags, &(&1 == key))
    has_tok = has_key and is_list(tokens_any) and Enum.any?(tokens_any, &(&1 == key))

    if has_tag or has_tok, do: 1.0 + gamma, else: 1.0
  end

  defp overlap_mult(_, _, _, _), do: 1.0

  defp decision_key(%{} = si) do
    si[:winner] || si["winner"] ||
      si[:lemma] || si["lemma"] ||
      si[:mwe] || si["mwe"] ||
      si[:decision] || si["decision"]
  end

  defp decision_key(_), do: nil

  defp norms_from_si(%{} = si) do
    slate = si[:slate] || si["slate"] || %{}

    winners =
      si[:winners] ||
        si["winners"] ||
        (si[:atl_slate] || %{})[:winners] ||
        (si["atl_slate"] || %{})["winners"] ||
        slate[:winners] ||
        slate["winners"] ||
        []

    winners
    |> List.wrap()
    |> Enum.flat_map(fn w ->
      [
        w[:lemma],
        w["lemma"],
        w[:norm],
        w["norm"],
        w[:mwe],
        w["mwe"]
      ]
    end)
    |> Enum.filter(&is_binary/1)
    |> MapSet.new()
  end

  defp norms_from_si(_), do: MapSet.new()

  # derive tokens_any from cues when caller didn't specify any
  defp cues_to_tokens_any(cues) do
    cues
    |> List.wrap()
    |> Enum.flat_map(fn
      s when is_binary(s) ->
        s
        |> String.downcase()
        |> String.trim()
        |> String.split(~r/\s+/, trim: true)

      m when is_map(m) ->
        candidate =
          Map.get(m, :lemma) || Map.get(m, "lemma") ||
            Map.get(m, :norm) || Map.get(m, "norm") ||
            Map.get(m, :word) || Map.get(m, "word")

        if is_binary(candidate) do
          candidate
          |> String.downcase()
          |> String.trim()
          |> then(&[&1])
        else
          []
        end

      _ ->
        []
    end)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_number(v) -> v * 1.0
      {_, v} when is_number(v) -> v * 1.0
      _ -> default * 1.0
    end
  end

  defp to_datetime!(%NaiveDateTime{} = ndt), do: DateTime.from_naive!(ndt, "Etc/UTC")
  defp to_datetime!(%DateTime{} = dt), do: dt

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
  defp clamp01(_), do: 0.0
end
