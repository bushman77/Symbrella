defmodule Core.Recall.Synonyms do
  @moduledoc """
  Facade for synonym lookup/expansion used by recall.

  Responsibilities:
    • Normalize inputs (case, NFC, optional POS).
    • Query provider(s) for raw candidates (DB, lexicons, etc.).
    • Rank/merge/dedupe with stable ordering.
    • (Optional) Cache results with TTL.
    • Emit telemetry for observability.

  Public API:
    - lookup/2   — ranked synonyms for a word (and optional POS)
    - expand/2   — expand many terms into a unified ranked set

  Swap providers via config (default keeps Core decoupled from :db):
      config :core, Core.Recall.Synonyms,
        provider: Core.Recall.Synonyms.Providers.External,
        cache?: true,
        ttl_ms: 60_000,
        top_k: 12

  Telemetry:
    [:core, :recall, :synonyms, :lookup]
      measurements: %{count: non_neg_integer()}
      metadata: %{word: String.t(), pos: atom() | nil, cached?: boolean(), took_ms: integer()}
  """

  alias Core.Recall.Synonyms.{Normalize, Ranker, Cache}

  @type word :: String.t()
  @type pos  :: atom() | nil
  @type entry :: %{
          lemma: word(),
          pos: pos(),
          prior: float(),
          source: String.t(),
          meta: map()
        }

  @default_top_k 12

  @spec lookup(word() | %{word: word(), pos: pos()}, keyword()) ::
          {:ok, [entry()], %{cached?: boolean(), took_ms: non_neg_integer()}}
  def lookup(word_or_map, opts \\ []) do
    t0 = System.monotonic_time(:millisecond)

    %{word: norm_word, pos: pos} =
      case word_or_map do
        %{} = m ->
          %{word: Normalize.word(Map.get(m, :word) || Map.get(m, "word")),
            pos: Normalize.pos(Map.get(m, :pos) || Map.get(m, "pos"))}

        w when is_binary(w) ->
          %{word: Normalize.word(w), pos: nil}
      end

    cache?  = get_opt(opts, :cache?, config(:cache?, true))
    ttl_ms  = get_opt(opts, :ttl_ms, config(:ttl_ms, 60_000))
    top_k   = get_opt(opts, :top_k, config(:top_k, @default_top_k))
    prov    = get_opt(opts, :provider, config(:provider, Core.Recall.Synonyms.Providers.Fallback))

    key = {norm_word, pos}

    # Use cache when present; otherwise hit provider safely.
    {raw, cached?} =
      case cache? && Cache.get(key, ttl_ms) do
        v when is_list(v) -> {v, true}
        _ -> {safe_lookup(prov, norm_word, pos, opts), false}
      end

    ranked = Ranker.rank(norm_word, raw, opts) |> Enum.take(top_k)

    if cache? and not cached?, do: Cache.put(key, ranked)

    took_ms = System.monotonic_time(:millisecond) - t0
    :telemetry.execute([:core, :recall, :synonyms, :lookup], %{count: length(ranked)}, %{
      word: norm_word, pos: pos, cached?: cached?, took_ms: took_ms
    })

    {:ok, ranked, %{cached?: cached?, took_ms: took_ms}}
  end

  @doc """
  Expand a list of query terms into a unique, ranked set suitable for recall.

  Options:
    :per_top_k — top-k per input term (default from lookup/2’s config)
    :limit     — overall cap after merge (default 64)
  """
  @spec expand([word() | %{word: word(), pos: pos()}], keyword()) ::
          {:ok, [entry()], %{inputs: non_neg_integer(), unique: non_neg_integer()}}
  def expand(terms, opts \\ []) when is_list(terms) do
    per_top_k = get_opt(opts, :per_top_k, get_opt(opts, :top_k, config(:top_k, @default_top_k)))
    limit     = get_opt(opts, :limit, 64)

    results =
      terms
      |> Enum.flat_map(fn t ->
        {:ok, list, _} = lookup(t, Keyword.put(opts, :top_k, per_top_k))
        list
      end)
      |> dedupe_keep_best()
      |> Enum.take(limit)

    {:ok, results, %{inputs: length(terms), unique: length(results)}}
  end

  # ---- helpers --------------------------------------------------------

  defp safe_lookup(provider, word, pos, opts) do
    if function_exported?(provider, :lookup, 3) do
      case provider.lookup(word, pos, opts) do
        {:ok, list, _meta} when is_list(list) -> list
        {:ok, list} when is_list(list) -> list
        other ->
          require Logger
          Logger.warning("Synonyms provider returned unexpected: #{inspect(other)}")
          []
      end
    else
      []
    end
  end

  defp config(key, default), do:
    Application.get_env(:core, __MODULE__, [])
    |> Keyword.get(key, default)

  defp get_opt(opts, k, default), do: Keyword.get(opts, k, default)

  defp dedupe_keep_best(list) do
    list
    |> Enum.reduce(%{}, fn e = %{lemma: l, pos: p}, acc ->
      k = {l, p}
      Map.update(acc, k, e, fn prev ->
        if (prev[:prior] || 0.0) >= (e[:prior] || 0.0), do: prev, else: e
      end)
    end)
    |> Map.values()
    |> Enum.sort_by(&{-(&1[:prior] || 0.0), &1.lemma})
  end
end

