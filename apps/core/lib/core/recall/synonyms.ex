# apps/core/lib/core/recall/synonyms.ex
defmodule Core.Recall.Synonyms do
  @moduledoc """
  Thin compatibility façade for recall-time synonym usage.

  • Keeps the existing public API: `lookup/2` and `expand/2`.
  • Delegates retrieval & policy to `Core.Synonyms` (which calls Brain via Core.BrainAdapter).
  • Adapts return shape to the legacy `[entry]` format used by recall (lemma/pos/prior/source/meta).
  • Emits telemetry under [:core, :recall, :synonyms, :lookup].

  This module intentionally contains no Db or legacy Lexicon references.
  """

  @type word :: String.t()
  @type pos  :: atom() | nil

  @type entry :: %{
          lemma: word(),
          pos: pos(),
          prior: float(),
          source: String.t() | atom(),
          meta: map()
        }

  @default_top_k 12

  # --------------------------------------------------------------------
  # Back-compat shim for older callsites that expect a plain map.
  # Aligns to Core.Synonyms.for_keys/2 returning {:ok, term, meta}.
  # --------------------------------------------------------------------
  @spec for_keys([String.t() | {String.t(), atom()} | {:mwe, [String.t()]}], keyword()) :: map()
  def for_keys(keys, opts \\ []) do
    case Core.Synonyms.for_keys(keys, opts) do
      {:ok, m, _meta} when is_map(m) -> m
      _ -> Map.new(keys, &{&1, []})
    end
  end

  @doc """
  Ranked synonyms for a word (and optional POS).

  Accepts:
    • binary word
    • %{word: ..., pos: ...} map

  Returns: {:ok, [entry], %{cached?: boolean(), took_ms: non_neg_integer()}}
  (cached? is surfaced if Core.Synonyms provides it; otherwise false)
  """
  @spec lookup(word() | %{word: word(), pos: pos()}, keyword()) ::
          {:ok, [entry()], %{cached?: boolean(), took_ms: non_neg_integer()}}
  def lookup(word_or_map, opts \\ []) do
    t0 = System.monotonic_time(:millisecond)

    {norm_word, pos} =
      case word_or_map do
        %{} = m ->
          {normalize_word(Map.get(m, :word) || Map.get(m, "word")),
           normalize_pos(Map.get(m, :pos) || Map.get(m, "pos"))}

        w when is_binary(w) ->
          {normalize_word(w), nil}
      end

    top_k = Keyword.get(opts, :top_k, config(:top_k, @default_top_k))

    syn_opts =
      opts
      |> Keyword.put_new(:limit, top_k)
      |> Keyword.put_new(:pos_filter, pos)

    key = build_key(norm_word, pos)

    {map, cached?} =
      case Core.Synonyms.for_keys([key], syn_opts) do
        {:ok, m, meta} when is_map(m) -> {m, Map.get(meta, :cached?, false)}
        _                             -> {%{}, false}  # defensive
      end

    list =
      map
      |> Map.get(key, [])
      |> Enum.map(&syn_to_entry/1)
      |> Enum.take(top_k)

    took_ms = System.monotonic_time(:millisecond) - t0

    :telemetry.execute(
      [:core, :recall, :synonyms, :lookup],
      %{count: length(list)},
      %{word: norm_word, pos: pos, cached?: cached?, took_ms: took_ms}
    )

    {:ok, list, %{cached?: cached?, took_ms: took_ms}}
  end

  @doc """
  Expand a list of terms into a unique, ranked set for recall.

  Options:
    :per_top_k — top-k per input (defaults to lookup/2’s top_k)
    :limit     — overall cap after merge (default 64)
  """
  @spec expand([word() | %{word: word(), pos: pos()}], keyword()) ::
          {:ok, [entry()], %{inputs: non_neg_integer(), unique: non_neg_integer()}}
  def expand(terms, opts \\ []) when is_list(terms) do
    per_top_k = Keyword.get(opts, :per_top_k, Keyword.get(opts, :top_k, config(:top_k, @default_top_k)))
    limit     = Keyword.get(opts, :limit, 64)

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

  # ---------- helpers --------------------------------

  defp syn_to_entry(s) when is_binary(s) do
    %{lemma: s, pos: nil, prior: 0.0, source: "brain", meta: %{}}
  end

  defp syn_to_entry(%{term: term} = s) do
    %{
      lemma: term,
      pos: Map.get(s, :pos),
      prior: (Map.get(s, :weight) || 0.0) * 1.0,
      source: Map.get(s, :source) || :brain,
      meta: %{origin_key: Map.get(s, :key)}
    }
  end

  defp syn_to_entry(%{} = s) do
    term =
      Map.get(s, :lemma) || Map.get(s, :term) ||
        Map.get(s, "lemma") || Map.get(s, "term") || ""

    %{
      lemma: term,
      pos: Map.get(s, :pos),
      prior: (Map.get(s, :prior) || Map.get(s, :weight) || 0.0) * 1.0,
      source: Map.get(s, :source) || :brain,
      meta: Map.drop(s, [:lemma, :term, :pos, :prior, :weight, :source])
    }
  end

  defp normalize_word(w) when is_binary(w), do: w |> String.trim() |> String.downcase()
  defp normalize_word(_), do: ""

  defp normalize_pos(nil), do: nil
  defp normalize_pos(p) when is_atom(p), do: p

  defp normalize_pos(p) when is_binary(p) do
    p
    |> String.downcase()
    |> case do
      "noun" -> :noun
      "verb" -> :verb
      "adj"  -> :adj
      "adv"  -> :adv
      other  -> String.to_atom(other)
    end
  end

  defp normalize_pos(_), do: nil

  defp build_key(word, nil), do: word
  defp build_key(word, pos) when is_binary(word) and is_atom(pos), do: {word, pos}

  defp config(key, default) do
    Application.get_env(:core, __MODULE__, [])
    |> Keyword.get(key, default)
  end

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

