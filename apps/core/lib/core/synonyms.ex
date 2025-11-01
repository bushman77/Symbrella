defmodule Core.Synonyms do
  @moduledoc """
  Core-level façade and policy point for synonym retrieval.

  • Calls Brain at runtime via `Core.BrainAdapter.synonyms_for_keys/2`.
  • Normalizes keys, adapts results to a stable `syn_obj` shape,
    ranks/dedupes, and emits telemetry (safely).
  """

  alias Core.BrainAdapter
  alias Core.Telemetry

  @type pos ::
          :noun | :verb | :adj | :adv | :pron | :det | :adp | :num | :part |
          :intj | :conj | :aux | :punct | :sym | :x | atom() | nil

  @type key :: String.t() | {String.t(), pos()} | {:mwe, [String.t()]}
  @type syn_obj :: %{
          term: String.t(),
          pos: pos(),
          weight: number() | nil,
          source: atom() | String.t() | nil,
          key: key() | nil
        }

  @default_limit 12

  @spec for_keys([key()], keyword()) ::
          {:ok, %{optional(key()) => [syn_obj()]}, %{cached?: boolean(), took_ms: non_neg_integer()}}
  def for_keys(keys, opts \\ []) when is_list(keys) do
    t0 = now_ms()
    limit      = Keyword.get(opts, :limit, @default_limit)
    pos_filter = Keyword.get(opts, :pos_filter, nil)
    dedupe?    = Keyword.get(opts, :dedupe, true)
    mwe_only?  = Keyword.get(opts, :mwe_only, false)

    norm_keys = Enum.map(keys, &normalize_key/1) |> Enum.reject(&is_nil/1)

    result =
      case BrainAdapter.synonyms_for_keys(norm_keys, %{pos_filter: pos_filter, limit: limit}) do
        {:ok, %{} = map} ->
          out =
            map
            |> Enum.map(fn {k, list} ->
              {k,
               list
               |> Enum.map(fn item -> adapt_syn(item, k) end)
               |> filter_pos(pos_filter)
               |> maybe_dedupe(dedupe?)
               |> rank_and_limit(limit, k, mwe_only?)}
            end)
            |> Map.new()

          took = now_ms() - t0
          Telemetry.emit([:core, :synonyms, :lookup], %{count: count_items(out)}, %{
            keys: length(norm_keys), cached?: false, took_ms: took
          })
          {:ok, out, %{cached?: false, took_ms: took}}

        {:error, reason} ->
          took = now_ms() - t0
          Telemetry.emit([:core, :synonyms, :lookup], %{count: 0}, %{
            keys: length(norm_keys), cached?: false, took_ms: took, unavailable?: true, reason: inspect(reason)
          })
          {:ok, Map.new(norm_keys, fn k -> {k, []} end), %{cached?: false, took_ms: took}}

        other ->
          took = now_ms() - t0
          Telemetry.emit([:core, :synonyms, :lookup], %{count: 0}, %{
            keys: length(norm_keys), cached?: false, took_ms: took, unexpected: inspect(other)
          })
          {:ok, Map.new(norm_keys, fn k -> {k, []} end), %{cached?: false, took_ms: took}}
      end

    result
  end

  @spec alternates_for_winner(term() | map() | {:ok, map(), any()}, keyword()) ::
          {:ok, [syn_obj()], %{cached?: boolean(), took_ms: non_neg_integer()}}
  def alternates_for_winner(winner, opts \\ [])
  def alternates_for_winner({:ok, m, _}, opts), do: alternates_for_winner(m, opts)
  def alternates_for_winner(%{} = m, opts) do
    term = m[:term] || m[:lemma] || m[:word] || ""
    pos  = m[:pos]
    do_alternates(term, pos, opts)
  end
  def alternates_for_winner(term, opts) when is_binary(term), do: do_alternates(term, nil, opts)

  defp do_alternates(term, pos, opts) do
    limit      = Keyword.get(opts, :limit, @default_limit)
    pos_filter = Keyword.get(opts, :pos_filter, pos)
    include_w  = Keyword.get(opts, :include_winner, false)

    key = if pos_filter, do: {normalize_word(term), pos_filter}, else: normalize_word(term)

    with {:ok, map, meta} <- for_keys([key], limit: limit, pos_filter: pos_filter) do
      list =
        map
        |> Map.get(key, [])
        |> (fn l -> if include_w, do: l, else: Enum.reject(l, &same_word?(&1.term, term)) end).()
      {:ok, list, meta}
    else
      _ -> {:ok, [], %{cached?: false, took_ms: 0}}
    end
  end

  @spec expand_candidates(map(), keyword()) ::
          {:ok, map(), %{added: non_neg_integer(), took_ms: non_neg_integer()}}
  def expand_candidates(%{} = si, opts \\ []) do
    t0 = now_ms()
    limit_per  = Keyword.get(opts, :limit_per, 4)
    pos_filter = Keyword.get(opts, :pos_filter, nil)

    base = Map.get(si, :sense_candidates)
    if not is_list(base) do
      {:ok, si, %{added: 0, took_ms: 0}}
    else
      keys = Enum.map(base, &to_key/1) |> Enum.reject(&is_nil/1)
      {:ok, per_map, _meta} = for_keys(keys, limit: limit_per, pos_filter: pos_filter)

      added =
        per_map
        |> Map.values()
        |> List.flatten()
        |> Enum.map(& &1.term)
        |> Enum.reject(&is_nil/1)

      new_si = Map.update(si, :sense_candidates, base, fn list -> list ++ added end)
      {:ok, new_si, %{added: length(added), took_ms: now_ms() - t0}}
    end
  end

  # ----- helpers -----

  defp normalize_key({w, p}) when is_binary(w) and (is_atom(p) or is_nil(p)), do: {normalize_word(w), p}
  defp normalize_key(w) when is_binary(w), do: normalize_word(w)
  defp normalize_key({:mwe, toks}) when is_list(toks) do
    toks = toks |> Enum.map(&normalize_word/1) |> Enum.reject(&(&1 == ""))
    if toks == [], do: nil, else: {:mwe, toks}
  end
  defp normalize_key(_), do: nil

  defp normalize_word(w) when is_binary(w), do: w |> String.trim() |> String.downcase()
  defp normalize_word(_), do: ""

  defp adapt_syn(%{term: t} = s, origin_key) when is_binary(t) do
    %{term: t, pos: Map.get(s, :pos), weight: Map.get(s, :weight),
      source: Map.get(s, :source) || :brain, key: Map.get(s, :key) || origin_key}
  end
  defp adapt_syn(t, origin_key) when is_binary(t),
    do: %{term: t, pos: nil, weight: nil, source: :brain, key: origin_key}
  defp adapt_syn(%{} = s, origin_key) do
    term = Map.get(s, :lemma) || Map.get(s, :term) || Map.get(s, "lemma") || Map.get(s, "term") || ""
    %{term: term, pos: Map.get(s, :pos), weight: Map.get(s, :weight) || Map.get(s, :prior),
      source: Map.get(s, :source) || :brain, key: Map.get(s, :key) || origin_key}
  end

  defp filter_pos(list, nil), do: list
  defp filter_pos(list, pos) when is_atom(pos), do: Enum.filter(list, fn s -> is_nil(s.pos) or s.pos == pos end)

  defp maybe_dedupe(list, true),  do: dedupe_by_term_pos_keep_max_weight(list)
  defp maybe_dedupe(list, false), do: list

  defp rank_and_limit(list, limit, origin_key, mwe_only?) do
    list
    |> maybe_filter_mwe(mwe_only?, origin_key)
    |> dedupe_by_term_pos_keep_max_weight()
    |> Enum.sort_by(fn s -> {-(s.weight || 0.0), s.term} end)
    |> Enum.take(limit)
  end

  defp maybe_filter_mwe(list, true, {:mwe, _}), do: list
  defp maybe_filter_mwe(list, true, _), do: Enum.filter(list, fn s -> String.contains?(s.term, " ") end)
  defp maybe_filter_mwe(list, _false, _), do: list

  defp dedupe_by_term_pos_keep_max_weight(list) do
    list
    |> Enum.reduce(%{}, fn s = %{term: t, pos: p}, acc ->
      Map.update(acc, {t, p}, s, fn prev ->
        if (prev.weight || 0.0) >= (s.weight || 0.0), do: prev, else: s
      end)
    end)
    |> Map.values()
  end

  defp same_word?(a, b), do: String.downcase(String.trim(a)) == String.downcase(String.trim(b))
  defp to_key(%{term: t, pos: p}) when is_binary(t), do: if(p, do: {normalize_word(t), p}, else: normalize_word(t))
  defp to_key(%{lemma: t, pos: p}) when is_binary(t), do: if(p, do: {normalize_word(t), p}, else: normalize_word(t))
  defp to_key(t) when is_binary(t), do: normalize_word(t)
  defp to_key(_), do: nil

  defp count_items(map) when is_map(map),
    do: map |> Map.values() |> Enum.map(&length/1) |> Enum.sum()

  defp now_ms, do: System.monotonic_time(:millisecond)
end

