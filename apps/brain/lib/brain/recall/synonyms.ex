defmodule Brain.Recall.Synonyms do
  @moduledoc """
  Brain-side synonym lookup (deps: **db ← brain** only).

  - Uses `Db` + `Db.BrainCell` as source of truth.
  - POS-aware lookup with graceful fallback.
  - Batch expansion for token lists.
  - Light telemetry spans for observability.

  Telemetry:
    • [:brain, :synonyms, :lookup]  — span for single lemma(+pos) query
       start_meta: %{lemma, pos, k}
       stop_meta:  %{count, hit?: boolean()}
    • [:brain, :synonyms, :expand]  — span around `expand_for_tokens/2`
       start_meta: %{tokens: non_neg_integer(), k}
       stop_meta:  %{hits, misses}
  """

  import Ecto.Query, only: [from: 2]
  alias Db
  alias Db.BrainCell

  @type lemma :: String.t()
  @type pos :: String.t() | atom() | nil
  @type k :: pos_integer()
  @type syns :: [String.t()]
  @type token :: %{
          required(:phrase) => String.t(),
          optional(:pos) => any(),
          optional(:mw) => boolean(),
          optional(:index) => non_neg_integer()
        }

  @default_k 5

  # ───────────────────────── Public API ─────────────────────────

  @spec lookup(lemma, k) :: syns
  def lookup(lemma, k \\ @default_k)
      when is_binary(lemma) and is_integer(k) and k > 0 do
    do_lookup(String.downcase(lemma), nil, k)
  end

  @spec lookup_by_pos(lemma, pos, k) :: syns
  def lookup_by_pos(lemma, pos, k \\ @default_k)
      when is_binary(lemma) and (is_binary(pos) or is_atom(pos) or is_nil(pos)) and
             is_integer(k) and k > 0 do
    lemma = String.downcase(lemma)
    pos_s = normalize_pos(pos)

    case do_lookup(lemma, pos_s, k) do
      [] -> do_lookup(lemma, nil, k)
      xs -> xs
    end
  end

  @spec expand_for_tokens([token], keyword) :: %{
          results:
            list(%{index: non_neg_integer(), phrase: String.t(), pos: any(), synonyms: syns}),
          hits: non_neg_integer(),
          misses: non_neg_integer()
        }
  def expand_for_tokens(tokens, opts \\ []) when is_list(tokens) do
    k = Keyword.get(opts, :k, @default_k)
    keep_self? = Keyword.get(opts, :keep_self?, false)

    :telemetry.span([:brain, :synonyms, :expand], %{tokens: length(tokens), k: k}, fn ->
      {results, hits} =
        tokens
        |> Enum.with_index()
        |> Enum.map_reduce(0, fn {tok, i}, acc_hits ->
          idx = Map.get(tok, :index, i)
          phrase = to_string(Map.fetch!(tok, :phrase))
          pos = Map.get(tok, :pos)

          syns =
            if is_nil(pos),
              do: lookup(phrase, k),
              else:
                lookup_by_pos(phrase, pos, k)
                |> maybe_drop_self(phrase, keep_self?)
                |> Enum.take(k)

          item = %{index: idx, phrase: phrase, pos: pos, synonyms: syns}
          acc_hits_new = acc_hits + if syns == [], do: 0, else: 1
          {item, acc_hits_new}
        end)

      misses = length(tokens) - hits
      {%{results: results, hits: hits, misses: misses}, %{hits: hits, misses: misses}}
    end)
  end

  # ───────────────────────── Internal ─────────────────────────

  defp normalize_pos(nil), do: nil
  defp normalize_pos(p) when is_atom(p), do: p |> Atom.to_string() |> String.downcase()
  defp normalize_pos(p) when is_binary(p), do: String.downcase(p)

  @spec do_lookup(lemma, String.t() | nil, k) :: syns
  defp do_lookup(lemma, pos, k) do
    :telemetry.span([:brain, :synonyms, :lookup], %{lemma: lemma, pos: pos, k: k}, fn ->
      q =
        from(c in BrainCell,
          where: c.word == ^lemma,
          select: %{pos: c.pos, synonyms: c.synonyms}
        )

      rows = Db.all(q)

      rows =
        case pos do
          nil -> rows
          _ -> Enum.filter(rows, fn r -> normalize_pos(r.pos) == pos end)
        end

      syns =
        rows
        |> Enum.flat_map(fn r -> List.wrap(r.synonyms || []) end)
        |> normalize_uniq()
        |> Enum.reject(&(&1 == lemma))
        |> Enum.take(k)

      {syns, %{count: length(syns), hit?: syns != []}}
    end)
  rescue
    _ -> []
  end

  # lowercase-unique, preserve first appearance casing
  defp normalize_uniq(list) do
    Enum.reduce(list, {[], MapSet.new()}, fn s, {acc, seen} ->
      s = to_string(s)
      key = String.downcase(s)
      if MapSet.member?(seen, key), do: {acc, seen}, else: {[s | acc], MapSet.put(seen, key)}
    end)
    |> elem(0)
    |> Enum.reverse()
  end

  defp maybe_drop_self(list, phrase, keep_self?) do
    if keep_self? do
      list
    else
      lemma = String.downcase(phrase)
      Enum.reject(list, fn s -> String.downcase(s) == lemma end)
    end
  end
end
