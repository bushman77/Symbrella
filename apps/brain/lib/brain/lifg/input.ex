defmodule Brain.LIFG.Input do
  @moduledoc """
  Candidate extraction + slate helpers for LIFG Stage-1.

  Accepts many shapes (flat lists, grouped-by-token maps, `active_cells`,
  slate winners) and normalizes to:
    • A flat list of `%{token_index, id/lemma/..., score?}` via `lifg_candidates!/1`
    • A per-token slate `%{idx => [cand, ...]}` via `slate_for/1`

  Notes:
    • All token indexes are coerced to integers (fallback 0).
    • All candidates returned by helpers are **plain maps** (no structs).
    • `:id` is always a **trimmed binary** when present.
    • `:score` is coerced to float and clamped into [0.0, 1.0].
  """

  alias Brain.Utils.Safe
  alias Brain.LIFG.SlateFilter

  @type candidate :: %{
          optional(:token_index) => non_neg_integer(),
          optional(:id) => String.t(),
          optional(:score) => number(),
          optional(:lemma) => String.t(),
          optional(:word) => String.t()
        }

  @type slate_map :: %{optional(non_neg_integer()) => [candidate()]}

  # ───────────────────────────── Public API ─────────────────────────────

  @doc """
  Extract a flat list of candidates from many input shapes.

  Raises if it cannot determine candidates.
  """
  @spec lifg_candidates!(list() | map()) :: [candidate()]
  def lifg_candidates!(list) when is_list(list) do
    list
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&Safe.to_plain/1)
    |> Enum.map(&normalize_candidate(&1, 0))
    |> Enum.reject(&reject_bad_candidate/1)
  end

  def lifg_candidates!(%{} = any_map) do
    m = Safe.to_plain(any_map)

    cands =
      cond do
        is_list(Safe.get(m, :lifg_candidates)) ->
          Safe.get(m, :lifg_candidates)

        is_list(Safe.get(m, :candidates)) ->
          Safe.get(m, :candidates)

        is_list(Safe.get(m, :sense_candidates)) ->
          Safe.get(m, :sense_candidates)

        is_map(Safe.get(m, :sense_candidates)) ->
          flatten_candidate_groups(Safe.get(m, :sense_candidates))

        is_map(Safe.get(m, :candidates_by_token)) ->
          flatten_candidate_groups(Safe.get(m, :candidates_by_token))

        is_list(Safe.get(m, :active_cells)) ->
          candidates_from_active_cells(Safe.get(m, :active_cells))

        is_list(get_in(m, [:slate, :winners])) ->
          winners_to_candidates(get_in(m, [:slate, :winners]))

        true ->
          raise ArgumentError, "Cannot extract LIFG candidates from: #{inspect(any_map)}"
      end

    cands
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&Safe.to_plain/1)
    |> Enum.map(fn c -> normalize_candidate(c, Map.get(c, :token_index, Map.get(c, "token_index", 0))) end)
    |> Enum.reject(&reject_bad_candidate/1)
  end

  @doc """
  Build a per-token slate for Stage-1.

  Always normalizes:
  – token index keys → integers
  – values → list of **plain maps** (no structs)
  – each candidate carries `:token_index`
  – scores (when present) are floats clamped to [0,1]
  """
  @spec slate_for(list() | map()) :: slate_map()
  def slate_for(%{} = any_map) do
    m = Safe.to_plain(any_map)

    raw =
      cond do
        is_map(Safe.get(m, :sense_candidates)) ->
          normalize_slate_map(Safe.get(m, :sense_candidates))

        is_map(Safe.get(m, :candidates_by_token)) ->
          normalize_slate_map(Safe.get(m, :candidates_by_token))

        true ->
          group_by_token_index(lifg_candidates!(m))
      end

    SlateFilter.sanitize_map(raw)
  end

  def slate_for(list) when is_list(list) do
    list
    |> group_by_token_index()
    |> SlateFilter.sanitize_map()
  end

  # ───────────────────────────── Helpers (private) ─────────────────────────────

  # Group a flat candidate list by token index, preserving original order per index.
  defp group_by_token_index(cands) when is_list(cands) do
    cands
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&Safe.to_plain/1)
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {c0, _ord}, acc ->
      idx = parse_idx(c0[:token_index] || c0["token_index"])
      c1  = normalize_candidate(c0, idx)
      Map.update(acc, idx, [c1], &(&1 ++ [c1]))
    end)
  end

  # Normalize an incoming slate map: keys → ints; values → normalized candidate maps.
  defp normalize_slate_map(groups) when is_map(groups) do
    groups
    |> Enum.reduce(%{}, fn {tidx, senses}, acc ->
      idx = parse_idx(tidx)

      list =
        senses
        |> Safe.ensure_list()
        |> Enum.reject(&is_nil/1)
        |> Enum.map(&Safe.to_plain/1)
        |> Enum.map(&normalize_candidate(&1, idx))
        |> Enum.reject(&reject_bad_candidate/1)

      Map.put(acc, idx, list)
    end)
  end

  # Flatten a by-token groups map into a single list of candidates.
  defp flatten_candidate_groups(groups) when is_map(groups) do
    groups
    |> Enum.flat_map(fn {tidx, senses} ->
      idx = parse_idx(tidx)

      senses
      |> Safe.ensure_list()
      |> Enum.reject(&is_nil/1)
      |> Enum.map(&Safe.to_plain/1)
      |> Enum.map(&normalize_candidate(&1, idx))
      |> Enum.reject(&reject_bad_candidate/1)
    end)
  end

  # Convert winners (e.g., from Hygiene/Stage-1) into plain candidates.
  defp winners_to_candidates(winners) when is_list(winners) do
    winners
    |> Enum.with_index()
    |> Enum.map(fn {w0, idx} ->
      w  = Safe.to_plain(w0)

      raw_id =
        Safe.get(w, :id) ||
          Safe.get(w, :chosen_id) ||
          Safe.get(w, :lemma) ||
          Safe.get(w, :word)

      token_idx = parse_idx(Safe.get(w, :token_index, idx))

      c = %{token_index: token_idx, id: raw_id}
      normalize_candidate(c, token_idx)
    end)
    |> Enum.reject(&reject_bad_candidate/1)
  end

  # Build candidates from active_cells rows, supporting several shapes:
  #  - scores: %{id => p, ...}
  #  - chosen_id: "w|pos|sense"
  #  - id / lemma / word with optional :score
  defp candidates_from_active_cells(ac) when is_list(ac) do
    ac
    |> Enum.flat_map(fn cell0 ->
      cell  = Safe.to_plain(cell0)
      idx   = parse_idx(Safe.get(cell, :token_index, 0))
      scores = Safe.get(cell, :scores)

      base =
        cond do
          is_map(scores) ->
            for {id, s} <- scores do
              normalize_candidate(%{token_index: idx, id: id, score: s}, idx)
            end

          id = Safe.get(cell, :chosen_id) ->
            [normalize_candidate(%{token_index: idx, id: id, score: 1.0}, idx)]

          id = Safe.get(cell, :id) ->
            s = Safe.get(cell, :score, 0.0)
            [normalize_candidate(%{token_index: idx, id: id, score: s}, idx)]

          lemma = (Safe.get(cell, :lemma) || Safe.get(cell, :word)) ->
            s = Safe.get(cell, :score, 0.0)
            [normalize_candidate(%{token_index: idx, id: lemma, score: s}, idx)]

          true ->
            []
        end

      Enum.reject(base, &reject_bad_candidate/1)
    end)
  end

  # ───────────────────────────── Normalization utilities ─────────────────────────────

  defp normalize_candidate(map, default_idx) when is_map(map) do
    m = Safe.to_plain(map)

    idx =
      m
      |> Map.get(:token_index, Map.get(m, "token_index", default_idx))
      |> parse_idx()

    # Prefer explicit :id; otherwise promote :lemma/:word into :id so Stage-1 can use it.
    raw_id =
      m[:id] || m["id"] ||
        m[:lemma] || m["lemma"] ||
        m[:word] || m["word"]

    id =
      raw_id
      |> to_string_if_present()
      |> case do
        nil -> nil
        s   -> s |> String.trim()
      end

    score =
      m
      |> Map.get(:score, Map.get(m, "score"))
      |> to_float_if_present()
      |> clamp01_or_nil()

    out =
      m
      |> Map.put(:token_index, idx)
      |> maybe_put(:id, id)
      |> maybe_put(:score, score)

    # Ensure we don't leak structs or nested exotic types
    Safe.to_plain(out)
  end

  defp normalize_candidate(other, default_idx) do
    # catch-all for bare ids/lemmas etc.
    id = other |> to_string_if_present() |> case do
      nil -> nil
      s -> String.trim(s)
    end

    %{token_index: parse_idx(default_idx)}
    |> maybe_put(:id, id)
    |> Safe.to_plain()
  end

  defp reject_bad_candidate(%{id: id}) when is_binary(id), do: id == ""
  defp reject_bad_candidate(%{id: nil}), do: true
  defp reject_bad_candidate(_), do: false

  defp parse_idx(i) when is_integer(i) and i >= 0, do: i
  defp parse_idx(i) when is_integer(i) and i < 0,  do: 0
  defp parse_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end
  defp parse_idx(_), do: 0

  defp clamp01_or_nil(nil), do: nil
  defp clamp01_or_nil(n) when is_number(n), do: n |> max(0.0) |> min(1.0)

  defp to_float_if_present(nil), do: nil
  defp to_float_if_present(n) when is_float(n), do: n
  defp to_float_if_present(n) when is_integer(n), do: n * 1.0
  defp to_float_if_present(b) when is_binary(b) do
    case Float.parse(b) do
      {f, _} -> f
      _ -> nil
    end
  end
  defp to_float_if_present(_), do: nil

  defp to_string_if_present(nil), do: nil
  defp to_string_if_present(v) when is_binary(v), do: v
  defp to_string_if_present(v), do: to_string(v)

  defp maybe_put(map, _k, nil), do: map
  defp maybe_put(map, k, v), do: Map.put(map, k, v)
end

