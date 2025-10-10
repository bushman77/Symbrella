defmodule Brain.LIFG.Input do
  @moduledoc """
  Candidate extraction + slate helpers for LIFG Stage-1.

  Accepts many shapes (flat lists, grouped-by-token maps, `active_cells`,
  slate winners) and normalizes to:
    • A flat list of `%{token_index, id/lemma/..., score?}` via `lifg_candidates!/1`
    • A per-token slate `%{idx => [cand, ...]}` via `slate_for/1`
  """

  alias Brain.Utils.Safe

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
  def lifg_candidates!(list) when is_list(list), do: list

  def lifg_candidates!(%{} = any_map) do
    m = Safe.to_plain(any_map)

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

      is_list(Safe.get(Safe.get(m, :slate, %{}), :winners)) ->
        winners_to_candidates(Safe.get(Safe.get(m, :slate), :winners))

      true ->
        raise ArgumentError,
              "Cannot extract LIFG candidates from: #{inspect(any_map)}"
    end
  end

  @doc """
  Build a per-token slate for Stage-1.

  Always normalizes:
  – token index keys → integers
  – values → list of **plain maps** (no structs)
  – each candidate carries `:token_index`
  """
  @spec slate_for(list() | map()) :: slate_map()
  def slate_for(%{} = any_map) do
    m = Safe.to_plain(any_map)

    cond do
      is_map(Safe.get(m, :sense_candidates)) ->
        normalize_slate_map(Safe.get(m, :sense_candidates))

      is_map(Safe.get(m, :candidates_by_token)) ->
        normalize_slate_map(Safe.get(m, :candidates_by_token))

      true ->
        group_by_token_index(lifg_candidates!(m))
    end
  end

  def slate_for(list) when is_list(list), do: group_by_token_index(list)

  # ───────────────────────────── Helpers (private) ─────────────────────────────

  defp group_by_token_index(cands) when is_list(cands) do
    cands
    |> Enum.reduce(%{}, fn c, acc ->
      c1 = Safe.to_plain(c)

      idx =
        case c1[:token_index] || c1["token_index"] do
          i when is_integer(i) -> i
          _ -> 0
        end

      c2 = Map.put_new(c1, :token_index, idx)
      Map.update(acc, idx, [c2], &[c2 | &1])
    end)
    |> Enum.into(%{}, fn {k, vs} -> {k, Enum.reverse(vs)} end)
  end

  defp normalize_slate_map(groups) when is_map(groups) do
    groups
    |> Enum.into(%{}, fn {tidx, senses} ->
      idx =
        case tidx do
          i when is_integer(i) ->
            i

          b when is_binary(b) ->
            case Integer.parse(b) do
              {n, _} -> n
              _ -> 0
            end

          _ ->
            0
        end

      list =
        senses
        |> Safe.ensure_list()
        |> Enum.reject(&is_nil/1)
        |> Enum.map(fn s ->
          s
          |> Safe.to_plain()
          |> Map.put_new(:token_index, idx)
        end)

      {idx, list}
    end)
  end

  defp flatten_candidate_groups(groups) when is_map(groups) do
    groups
    |> Enum.flat_map(fn {tidx, senses} ->
      idx =
        case tidx do
          i when is_integer(i) ->
            i

          b when is_binary(b) ->
            case Integer.parse(b) do
              {n, _} -> n
              _ -> 0
            end

          _ ->
            0
        end

      senses
      |> Safe.ensure_list()
      |> Enum.reject(&is_nil/1)
      |> Enum.map(fn s ->
        s
        |> Safe.to_plain()
        |> Map.put_new(:token_index, idx)
      end)
    end)
  end

  defp winners_to_candidates(winners) when is_list(winners) do
    winners
    |> Enum.with_index()
    |> Enum.map(fn {w, idx} ->
      w = Safe.to_plain(w)

      id =
        Safe.get(w, :id) ||
          Safe.get(w, :chosen_id) ||
          Safe.get(w, :lemma) ||
          Safe.get(w, :word)

      %{token_index: Safe.get(w, :token_index, idx), id: id && to_string(id)}
    end)
    |> Enum.reject(fn c -> is_nil(c.id) or c.id == "" end)
  end

  defp candidates_from_active_cells(ac) when is_list(ac) do
    ac
    |> Enum.flat_map(fn cell ->
      cell = Safe.to_plain(cell)
      idx = Safe.get(cell, :token_index, 0)
      scores = Safe.get(cell, :scores)

      cond do
        is_map(scores) ->
          for {id, score} <- scores do
            %{token_index: idx, id: to_string(id), score: (score || 0.0) * 1.0}
          end

        id = Safe.get(cell, :chosen_id) ->
          [%{token_index: idx, id: to_string(id), score: 1.0}]

        id = Safe.get(cell, :id) ->
          s = Safe.get(cell, :score, 0.0)
          [%{token_index: idx, id: to_string(id), score: s * 1.0}]

        lemma = Safe.get(cell, :lemma) || Safe.get(cell, :word) ->
          s = Safe.get(cell, :score, 0.0)
          [%{token_index: idx, id: to_string(lemma), score: s * 1.0}]

        true ->
          []
      end
    end)
  end
end
