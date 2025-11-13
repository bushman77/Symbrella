defmodule Brain.LIFG.Choices do
  @moduledoc """
  Utilities for normalizing and augmenting Stage-1 choices.

  Responsibilities
  • Ensure each choice has: `:chosen_id`, `:alt_ids`, and a sane `:margin`.
  • Merge alternative IDs from both the scores map and the slate bucket.
  • Enforce a minimum margin floor (e.g., 0.05) even for singletons.
  """

  alias Brain.Utils.Safe

  @type choice :: map()
  @type si :: map()

  @doc """
  Augment raw Stage-1 choices with stable fields.

  * `raw_choices` — list of maps from Stage-1 (may omit chosen_id/margin/alt_ids)
  * `si_after`    — SI after Stage-1, used to access the slate buckets
  * `min_margin`  — floor for margins when winners are singletons or gaps are tiny
  """
  @spec augment([choice], si, number()) :: [choice]
  def augment(raw_choices, si_after, min_margin) when is_list(raw_choices) and is_map(si_after) do
    slate =
      Safe.get(si_after, :sense_candidates, %{}) ||
        Safe.get(si_after, :candidates_by_token, %{}) || %{}

    Enum.map(raw_choices, fn ch0 ->
      ch = Safe.to_plain(ch0)
      idx = Safe.get(ch, :token_index, 0)
      # Scores may be :none depending on config; default to %{}
      scores = Safe.get(ch, :scores, %{}) || %{}

      # Winner: prefer existing chosen_id, else argmax(scores)
      chosen_id =
        case Safe.get(ch, :chosen_id) do
          nil ->
            case scores |> Enum.max_by(fn {_id, s} -> s end, fn -> nil end) do
              {id, _} -> id
              _ -> nil
            end

          x ->
            x
        end

      chosen_id_s = if is_nil(chosen_id), do: nil, else: to_string(chosen_id)

      # Alternatives from scores map
      alt_from_scores = if map_size(scores) > 0, do: Map.keys(scores), else: []

      # Alternatives from slate bucket (ids/lemma/word)
      slate_alts =
        case Map.get(slate, idx) do
          list when is_list(list) ->
            list
            |> Enum.map(fn c ->
              c = Safe.to_plain(c)
              Safe.get(c, :id) || Safe.get(c, :lemma) || Safe.get(c, :word)
            end)
            |> Enum.reject(&is_nil/1)

          _ ->
            []
        end

      # Merge + clean + exclude chosen
      alt_ids =
        (Safe.get(ch, :alt_ids, []) ++ alt_from_scores ++ slate_alts)
        |> Enum.map(&to_string/1)
        |> Enum.reject(&(&1 in [nil, ""]))
        |> Enum.uniq()
        |> then(fn ids -> if chosen_id_s, do: ids -- [chosen_id_s], else: ids end)

      singleton? = length(alt_ids) == 0

      # Margin: use provided positive margin if present and not singleton;
      # otherwise compute gap from top-2 scores, and always floor at min_margin.
      margin0 =
        case Safe.get(ch, :margin) do
          m when is_number(m) and m > 0.0 and not singleton? ->
            m

          _ ->
            vals = scores |> Map.values() |> Enum.sort(:desc)

            case vals do
              [a, b | _] -> a - b
              [_a] -> 0.0
              _ -> 0.0
            end
        end

      margin = Float.round(max(margin0, min_margin), 6)

      ch
      |> Map.put(:chosen_id, chosen_id_s)
      |> Map.put(:alt_ids, alt_ids)
      |> Map.put(:margin, margin)
    end)
  end
end
