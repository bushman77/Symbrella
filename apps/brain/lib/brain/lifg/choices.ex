defmodule Brain.LIFG.Choices do
  @moduledoc """
  Utilities for normalizing and augmenting Stage-1 choices.

  Responsibilities
  • Ensure each choice has: `:chosen_id`, `:alt_ids`, and a sane `:margin`.
  • `:alt_ids` are strictly the *scored competitors* (keys from the `:scores` map),
    excluding the chosen id.
  • `:slate_alt_ids` are bucket candidates present in the SI slate for that token index
    that were *not scored* in this Stage-1 run (clean separation for auditing/debugging).
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

  Output invariants:
  * `:chosen_id` is always a string (or nil if indeterminate)
  * `:alt_ids` contains only scored competitor ids (strings), never slate-only candidates
  * `:slate_alt_ids` contains only slate-only ids (strings) not present in `:scores`
  * `:margin` is always >= `min_margin`
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
            case Enum.max_by(scores, fn {_id, s} -> s end, fn -> nil end) do
              {id, _} -> id
              _ -> nil
            end

          x ->
            x
        end

      chosen_id_s = normalize_id(chosen_id)

      scored_ids =
        scores
        |> Map.keys()
        |> Enum.map(&normalize_id/1)
        |> Enum.reject(&is_nil/1)
        |> Enum.uniq()

      # Option A: alt_ids are ONLY the scored competitors (exclude chosen).
      alt_ids =
        case chosen_id_s do
          nil -> scored_ids
          _ -> scored_ids -- [chosen_id_s]
        end

      # Slate/bucket candidates: present in slate but NOT scored in this run.
      slate_ids_all =
        case Map.get(slate, idx) do
          list when is_list(list) ->
            list
            |> Enum.map(fn c ->
              c = Safe.to_plain(c)
              Safe.get(c, :id) || Safe.get(c, :lemma) || Safe.get(c, :word)
            end)
            |> Enum.map(&normalize_id/1)
            |> Enum.reject(&is_nil/1)
            |> Enum.uniq()

          _ ->
            []
        end

      slate_alt_ids =
        slate_ids_all
        |> Kernel.--(scored_ids)
        |> then(fn ids -> if chosen_id_s, do: ids -- [chosen_id_s], else: ids end)
        |> Enum.uniq()

      # Margin: use provided positive margin if present; otherwise compute top-2 gap from scores.
      margin0 =
        case Safe.get(ch, :margin) do
          m when is_number(m) and m > 0.0 ->
            m

          _ ->
            vals = scores |> Map.values() |> Enum.sort(:desc)

            case vals do
              [a, b | _] -> a - b
              _ -> 0.0
            end
        end

      margin = Float.round(max(margin0, min_margin), 6)

      ch
      |> Map.put(:chosen_id, chosen_id_s)
      |> Map.put(:alt_ids, alt_ids)
      |> Map.put(:slate_alt_ids, slate_alt_ids)
      |> Map.put(:margin, margin)
    end)
  end

  defp normalize_id(nil), do: nil

  defp normalize_id(id) do
    s = id |> to_string() |> String.trim()
    if s == "", do: nil, else: s
  end
end

