defmodule Brain.LIFG.Legacy do
  @moduledoc """
  Legacy view helpers for Stage-1 output.

  Produces the historical `:boosts` / `:inhibitions` payloads that downstream
  WM/consumers expect.

  Rules
  • Boost amount = chosen margin (>= 0)
  • Inhibition amount = max(margin_threshold - score_gap, 0.0)
  • Inhibition targets must cover *all* non-winner candidates for the token:
      - scored competitors (from `:scores` keys)
      - slate-only competitors (from `:slate_alt_ids`)
      - plus `:alt_ids` as a safe fallback
  • When `pairs` mode is active, return `{id, amount}` tuples instead of maps.
    Pairs mode is triggered if any of the following are present in opts:
      - `:gate_into_wm`
      - `:lifg_min_score`
      - `:emit_pairs`
  """

  alias Brain.Utils.Safe

  @type choice :: map()
  @type opts_kw :: keyword()

  @spec boosts_inhibitions([choice], number(), atom(), opts_kw) :: {list(), list()}
  def boosts_inhibitions(choices, margin_thr, _scores_mode, eff_opts) do
    thr = (margin_thr || 0.0) * 1.0

    {boosts_maps, inhibitions_maps} =
      Enum.reduce(choices, {[], []}, fn ch, {boos, inhs} ->
        token_index = Safe.get(ch, :token_index, 0)
        chosen_id0 = Safe.get(ch, :chosen_id, nil)
        chosen_id = normalize_id(chosen_id0)
        margin = (Safe.get(ch, :margin, 0.0) || 0.0) * 1.0

        scores0 = Safe.get(ch, :scores, %{}) || %{}
        scores = normalize_scores_map(scores0)

        top_s =
          if map_size(scores) > 0 and is_binary(chosen_id) do
            Map.get(scores, chosen_id, 0.0) * 1.0
          else
            0.0
          end

        loser_ids =
          ((scores |> Map.keys() |> Enum.reject(&(&1 == chosen_id))) ++
             normalize_ids_list(Safe.get(ch, :alt_ids, [])) ++
             normalize_ids_list(Safe.get(ch, :slate_alt_ids, [])))
          |> Enum.reject(&is_nil/1)
          |> Enum.reject(&(&1 == chosen_id))
          |> Enum.uniq()

        boost_here = %{
          token_index: token_index,
          id: chosen_id0,
          amount: Float.round(max(margin, 0.0), 6)
        }

        inhibitions_here =
          Enum.map(loser_ids, fn lid ->
            gap = max(top_s - Map.get(scores, lid, 0.0), 0.0)
            amt = Float.round(max(thr - gap, 0.0), 6)
            %{token_index: token_index, id: lid, amount: amt}
          end)

        {[boost_here | boos], inhibitions_here ++ inhs}
      end)

    pairs_mode? =
      Keyword.has_key?(eff_opts, :gate_into_wm) or
        Keyword.has_key?(eff_opts, :lifg_min_score) or
        Keyword.get(eff_opts, :emit_pairs, false)

    if pairs_mode? do
      {
        boosts_maps |> Enum.map(&to_pair/1) |> Enum.reject(&is_nil/1) |> Enum.reverse(),
        inhibitions_maps |> Enum.map(&to_pair/1) |> Enum.reject(&is_nil/1) |> Enum.reverse()
      }
    else
      {Enum.reverse(boosts_maps), Enum.reverse(inhibitions_maps)}
    end
  end

  defp normalize_scores_map(m) when is_map(m) do
    Enum.reduce(m, %{}, fn {k, v}, acc ->
      id = normalize_id(k)
      s = if(is_number(v), do: v * 1.0, else: 0.0) |> max(0.0)

      if is_nil(id) do
        acc
      else
        Map.update(acc, id, s, &max(&1, s))
      end
    end)
  end

  defp normalize_scores_map(_), do: %{}

  defp normalize_ids_list(list) do
    list
    |> List.wrap()
    |> Enum.map(&normalize_id/1)
    |> Enum.reject(&is_nil/1)
  end

  defp normalize_id(nil), do: nil

  defp normalize_id(id) do
    s = id |> to_string() |> String.trim()
    if s == "", do: nil, else: s
  end

  defp to_pair(%{id: id, amount: amt}), do: {to_string(id), (amt || 0.0) * 1.0}
  defp to_pair({id, amt}), do: {to_string(id), (amt || 0.0) * 1.0}
  defp to_pair(_), do: nil
end
