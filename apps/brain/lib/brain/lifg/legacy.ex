defmodule Brain.LIFG.Legacy do
  @moduledoc """
  Legacy view helpers for Stage-1 output.

  Produces the historical `:boosts` / `:inhibitions` payloads that downstream
  WM/consumers expect.

  Rules
  • Boost amount = chosen margin (>= 0)
  • Inhibition amount = max(margin_threshold - score_gap, 0.0)
  • When `pairs` mode is active, return `{id, amount}` tuples instead of maps.
    Pairs mode is triggered if any of the following are present in opts:
      - `:gate_into_wm`
      - `:lifg_min_score`
      - `:emit_pairs`
  """

  alias Brain.Utils.Safe

  @type choice :: map()
  @type opts_kw :: keyword()

  @spec boosts_inhibitions([choice], number(), atom(), opts_kw) ::
          {list(), list()}
  def boosts_inhibitions(choices, margin_thr, _scores_mode, eff_opts) do
    {boosts_maps, inhibitions_maps} =
      Enum.reduce(choices, {[], []}, fn ch, {boos, inhs} ->
        token_index = Safe.get(ch, :token_index, 0)
        chosen_id = Safe.get(ch, :chosen_id, nil)
        margin = (Safe.get(ch, :margin, 0.0) || 0.0) * 1.0
        scores = Safe.get(ch, :scores, %{}) || %{}

        top_s =
          if is_map(scores) and map_size(scores) > 0 do
            Map.get(scores, chosen_id, 0.0) * 1.0
          else
            0.0
          end

        loser_ids =
          if is_map(scores) and map_size(scores) > 0 do
            scores |> Map.keys() |> Enum.reject(&(&1 == chosen_id))
          else
            Safe.get(ch, :alt_ids, [])
          end

        boost_here = %{
          token_index: token_index,
          id: chosen_id,
          amount: Float.round(max(margin, 0.0), 6)
        }

        inhibitions_here =
          Enum.map(loser_ids, fn lid ->
            gap = max(top_s - Map.get(scores, lid, 0.0), 0.0)
            amt = Float.round(max(margin_thr - gap, 0.0), 6)
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

  defp to_pair(%{id: id, amount: amt}), do: {to_string(id), (amt || 0.0) * 1.0}
  defp to_pair({id, amt}), do: {to_string(id), (amt || 0.0) * 1.0}
  defp to_pair(_), do: nil
end
