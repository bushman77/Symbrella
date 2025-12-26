defmodule Brain.LIFG.Choices do
  @moduledoc """
  Utilities for normalizing and augmenting Stage-1 choices.

  Responsibilities
  • Ensure each choice has: `:chosen_id`, `:alt_ids`, `:slate_alt_ids`, and a sane `:margin`.
  • `:alt_ids` are strictly the *scored competitors* (keys from the scoring map),
    excluding the chosen id.
    - When `:scores` is absent/empty (e.g. `scores: :none`), we treat `:probs` as the scoring map
      for enumerating scored ids.
  • `:slate_alt_ids` are slate-only candidates (present in SI slate, absent from scoring map).
  • Apply `min_margin` floor when scoring is singleton-like (score-map has < 2 entries).
    (If the scoring map is empty, margin stays 0.0 — we do not invent confidence.)
  """

  alias Brain.Utils.Safe

  @type choice :: map()
  @type si :: map()

  # apps/brain/lib/brain/lifg/choices.ex

  @spec augment([choice], si, number()) :: [choice]
  def augment(raw_choices, si_after, min_margin) when is_list(raw_choices) and is_map(si_after) do
    slate0 =
      Safe.get(si_after, :sense_candidates, %{}) ||
        Safe.get(si_after, :candidates_by_token, %{}) || %{}

    slate = if is_map(slate0), do: slate0, else: %{}

    Enum.map(raw_choices, fn ch0 ->
      ch = Safe.to_plain(ch0)
      idx = normalize_idx(Safe.get(ch, :token_index, 0))

      scores0 = Safe.get(ch, :scores, %{}) || %{}
      scores = score_map(scores0)

      probs0 = Safe.get(ch, :probs, %{}) || %{}
      probs = score_map(probs0)

      score_map_for_ids =
        cond do
          map_size(scores) > 0 -> scores
          map_size(probs) > 0 -> probs
          true -> %{}
        end

      chosen_id =
        case Safe.get(ch, :chosen_id) do
          nil ->
            case Enum.max_by(score_map_for_ids, fn {_id, s} -> num(s) end, fn -> nil end) do
              {id, _} -> id
              _ -> Safe.get(ch, :id) || Safe.get(ch, :winner_id)
            end

          x ->
            x
        end

      chosen_id_s = normalize_id(chosen_id)

      scored_ids =
        score_map_for_ids
        |> Map.keys()
        |> Enum.map(&normalize_id/1)
        |> Enum.reject(&is_nil/1)
        |> Enum.uniq()

      alt_ids =
        case chosen_id_s do
          nil -> scored_ids
          _ -> scored_ids -- [chosen_id_s]
        end

      slate_ids_all =
        case bucket_for(slate, idx) do
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

      margin0 =
        case Safe.get(ch, :margin) do
          m when is_number(m) and m > 0.0 ->
            m * 1.0

          _ ->
            vals =
              score_map_for_ids
              |> Map.values()
              |> Enum.map(&num/1)
              |> Enum.sort(:desc)

            case vals do
              [a, b | _] -> a - b
              _ -> 0.0
            end
        end

      score_count = map_size(score_map_for_ids)

      margin =
        cond do
          score_count == 0 -> 0.0
          score_count < 2 -> max(margin0, min_margin * 1.0)
          true -> max(margin0, 0.0)
        end
        |> Float.round(6)

      ch
      |> Map.put(:token_index, idx)
      |> Map.put(:chosen_id, chosen_id_s)
      |> Map.put(:alt_ids, alt_ids)
      |> Map.put(:slate_alt_ids, slate_alt_ids)
      |> Map.put(:margin, margin)
    end)
  end

  defp bucket_for(slate, idx) when is_map(slate) do
    cond do
      Map.has_key?(slate, idx) ->
        Map.get(slate, idx)

      is_integer(idx) and Map.has_key?(slate, Integer.to_string(idx)) ->
        Map.get(slate, Integer.to_string(idx))

      is_binary(idx) ->
        case Integer.parse(idx) do
          {n, _} -> Map.get(slate, n) || Map.get(slate, idx)
          _ -> Map.get(slate, idx)
        end

      true ->
        nil
    end
  end

  defp bucket_for(_, _), do: nil

  defp normalize_idx(i) when is_integer(i) and i >= 0, do: i
  defp normalize_idx(f) when is_float(f) and f >= 0.0, do: trunc(f)

  defp normalize_idx(b) when is_binary(b) do
    case Integer.parse(b) do
      {n, _} when n >= 0 -> n
      _ -> 0
    end
  end

  defp normalize_idx(_), do: 0

  defp normalize_id(nil), do: nil

  defp normalize_id(id) do
    s = id |> to_string() |> String.trim()
    if s == "", do: nil, else: s
  end

  defp num(v) when is_integer(v), do: v * 1.0
  defp num(v) when is_float(v), do: v

  defp num(v) when is_binary(v) do
    case Float.parse(v) do
      {f, ""} -> f
      _ -> 0.0
    end
  end

  defp num(_), do: 0.0
  defp score_map(m) when is_map(m), do: m

  defp score_map(list) when is_list(list) do
    cond do
      Keyword.keyword?(list) ->
        Enum.into(list, %{})

      Enum.all?(list, &match?({_, _}, &1)) ->
        Enum.into(list, %{})

      true ->
        %{}
    end
  rescue
    _ -> %{}
  end

  defp score_map(_), do: %{}
end
