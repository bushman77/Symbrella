defmodule Brain.Hippocampus.Recall do
  @moduledoc """
  Recall pipeline: resolve `ignore_head`, scope filter, Jaccard×recency scoring,
  duplicate expansion, and top-k selection.
  """

  alias Brain.Hippocampus.{Normalize, Scoring, Dup}

  @type episode :: %{slate: map(), meta: map(), norms: MapSet.t()}
  @type scored  :: %{score: float(), at: non_neg_integer(), episode: episode()}

  @spec run([String.t()] | map(), [{non_neg_integer(), episode()}], keyword()) ::
          {[scored()], map(), map()}
  def run(cues, window, opts) when (is_list(cues) or is_map(cues)) and is_list(window) do
    now         = System.system_time(:millisecond)
    limit       = Keyword.get(opts, :limit, 3)
    half_life   = Keyword.get(opts, :half_life_ms, 300_000)
    min_jacc    = Keyword.get(opts, :min_jaccard, 0.0)
    scope_opt   = Keyword.get(opts, :scope, nil)
    ignore_head = Keyword.get(opts, :ignore_head, :auto)

    cues_set = Normalize.cue_set(cues)

    scan_window =
      case resolve_ignore_head(window, ignore_head, cues_set) do
        {:ok, list} -> list
        _ -> window
      end

    base_scored =
      if MapSet.size(cues_set) == 0 do
        []
      else
        for {at, ep} <- scan_window,
            scope_match?(ep.meta, scope_opt),
            jacc = Scoring.jaccard(cues_set, Normalize.episode_token_set(ep)),
            jacc >= min_jacc,
            age = max(now - at, 0),
            rec = Scoring.recency_factor(age, half_life),
            score = jacc * rec,
            score > 0.0 do
          %{score: score, at: at, episode: ep}
        end
        |> Enum.sort_by(& &1.score, :desc)
      end

    scored =
      base_scored
      |> Enum.flat_map(fn r -> List.duplicate(r, Dup.dup_count(r.episode)) end)
      |> Enum.take(limit)

    meas = %{
      cue_count:   MapSet.size(cues_set),
      window_size: length(window),
      returned:    length(scored),
      top_score:   case scored do [%{score: s} | _] -> s; _ -> 0.0 end
    }

    meta_map = %{limit: limit, half_life_ms: half_life}

    {scored, meas, meta_map}
  end

  @spec resolve_ignore_head([{non_neg_integer(), episode()}], :auto | :always | :never | boolean(), MapSet.t()) ::
          {:ok, [{non_neg_integer(), episode()}]}
  def resolve_ignore_head(window, ignore_opt, cues_set) when is_list(window) do
    case normalize_ignore(ignore_opt) do
      :never ->
        {:ok, window}

      :always ->
        case window do
          [_head | tail] -> {:ok, tail}
          [] -> {:ok, []}
        end

      :auto ->
        case window do
          [head = {_, ep} | tail] ->
            head_set = Map.get(ep, :norms, MapSet.new())
            if MapSet.equal?(head_set, cues_set), do: {:ok, tail}, else: {:ok, [head | tail]}

          [] ->
            {:ok, []}
        end
    end
  end

  # ————— scope helpers —————

  defp scope_match?(meta, scope) when is_map(scope) and map_size(scope) > 0 and is_map(meta) do
    effective =
      case Map.get(meta, :scope) || Map.get(meta, "scope") do
        s when is_map(s) -> Map.merge(meta, s)
        _ -> meta
      end

    Enum.all?(scope, fn {k, v} -> fetch_meta(effective, k) == v end)
  end

  defp scope_match?(_meta, _scope), do: true

  defp fetch_meta(meta, key) when is_map(meta) do
    cond do
      is_atom(key) -> Map.get(meta, key) || Map.get(meta, Atom.to_string(key))
      is_binary(key) ->
        Map.get(meta, key) ||
          case Enum.find(meta, fn
                 {mk, _} when is_atom(mk) -> Atom.to_string(mk) == key
                 _ -> false
               end) do
            {_, v} -> v
            _ -> nil
          end

      true ->
        nil
    end
  end

  # ————— options —————

  defp normalize_ignore(true),      do: :always
  defp normalize_ignore(false),     do: :never
  defp normalize_ignore(:always),   do: :always
  defp normalize_ignore(:never),    do: :never
  defp normalize_ignore(:auto),     do: :auto
  defp normalize_ignore(_unknown),  do: :auto
end

