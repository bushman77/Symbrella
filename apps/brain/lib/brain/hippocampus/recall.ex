defmodule Brain.Hippocampus.Recall do
  @moduledoc """
  Recall pipeline: resolve `ignore_head`, scope filter, Jaccard×recency scoring,
  duplicate expansion, and top-k selection.

  Implementation notes:

    • Uses the episode's `norms` field as the canonical token set.
    • Builds cue sets from simple string / slate fields (lemma/norm/word/id).
    • Jaccard is computed locally via `MapSet` intersection/union.
    • Recency weight is delegated to `Brain.Hippocampus.Scoring.recency_factor/2`.
    • Scope filtering honors simple metadata maps (including nested `:scope` maps).
  """

  alias Brain.Hippocampus.{Scoring, Dup}

  @type episode :: %{slate: map(), meta: map(), norms: MapSet.t()}
  @type scored :: %{score: float(), at: non_neg_integer(), episode: episode()}

  @spec run([String.t()] | map(), [{non_neg_integer(), episode()}], keyword()) ::
          {[scored()], map(), map()}
  def run(cues, window, opts) when (is_list(cues) or is_map(cues)) and is_list(window) do
    # Use the same clock family as `encode/2` (monotonic).
    now        = System.monotonic_time(:millisecond)
    limit      = Keyword.get(opts, :limit, 3)
    half_life  = Keyword.get(opts, :half_life_ms, 300_000)
    min_jacc   = Keyword.get(opts, :min_jaccard, 0.0)
    scope_opt  = Keyword.get(opts, :scope, nil)
    ignore_opt = Keyword.get(opts, :ignore_head, :auto)

    cues_set = cues_to_set(cues)

    scan_window =
      case resolve_ignore_head(window, ignore_opt, cues_set) do
        {:ok, list} -> list
        _ -> window
      end

    base_scored =
      if MapSet.size(cues_set) == 0 do
        []
      else
        for {at, ep} <- scan_window,
            scope_match?(ep.meta, scope_opt),
            ep_set = episode_set(ep),
            jacc = jaccard(cues_set, ep_set),
            jacc >= min_jacc,
            age = max(now - at, 0),
            rec = Scoring.recency_factor(age, half_life),
            score = jacc * rec,
            score > 0.0 do
          %{score: score, at: at, episode: ep}
        end
        # deterministic: sort by score, then by timestamp (newer first on ties)
        |> Enum.sort_by(fn %{score: s, at: at} -> {s, at} end, :desc)
      end

    scored =
      base_scored
      |> Enum.flat_map(fn r -> List.duplicate(r, Dup.dup_count(r.episode)) end)
      |> Enum.take(limit)

    meas = %{
      cue_count: MapSet.size(cues_set),
      window_size: length(window),
      returned: length(scored),
      top_score:
        case scored do
          [%{score: s} | _] -> s
          _ -> 0.0
        end
    }

    meta_map = %{
      limit: limit,
      half_life_ms: half_life,
      min_jaccard: min_jacc,
      ignore_head: ignore_opt,
      scoped?: scope_opt != nil
    }

    {scored, meas, meta_map}
  end

  # ───────────────────── ignore_head resolution ─────────────────────

  @spec resolve_ignore_head(
          [{non_neg_integer(), episode()}],
          :auto | :always | :never | boolean(),
          MapSet.t()
        ) ::
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
            # If the head episode's norm set exactly matches the cue set,
            # treat it as the "current context" and ignore it for recall.
            head_set = episode_set(ep)

            if MapSet.equal?(head_set, cues_set) do
              {:ok, tail}
            else
              {:ok, [head | tail]}
            end

          [] ->
            {:ok, []}
        end
    end
  end

  # ─────────────────────────── cues helpers ─────────────────────────

  defp cues_to_set(cues) when is_list(cues) do
    cues
    |> Enum.flat_map(&tokenize_term/1)
    |> Enum.reject(&(&1 == ""))
    |> MapSet.new()
  end

  defp cues_to_set(%{} = slate) do
    winners = slate[:winners] || slate["winners"] || []

    winners
    |> Enum.flat_map(fn w ->
      [
        w[:lemma], w["lemma"],
        w[:norm],  w["norm"],
        w[:word],  w["word"],
        w[:id],    w["id"]
      ]
    end)
    |> Enum.flat_map(&tokenize_term/1)
    |> Enum.reject(&(&1 == ""))
    |> MapSet.new()
  end

  defp cues_to_set(_), do: MapSet.new()

  defp tokenize_term(nil), do: []

  defp tokenize_term(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.trim()
    |> String.split(~r/\s+/, trim: true)
  end

  defp tokenize_term(v) when is_atom(v), do: v |> Atom.to_string() |> tokenize_term()
  defp tokenize_term(v), do: v |> to_string() |> tokenize_term()

  # ───────────────────── episode token helpers ──────────────────────

  # Prefer the episode's `norms` MapSet if present.
  defp episode_set(%{norms: %MapSet{} = norms}), do: norms

  defp episode_set(%{norms: norms}) when is_list(norms),
    do: MapSet.new(norms)

  defp episode_set(%{slate: slate}) when is_map(slate) do
    winners = slate[:winners] || slate["winners"] || []

    winners
    |> Enum.flat_map(fn w ->
      [
        w[:lemma], w["lemma"],
        w[:norm],  w["norm"],
        w[:word],  w["word"],
        w[:id],    w["id"]
      ]
    end)
    |> Enum.flat_map(&tokenize_term/1)
    |> Enum.reject(&(&1 == ""))
    |> MapSet.new()
  end

  defp episode_set(_), do: MapSet.new()

  defp jaccard(a, b) do
    inter = MapSet.intersection(a, b) |> MapSet.size()
    union = MapSet.union(a, b) |> MapSet.size()

    cond do
      union == 0 -> 0.0
      true -> inter / union
    end
  end

  # ─────────────────────────── scope helpers ────────────────────────

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
      is_atom(key) ->
        Map.get(meta, key) || Map.get(meta, Atom.to_string(key))

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

  # ───────────────────────── option helpers ─────────────────────────

  defp normalize_ignore(true), do: :always
  defp normalize_ignore(false), do: :never
  defp normalize_ignore(:always), do: :always
  defp normalize_ignore(:never), do: :never
  defp normalize_ignore(:auto), do: :auto
  defp normalize_ignore(_unknown), do: :auto
end

