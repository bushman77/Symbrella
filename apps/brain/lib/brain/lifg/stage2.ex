defmodule Brain.LIFG.Stage2 do
  @moduledoc """
  LIFG.Stage2 — commitment arbitration and suppression.

  Stage2 sits between Stage1 (competitive sense selection) and downstream
  integration (WM, ATL, Hippocampus).

  Responsibilities (current):
    • Select a single dominant MWE (if any)
    • Suppress weaker overlapping MWEs and weak unigrams
    • Commit high-confidence winners
    • Defer low-margin winners
    • Emit a clear, auditable Stage2 trace event

  Non-responsibilities (by design):
    • No re-scoring
    • No beam search
    • No PMTG consultation
    • No WM mutation
    • No sense flipping

  This keeps Stage2 principled, deterministic, and low-risk.
  """

  require Logger

  @type si :: map()
  @type choice :: map()

  @type decision ::
          {:commit, choice()}
          | {:defer, choice(), reason :: atom()}
          | {:collapse, mwe_choice :: choice(), suppressed :: [choice()]}

  @type opts :: [
          reanalysis_margin_threshold: float(),
          enable_stage2: boolean()
        ]

  @default_opts [
    reanalysis_margin_threshold: 0.10,
    enable_stage2: false
  ]

  # ───────────────────────── Public API ─────────────────────────

  @doc """
  Run Stage2.

  Contract (tests rely on this):

  * If a Stage1 trace event is present and Stage2 is **not enabled**, return:
    `{:skip, %{si: si, reason: :not_enabled}}`

  Otherwise, Stage2 will:
  * locate the Stage1 event (if present),
  * derive decisions from Stage1 choices,
  * emit a Stage2 trace event,
  * and return `{:ok, %{si: si2, event: event}}`.
  """
  @spec run(si(), opts()) ::
          {:ok, %{si: si(), event: map()}}
          | {:skip, %{si: si(), reason: atom()}}
          | {:error, term()}
  def run(si, opts \\ [])

  def run(%{} = si, opts) when is_list(opts) do
    eff_opts = Keyword.merge(@default_opts, opts)

    trace =
      case Map.get(si, :trace) do
        t when is_list(t) -> t
        _ -> []
      end

    stage1_present? = Enum.any?(trace, fn ev -> is_map(ev) and (ev[:stage] || ev["stage"]) == :lifg_stage1 end)

    if stage1_present? and not Keyword.get(eff_opts, :enable_stage2, false) do
      {:skip, %{si: si, reason: :not_enabled}}
    else
      do_run(si, eff_opts)
    end
  end

  def run(other, _opts), do: {:error, {:bad_si, other}}

  # ───────────────────────── Decision Core ─────────────────────────

  @spec decide([choice()], opts()) :: [decision()]
  def decide(choices, opts) when is_list(choices) and is_list(opts) do
    threshold = Keyword.get(opts, :reanalysis_margin_threshold, 0.10) * 1.0

    {mwes, unigrams} =
      Enum.split_with(choices, fn ch ->
        Map.get(ch, :mw?, false) == true or Map.get(ch, :mw, false) == true
      end)

    dominant_mwe = select_dominant_mwe(mwes)

    {collapse_decisions, suppressed_unigram_idxs} =
      collapse_from_dominant_mwe(dominant_mwe, unigrams, threshold)

    mwe_deferrals =
      mwes
      |> Enum.reject(&(&1 == dominant_mwe))
      |> Enum.map(&{:defer, &1, :suppressed_by_dominant_mwe})

    unigram_decisions =
      unigrams
      |> Enum.reject(fn u ->
        idx = Map.get(u, :token_index, Map.get(u, "token_index", -1))
        MapSet.member?(suppressed_unigram_idxs, idx)
      end)
      |> Enum.map(&commit_or_defer(&1, threshold))

    collapse_decisions ++ mwe_deferrals ++ unigram_decisions
  end

  # ───────────────────────── Runner ─────────────────────────

  defp do_run(%{} = si, opts) do
    trace =
      case Map.get(si, :trace) do
        t when is_list(t) -> t
        _ -> []
      end

    with {:ok, %{event: stage1_event}} <- stage1_snapshot(si) do
      choices0 =
        stage1_event[:choices] ||
          stage1_event["choices"] ||
          Map.get(si, :lifg_choices) ||
          Map.get(si, "lifg_choices") ||
          []

      choices = choices0 |> List.wrap() |> Enum.filter(&is_map/1)

      decisions = decide(choices, opts)
      event = stage2_event(stage1_event, decisions)

      si2 =
        si
        |> Map.put(:trace, [event | trace])

      {:ok, %{si: si2, event: event}}
    else
      {:skip, _} = skip -> skip
      {:error, _} = err -> err
      other -> {:error, {:stage2_unexpected, other}}
    end
  end

  # ───────────────────────── Commitment Logic ─────────────────────────

  defp commit_or_defer(choice, threshold) do
    if weak?(choice, threshold) do
      {:defer, choice, :low_margin}
    else
      {:commit, choice}
    end
  end

  defp weak?(choice, threshold) do
    (Map.get(choice, :margin, Map.get(choice, "margin", 0.0)) * 1.0) < threshold
  end

  # ───────────────────────── Dominant MWE ─────────────────────────

  @spec select_dominant_mwe([choice()]) :: choice() | nil
  defp select_dominant_mwe([]), do: nil

  defp select_dominant_mwe(mwes) do
    mwes
    |> Enum.sort_by(fn mwe ->
      {
        -mwe_span_size(mwe),
        -(Map.get(mwe, :margin, Map.get(mwe, "margin", 0.0)) * 1.0),
        Map.get(mwe, :token_index, Map.get(mwe, "token_index", 0))
      }
    end)
    |> hd()
  end

  defp mwe_span_size(%{span: {s, e}})
       when is_integer(s) and is_integer(e),
       do: max(e - s, 0)

  defp mwe_span_size(%{span: [s, e]})
       when is_integer(s) and is_integer(e),
       do: max(e - s, 0)

  defp mwe_span_size(%{span: %{start: s, stop: e}})
       when is_integer(s) and is_integer(e),
       do: max(e - s, 0)

  defp mwe_span_size(%{n: n}) when is_integer(n), do: max(n, 0)
  defp mwe_span_size(_), do: 0

  # ───────────────────────── Span Suppression ─────────────────────────

  defp collapse_from_dominant_mwe(nil, _unigrams, _threshold),
    do: {[], MapSet.new()}

  defp collapse_from_dominant_mwe(mwe, unigrams, threshold) do
    suppressed =
      Enum.filter(unigrams, fn u ->
        weak?(u, threshold) and covers_span?(mwe, u)
      end)

    dominant_decision =
      case commit_or_defer(mwe, threshold) do
        {:commit, _} = d ->
          if suppressed == [] do
            [d]
          else
            [{:collapse, mwe, suppressed}]
          end

        {:defer, _c, _r} = d ->
          # If dominant MWE is weak, do not “collapse”; just defer it.
          [d]
      end

    idxs =
      suppressed
      |> Enum.map(fn u -> Map.get(u, :token_index, Map.get(u, "token_index", -1)) end)
      |> MapSet.new()

    {dominant_decision, idxs}
  end

  defp covers_span?(%{} = mwe, %{} = u) do
    case {span_tuple(mwe), span_tuple(u)} do
      {{s1, e1}, {s2, e2}}
      when is_integer(s1) and is_integer(e1) and is_integer(s2) and is_integer(e2) ->
        s1 <= s2 and e1 >= e2

      _ ->
        false
    end
  end

  defp covers_span?(_, _), do: false

  defp span_tuple(%{span: {s, e}}) when is_integer(s) and is_integer(e), do: {s, e}
  defp span_tuple(%{span: [s, e]}) when is_integer(s) and is_integer(e), do: {s, e}
  defp span_tuple(%{span: %{start: s, stop: e}}) when is_integer(s) and is_integer(e), do: {s, e}

  defp span_tuple(%{start: s, stop: e}) when is_integer(s) and is_integer(e), do: {s, e}
  defp span_tuple(%{start: s, end: e}) when is_integer(s) and is_integer(e), do: {s, e}
  defp span_tuple(%{start: s, stop: e}) when is_integer(s) and is_integer(e), do: {s, e}
  defp span_tuple(_), do: nil

  # ───────────────────────── Trace Event ─────────────────────────

  defp stage2_event(stage1_event, decisions) do
    {committed, deferred, collapsed} =
      Enum.reduce(decisions, {[], [], []}, fn
        {:commit, c}, {cm, df, cl} ->
          {[choice_id(c) | cm], df, cl}

        {:defer, c, _reason}, {cm, df, cl} ->
          {cm, [choice_id(c) | df], cl}

        {:collapse, mwe, suppressed}, {cm, df, cl} ->
          {
            [choice_id(mwe) | cm],
            df,
            [{choice_id(mwe), Enum.map(suppressed, &choice_id/1)} | cl]
          }
      end)

    %{
      stage: :lifg_stage2,
      source_stage1: stage1_event[:stage] || stage1_event["stage"] || :lifg_stage1,
      committed: Enum.reverse(committed),
      deferred: Enum.reverse(deferred),
      collapsed: Enum.reverse(collapsed),
      ts_ms: System.system_time(:millisecond)
    }
  end

  defp choice_id(%{} = c) do
    (Map.get(c, :chosen_id) ||
       Map.get(c, "chosen_id") ||
       Map.get(c, :id) ||
       Map.get(c, "id") ||
       Map.get(c, :lemma) ||
       Map.get(c, "lemma") ||
       "unknown")
    |> to_string()
  end

  defp choice_id(_), do: "unknown"

  # ───────────────────────── Stage1 Snapshot ─────────────────────────

  defp stage1_snapshot(%{} = si) do
    case Map.get(si, :trace) do
      trace when is_list(trace) ->
        case Enum.find(trace, fn ev -> is_map(ev) and (ev[:stage] || ev["stage"]) == :lifg_stage1 end) do
          nil -> {:skip, %{si: si, reason: :no_stage1_event}}
          ev -> {:ok, %{si: si, event: ev}}
        end

      _ ->
        {:skip, %{si: si, reason: :no_stage1_event}}
    end
  rescue
    e -> {:error, e}
  end
end

