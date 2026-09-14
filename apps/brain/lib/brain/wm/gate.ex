defmodule Brain.WM.Gate do
  @moduledoc """
  Legacy compatibility adapter for LIFG-to-WM gating.

  Runtime WM admission is delegated to `Brain.WM.Admission`; this module only
  translates older SI shapes into candidate evidence.
  """

  alias Brain.WM.Admission

  @spec ingest_from_si(map(), map(), keyword()) :: map()
  def ingest_from_si(state, si, opts \\ [])

  def ingest_from_si(state, si, opts) when is_map(state) and is_map(si) and is_list(opts) do
    allow_fb? =
      Keyword.get(opts, :allow_phrase_fallback?, false) or
        Keyword.get(opts, :allow_fallback?, false)

    min_score =
      opts
      |> Keyword.get(:min_score, 0.0)
      |> as_float()
      |> clamp01()

    candidates =
      case lifg_pairs_to_tuples(si) do
        [] -> winners_to_candidates(si)
        tuples -> pairs_to_candidates(tuples)
      end

    admission_opts =
      opts
      |> Keyword.put(:source, :lifg)
      |> Keyword.put(:lifg_min_score, min_score)
      |> Keyword.put(:allow_fallback_into_wm?, allow_fb?)

    {wm_next, _added, _removed} = Admission.run(state, candidates, admission_opts)

    state
    |> Map.put(:wm, wm_next)
    |> bump_wm_ts()
  end

  def ingest_from_si(state, _si, _opts), do: state

  @spec lifg_pairs_to_tuples(map()) :: list()
  defp lifg_pairs_to_tuples(si) when is_map(si) do
    tokens = (si[:tokens] || si["tokens"] || []) |> List.wrap()

    get_span = fn idx ->
      case Enum.find(tokens, fn t -> (t[:index] || t["index"]) == idx end) do
        %{span: span} -> span
        %{"span" => span} -> span
        _ -> nil
      end
    end

    mwe_span_fallback =
      case Enum.find(tokens, fn t -> t[:mw] == true or t["mw"] == true end) do
        %{span: span} -> span
        %{"span" => span} -> span
        _ -> nil
      end

    (si[:lifg_pairs] || si["lifg_pairs"] || [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{type: :mwe_unigram, mwe_id: mwe, unigram_id: uni, token_index: j} = m ->
        ms = m[:mwe_span] || m["mwe_span"] || mwe_span_fallback
        us = get_span.(j)
        sc = m[:weight] || m["weight"] || 1.0
        if ms && us, do: [{to_string(mwe), to_string(uni), j, sc}], else: []

      tup when is_tuple(tup) and tuple_size(tup) == 5 ->
        {mwe, _ms, uni, _us, sc} = tup
        [{to_string(mwe), to_string(uni), 0, sc}]

      tup when is_tuple(tup) and tuple_size(tup) == 2 ->
        {ida, idb} = tup
        [{to_string(ida), to_string(idb), 0, 0.3}]

      _ ->
        []
    end)
  end

  defp pairs_to_candidates(tuples) do
    Enum.flat_map(tuples, fn {mwe, uni, token_index, score0} ->
      score = clamp01(as_float(score0))

      [
        lifg_candidate(uni, score, token_index),
        lifg_candidate(mwe, score, token_index)
      ]
    end)
  end

  defp winners_to_candidates(si) do
    (si[:lifg_choices] || si["lifg_choices"] || [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{} = winner ->
        id = winner[:chosen_id] || winner["chosen_id"] || winner[:id] || winner["id"]
        score = winner[:score] || winner["score"] || winner[:prob] || winner["prob"] || 1.0

        token_index =
          winner[:token_index] || winner["token_index"] || winner[:index] || winner["index"]

        if is_nil(id) do
          []
        else
          [lifg_candidate(to_string(id), clamp01(as_float(score)), token_index)]
        end

      _ ->
        []
    end)
  end

  defp lifg_candidate(id, score, token_index) do
    %{
      id: id,
      lemma: lemma_from_id(id),
      token_index: token_index,
      score: score,
      source: :lifg
    }
  end

  defp bump_wm_ts(state),
    do: Map.put(state, :wm_last_ms, System.system_time(:millisecond))

  defp lemma_from_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 2) do
      [lemma, _] -> lemma
      _ -> id
    end
  end

  defp as_float(nil), do: 0.0
  defp as_float(value) when is_number(value), do: value * 1.0

  defp as_float(value) when is_binary(value) do
    case Float.parse(String.trim(value)) do
      {parsed, ""} -> parsed
      _ -> 0.0
    end
  end

  defp as_float(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
