defmodule Core.Comprehension.Summary do
  @moduledoc """
  Builds a compact, prompt-facing summary of what the pipeline understood.

  The summary is deliberately coarser than LIFG choices. It gives downstream
  response planning a stable contract:

  * `:understood` for high-confidence or explicit semantic anchors.
  * `:uncertain` for weak/fallback concepts.
  * `:degraded?` and `:reasons` when the evidence quality is poor.
  """

  @summary_limit 6
  @weak_score 0.5
  @weak_rate_threshold 0.45
  @fallback_rate_threshold 0.35

  @low_info_terms MapSet.new(~w(
    a an the and or to of in on for with at by is are was were be been being
    i you he she it we they me him her us them my your his its our their this
    that these those have has had do does did tell say ask about
  ))

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    summary = build(si, opts)

    si
    |> Map.put(:comprehension, summary)
    |> Core.Pipeline.Trace.append(
      :comprehension,
      decision: if(summary.degraded?, do: :degraded, else: :ok),
      reason: :semantic_summary,
      scores: %{
        confidence: summary.confidence,
        degraded: if(summary.degraded?, do: 1.0, else: 0.0),
        understood: length(summary.understood) / @summary_limit,
        uncertain: length(summary.uncertain) / @summary_limit
      },
      meta: summary
    )
  end

  def attach(si, _opts), do: si

  @spec build(map(), keyword()) :: map()
  def build(si, opts \\ [])

  def build(%{} = si, _opts) do
    intent = Map.get(si, :intent) || Map.get(si, "intent") || :unknown
    confidence = clamp01(Map.get(si, :confidence) || Map.get(si, "confidence") || 0.0)
    keyword = normalize_term(Map.get(si, :keyword) || Map.get(si, "keyword"))
    sentence = normalize_sentence(Map.get(si, :sentence) || Map.get(si, "sentence"))
    choices = List.wrap(Map.get(si, :lifg_choices) || Map.get(si, "lifg_choices") || [])

    {strong, weak, stats} = split_choice_terms(choices)

    understood =
      []
      |> maybe_add_keyword(keyword, intent)
      |> Kernel.++(intent_anchors(intent, sentence))
      |> Kernel.++(strong)
      |> clean_terms()
      |> Enum.take(@summary_limit)

    uncertain =
      weak
      |> clean_terms()
      |> Enum.reject(&(&1 in understood))
      |> Enum.take(@summary_limit)

    reasons = degradation_reasons(stats, choices)
    degraded? = reasons != []

    %{
      version: 1,
      intent: intent,
      confidence: confidence,
      keyword: blank_to_nil(keyword),
      understood: understood,
      uncertain: uncertain,
      degraded?: degraded?,
      reasons: reasons,
      stats: stats
    }
  end

  def build(_si, _opts), do: empty_summary()

  defp split_choice_terms(choices) do
    total = length(choices)

    Enum.reduce(choices, {[], [], %{total: total, weak: 0, fallback: 0}}, fn choice,
                                                                             {strong, weak, stats} ->
      term = choice_term(choice)
      score = choice_score(choice)
      fallback? = fallback_choice?(choice)
      weak? = score < @weak_score or fallback?

      stats =
        stats
        |> Map.update!(:weak, &if(weak?, do: &1 + 1, else: &1))
        |> Map.update!(:fallback, &if(fallback?, do: &1 + 1, else: &1))

      cond do
        term == "" ->
          {strong, weak, stats}

        weak? ->
          {strong, [term | weak], stats}

        true ->
          {[term | strong], weak, stats}
      end
    end)
    |> then(fn {strong, weak, stats} ->
      stats =
        stats
        |> Map.put(:weak_rate, rate(stats.weak, total))
        |> Map.put(:fallback_rate, rate(stats.fallback, total))

      {Enum.reverse(strong), Enum.reverse(weak), stats}
    end)
  end

  defp degradation_reasons(%{total: 0}, _choices), do: [:no_lifg_choices]

  defp degradation_reasons(stats, _choices) do
    []
    |> maybe_reason(stats.fallback_rate >= @fallback_rate_threshold, :fallback_rate_high)
    |> maybe_reason(stats.weak_rate >= @weak_rate_threshold, :weak_decision_rate_high)
  end

  defp maybe_add_keyword(acc, "", _intent), do: acc
  defp maybe_add_keyword(acc, _keyword, :unknown), do: acc
  defp maybe_add_keyword(acc, keyword, _intent), do: acc ++ [keyword]

  defp intent_anchors(:illicit_request, sentence) do
    []
    |> maybe_anchor(acquisition_drug_request?(sentence), "buy drugs")
    |> maybe_anchor(Regex.match?(~r/\bdrugs?\b/u, sentence), "drugs")
    |> maybe_anchor(
      Regex.match?(~r/\b(wasted|high|stoned|intoxicated)\b/u, sentence),
      "intoxication"
    )
  end

  defp intent_anchors(intent, sentence) when intent in [:ask, :question, :command, :explain] do
    []
    |> maybe_anchor(Regex.match?(~r/\bworking memory|wm\b/u, sentence), "working memory")
    |> maybe_anchor(Regex.match?(~r/\blifg\b/u, sentence), "lifg")
    |> maybe_anchor(Regex.match?(~r/\bpmtg\b/u, sentence), "pmtg")
    |> maybe_anchor(Regex.match?(~r/\bhippocampus\b/u, sentence), "hippocampus")
  end

  defp intent_anchors(_intent, _sentence), do: []

  defp maybe_anchor(acc, true, anchor), do: acc ++ [anchor]
  defp maybe_anchor(acc, false, _anchor), do: acc

  defp acquisition_drug_request?(sentence) do
    Regex.match?(~r/\b(buy|get|score|find|order|source)\b.{0,80}\bdrugs?\b/u, sentence)
  end

  defp maybe_reason(reasons, true, reason), do: reasons ++ [reason]
  defp maybe_reason(reasons, false, _reason), do: reasons

  defp choice_term(choice) when is_map(choice) do
    choice
    |> map_get(:lemma)
    |> case do
      nil -> choice |> map_get(:id) |> lemma_from_id()
      term -> term
    end
    |> normalize_term()
  end

  defp choice_term(_), do: ""

  defp choice_score(choice) when is_map(choice) do
    choice
    |> map_get(:score)
    |> case do
      n when is_number(n) -> clamp01(n)
      _ -> 0.0
    end
  end

  defp choice_score(_), do: 0.0

  defp fallback_choice?(choice) when is_map(choice) do
    id = map_get(choice, :id) || map_get(choice, :chosen_id)
    is_binary(id) and String.contains?(id, "|fallback")
  end

  defp fallback_choice?(_), do: false

  defp clean_terms(terms) do
    terms
    |> Enum.map(&normalize_term/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.reject(&low_info_term?/1)
    |> Enum.uniq()
  end

  defp low_info_term?(term) do
    down = String.downcase(term)
    MapSet.member?(@low_info_terms, down)
  end

  defp normalize_term(nil), do: ""

  defp normalize_term(term) when is_binary(term) do
    term
    |> String.downcase()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp normalize_term(term), do: term |> to_string() |> normalize_term()

  defp normalize_sentence(nil), do: ""

  defp normalize_sentence(sentence) do
    sentence
    |> to_string()
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}\s]/u, " ")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp lemma_from_id(id) when is_binary(id) do
    id
    |> String.split("|", parts: 2)
    |> List.first()
  end

  defp lemma_from_id(_), do: nil

  defp map_get(map, key) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key)))
  end

  defp rate(_count, 0), do: 0.0
  defp rate(count, total), do: Float.round(count / total, 6)

  defp blank_to_nil(""), do: nil
  defp blank_to_nil(value), do: value

  defp clamp01(n) when is_number(n), do: n |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp empty_summary do
    %{
      version: 1,
      intent: :unknown,
      confidence: 0.0,
      keyword: nil,
      understood: [],
      uncertain: [],
      degraded?: true,
      reasons: [:invalid_input],
      stats: %{total: 0, weak: 0, fallback: 0, weak_rate: 0.0, fallback_rate: 0.0}
    }
  end
end
