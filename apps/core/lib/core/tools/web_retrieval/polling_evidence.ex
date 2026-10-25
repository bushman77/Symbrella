defmodule Core.Tools.WebRetrieval.PollingEvidence do
  @moduledoc """
  Bounded, source-attributed polling observations.

  A news headline may report a percentage, but it does not provide a verified
  survey record. Structured provider observations retain methodology and
  fieldwork only when explicitly supplied. Comparisons are permitted only for
  matching questions, populations, pollsters and methodologies in distinct,
  non-overlapping fieldwork periods. This module never infers causes.
  """

  @max_observations 8
  @max_sources 3
  @metric_regex ~r/\b(approval|disapproval|favo[u]?rability)\b/iu
  @pct_regex ~r/\b(?:approval|disapproval|favo[u]?rability)\b[^%]{0,85}?\b(\d{1,3}(?:\.\d+)?)\s*%/iu

  @spec from_sources(list(), keyword()) :: map()
  def from_sources(sources, opts \\ []) when is_list(sources) do
    subject = Keyword.get(opts, :subject)
    relevant = sources |> Enum.filter(&relevant?(&1, subject)) |> Enum.take(@max_sources)

    observations =
      relevant
      |> Enum.flat_map(&parse_source(&1, subject))
      |> Enum.take(@max_observations)

    %{
      version: 1,
      kind: :public_polling,
      subject: subject,
      sources: relevant,
      observations: observations,
      comparisons: comparable_changes(observations),
      verified?: false,
      evidence_level: :source_reported,
      limitations: [
        :provider_reports_not_independently_verified,
        :headlines_not_full_poll_methodology,
        :causation_not_established
      ]
    }
  end

  def from_sources(_, _), do: from_sources([])

  @doc "Provide an attributed answer without inventing figures, methodology or causes."
  @spec answer(map()) :: String.t()
  def answer(%{kind: :public_polling} = evidence) do
    observations = Map.get(evidence, :observations, [])
    sources = Map.get(evidence, :sources, [])
    comparisons = Map.get(evidence, :comparisons, [])

    cond do
      sources == [] ->
        "I couldn't find relevant dated polling reporting for that public figure in " <>
          "this lookup. I can't establish a trend or its causes without sourced, " <>
          "comparable survey data."

      true ->
        reported_figures =
          observations
          |> Enum.filter(&(&1.origin == :headline_reported))
          |> Enum.take(2)
          |> Enum.map_join("\n", &headline_observation/1)

        reports =
          sources
          |> Enum.reject(fn source ->
            Enum.any?(observations, fn observation ->
              observation.origin == :headline_reported and
                observation.provenance.url == Map.get(source, :url)
            end)
          end)
          |> Enum.take(2)
          |> Enum.map_join("\n", &reported_headline/1)

        trend =
          case comparisons do
            [latest | _] -> comparison_text(latest)
            [] ->
              "These results do not establish a comparable before-and-after " <>
                "approval trend: matching fieldwork dates, population, " <>
                "pollster, question and methodology are not available."
          end

        [
          "Here is what the dated reporting supports (not independently verified survey data):",
          reported_figures,
          reports,
          trend,
          "The headlines do not establish what caused any change in opinion. " <>
            "That requires separate evidence, not an inference from a poll movement."
        ]
        |> Enum.reject(&(&1 == ""))
        |> Enum.join("\n")
    end
  end

  def answer(_), do: "No attributable polling evidence is available."

  @spec comparable_changes(list(map())) :: list(map())
  def comparable_changes(observations) when is_list(observations) do
    complete =
      observations
      |> Enum.filter(&comparable?/1)
      |> Enum.sort_by(& &1.fieldwork_end)

    for earlier <- complete,
        later <- complete,
        earlier.provenance.url != later.provenance.url,
        earlier.fieldwork_end < later.fieldwork_start,
        same_measure?(earlier, later) do
      %{
        earlier: earlier,
        later: later,
        delta_percentage_points: Float.round(later.value_pct - earlier.value_pct, 2),
        kind: :source_reported_comparison,
        causal_inference?: false
      }
    end
    |> Enum.sort_by(& &1.later.fieldwork_end, :desc)
    |> Enum.take(2)
  end

  def comparable_changes(_), do: []

  defp parse_source(%{title: title, url: url, publisher: publisher, published_at: date} = source, subject)
       when is_binary(title) and is_binary(url) and is_binary(publisher) and is_binary(date) do
    provenance = %{
      title: title,
      url: url,
      publisher: publisher,
      published_at: date
    }

    case Map.get(source, :poll_measurement) do
      %{} = measurement ->
        case structured_measurement(measurement, provenance, subject) do
          nil -> []
          result -> [result]
        end

      _ ->
        case headline_measurement(title, provenance, subject) do
          nil -> []
          result -> [result]
        end
    end
  end

  defp parse_source(_, _), do: []

  defp headline_measurement(title, provenance, subject) do
    with [metric_name] <- Regex.run(@metric_regex, title, capture: :all_but_first),
         [number] <- Regex.run(@pct_regex, title, capture: :all_but_first),
         {value, ""} <- Float.parse(number),
         true <- value >= 0 and value <= 100 do
      %{
        subject: subject,
        metric: metric(metric_name),
        value_pct: value,
        pollster: nil,
        population: nil,
        fieldwork_start: nil,
        fieldwork_end: nil,
        question: nil,
        sample_size: nil,
        methodology: nil,
        origin: :headline_reported,
        provenance: provenance
      }
    else
      _ -> nil
    end
  end

  defp structured_measurement(measurement, provenance, subject) do
    value = Map.get(measurement, :value_pct)
    name = Map.get(measurement, :metric)
    start_date = Map.get(measurement, :fieldwork_start)
    end_date = Map.get(measurement, :fieldwork_end)
    sample = Map.get(measurement, :sample_size)

    with true <- is_number(value) and value >= 0 and value <= 100,
         normalized_metric when not is_nil(normalized_metric) <- metric(name),
         true <- valid_date_range?(start_date, end_date),
         true <- is_nil(sample) or (is_integer(sample) and sample > 0) do
      %{
        subject: subject,
        metric: normalized_metric,
        value_pct: value * 1.0,
        pollster: bounded(measurement[:pollster], 80),
        population: bounded(measurement[:population], 100),
        fieldwork_start: start_date,
        fieldwork_end: end_date,
        question: bounded(measurement[:question], 220),
        sample_size: sample,
        methodology: bounded(measurement[:methodology], 180),
        origin: :provider_structured,
        provenance: provenance
      }
    else
      _ -> nil
    end
  end

  defp comparable?(%{origin: :provider_structured} = x) do
    Enum.all?([:pollster, :population, :question, :methodology,
                :fieldwork_start, :fieldwork_end], fn key ->
      is_binary(Map.get(x, key)) and String.trim(Map.get(x, key)) != ""
    end) and is_integer(x.sample_size) and x.sample_size > 0
  end

  defp comparable?(_), do: false

  defp same_measure?(a, b) do
    a.subject == b.subject and a.metric == b.metric and
      a.pollster == b.pollster and a.population == b.population and
      a.question == b.question and a.methodology == b.methodology
  end

  defp valid_date_range?(nil, nil), do: true

  defp valid_date_range?(start_date, end_date)
       when is_binary(start_date) and is_binary(end_date) do
    with {:ok, start} <- Date.from_iso8601(start_date),
         {:ok, finish} <- Date.from_iso8601(end_date) do
      Date.compare(start, finish) in [:lt, :eq]
    else
      _ -> false
    end
  end

  defp valid_date_range?(_, _), do: false

  defp bounded(text, length) when is_binary(text) do
    text = text |> String.trim() |> String.slice(0, length)
    if text == "", do: nil, else: text
  end

  defp bounded(_, _), do: nil

  defp metric(name) when is_atom(name), do: metric(Atom.to_string(name))
  defp metric(name) when is_binary(name) do
    case String.downcase(name) do
      "approval" -> :approval
      "disapproval" -> :disapproval
      "favorability" -> :favorability
      "favourability" -> :favorability
      _ -> nil
    end
  end
  defp metric(_), do: nil

  defp relevant?(%{title: title} = source, subject) when is_binary(title) do
    valid? =
      is_binary(Map.get(source, :url)) and
        is_binary(Map.get(source, :published_at)) and
        is_binary(Map.get(source, :publisher))

    valid? and mentions_subject?(title, subject)
  end

  defp relevant?(_, _), do: false

  defp mentions_subject?(_title, subject) when subject in [nil, ""], do: true
  defp mentions_subject?(title, subject) when is_binary(subject) do
    # The retrieval query starts with a named public figure. Other news
    # results cannot be treated as evidence about that figure.
    name =
      case Regex.run(~r/^\s*(\p{Lu}[\p{L}'’.-]+(?:\s+\p{Lu}[\p{L}'’.-]+){1,3})\b/u,
             subject, capture: :all_but_first) do
        [person] -> person
        _ -> nil
      end

    if is_binary(name) do
      surname = name |> String.split() |> List.last() |> String.downcase()
      title = String.downcase(title)
      canonical = if String.ends_with?(surname, "s") and
                     not String.ends_with?(surname, "is"),
                   do: String.trim_trailing(surname, "s"), else: surname
      String.contains?(title, surname) or String.contains?(title, canonical)
    else
      true
    end
  end

  defp reported_headline(source) do
    "- #{source.publisher} (#{String.slice(source.published_at, 0, 10)}) " <>
      "reported: #{source.title}. Source: #{source.url}"
  end

  defp headline_observation(x) do
    "- #{x.provenance.publisher} (#{String.slice(x.provenance.published_at, 0, 10)}) " <>
      "reported #{format_pct(x.value_pct)}% #{x.metric} in a headline: " <>
      "#{x.provenance.title}. Source: #{x.provenance.url} " <>
      "(survey population and fieldwork not established here)."
  end

  defp comparison_text(%{earlier: earlier, later: later, delta_percentage_points: delta}) do
    "Two matching #{earlier.pollster} survey reports show #{earlier.metric} " <>
      "of #{format_pct(earlier.value_pct)}% (fieldwork #{earlier.fieldwork_start} " <>
      "to #{earlier.fieldwork_end}) and #{format_pct(later.value_pct)}% " <>
      "(fieldwork #{later.fieldwork_start} to #{later.fieldwork_end}), a change " <>
      "of #{format_pct(delta)} percentage points among #{earlier.population}. " <>
      "Methodology: #{earlier.methodology}; question: #{earlier.question}. " <>
      "Sources: #{earlier.provenance.url} and #{later.provenance.url}. " <>
      "These are attributed measurements, not independent verification."
  end

  defp format_pct(n) when is_number(n) do
    rounded = Float.round(n * 1.0, 2)
    if rounded == trunc(rounded), do: Integer.to_string(trunc(rounded)), else: to_string(rounded)
  end
end
