defmodule Core.Tools.WebRetrieval.Grounding do
  @moduledoc """
  Narrow fact extraction and response guard for a cited population figure.
  A sourced extraction is not the same as independent fact verification.

  This module deliberately refuses to verify arbitrary prose or proximity
  measurements by looking for loosely matching numerals.
  """

  # Wikipedia's article lead commonly says "population in 2021 was 90,990".
  # Capture the year and the number together so a census year from unrelated
  # preceding text cannot accidentally be attributed to the population.
  @population ~r/\bpopulation\s+(?:in\s+(?<year>(?:19|20)\d{2})\s+)?(?:of|was|is)\s+(?<display>\d{1,3}(?:,\d{3})+|\d{3,8})\b/iu

  @spec extract_support(list(), atom() | nil, String.t() | nil) :: map() | nil
  def extract_support(sources, :population, subject) when is_list(sources) and is_binary(subject) do
    Enum.find_value(sources, fn
      %{provider: :wikipedia, title: title, url: url, excerpt: excerpt}
      when is_binary(title) and is_binary(url) and is_binary(excerpt) ->
        if matching_place?(title, subject) do
          case Regex.named_captures(@population, excerpt) do
            %{"display" => display} = captures ->
              with {value, ""} <- Integer.parse(String.replace(display, ",", "")),
                   true <- value > 0 do
                [matched | _] = Regex.run(@population, excerpt)
                before = excerpt |> String.split(matched, parts: 2) |> hd()
                preamble = String.slice(before, max(0, String.length(before) - 180), 180)

                year =
                  case Map.get(captures, "year") do
                    year when is_binary(year) and byte_size(year) == 4 ->
                      String.to_integer(year)

                    _ ->
                      case Regex.run(~r/\b((?:19|20)\d{2})\s+[Cc]ensus\b/u, preamble) do
                        [_, parsed] -> String.to_integer(parsed)
                        _ -> nil
                      end
                  end

                %{
                  kind: :population,
                  value: value,
                  display: display,
                  census_year: year,
                  source_title: title,
                  source_url: url,
                  status: :source_supported
                }
              else
                _ -> nil
              end

            _ ->
              nil
          end
        end

      _ ->
        nil
    end)
  end

  def extract_support(_, _, _), do: nil

  defp matching_place?(title, subject) do
    place = subject |> String.split(",") |> hd() |> String.downcase() |> String.trim()
    String.length(place) >= 3 and String.contains?(String.downcase(title), place)
  end

  @spec align(String.t(), map()) :: String.t()
  def align(draft, context) when is_binary(draft) and is_map(context) do
    cond do
      is_binary(population_comparison_answer(context)) ->
        population_comparison_answer(context)

      is_binary(population_group_answer(context)) ->
        population_group_answer(context)

      true ->
        align_single(draft, context)
    end
  end

  def align(draft, _), do: draft

  defp align_single(draft, context) do
    case supported_identity(context) do
      %{title: title, description: description, sources: [%{url: url}]}
      when is_binary(title) and is_binary(description) and is_binary(url) ->
        "Wikidata describes #{title} as “#{description}”. Source: #{url}"

      _ ->
        align_distance_or_population(draft, context)
    end
  end

  defp align_distance_or_population(draft, context) do
    case supported_proximity(context) do
      %{distance_km: km, subject: subject, target: target, sources: sources}
      when is_number(km) and is_binary(subject) and is_binary(target) ->
        citations = Enum.map_join(sources, " and ", &Map.get(&1, :url, ""))
        origin =
          if Enum.all?(sources, &(Map.get(&1, :provider) == :natural_resources_canada)),
            do: "Natural Resources Canada official place-name coordinates",
            else: "Wikidata coordinates"

        "Based on #{origin}, #{subject} and #{target} are approximately " <>
          "#{km} km apart in a straight line (not driving distance). Sources: #{citations}"

      _ ->
        align_population(draft, context)
    end
  end

  defp align_population(draft, context) do
    case supported_population(context) do
      %{display: display, source_url: source, census_year: year} ->
        frame = geographic_frame(context)
        subject = Map.get(frame, :subject)

        if is_binary(subject) do
          case Map.get(supported_population(context), :source_type) do
            :statistics_canada ->
              "Statistics Canada estimates #{subject}'s population at #{display} " <>
                "as of July 1, #{year}. This is an official annual estimate, " <>
                "not a census count. Source: #{source}"

            :wikidata ->
              "Wikidata lists a dated population statement for #{subject} of #{display} " <>
                "(#{year}). This is not necessarily the current population estimate. Source: #{source}"

            _ ->
              date_note = if is_integer(year), do: " (#{year} census)", else: ""
              "According to Wikipedia, #{subject} had a population of #{display}#{date_note}. Source: #{source}"
          end
        else
          draft
        end

      _ -> draft
    end
  end

  @spec available_support?(map()) :: boolean()
  def available_support?(context) when is_map(context),
    do: is_map(supported_population_comparison(context)) or
      is_map(supported_population_group(context)) or
      is_map(supported_population(context)) or is_map(supported_proximity(context)) or
      is_map(supported_identity(context))

  def available_support?(_), do: false

  @spec supports?(String.t(), map()) :: boolean()
  def supports?(text, context) when is_binary(text) and is_map(context) do
    cond do
      map_size(authorized_comparison_answers(context)) > 0 ->
        text in Map.values(authorized_comparison_answers(context))

      is_binary(population_group_answer(context)) ->
        text == population_group_answer(context)

      true ->
        supports_single?(text, context)
    end
  end

  def supports?(_, _), do: false

  defp supports_single?(text, context) do
    case supported_identity(context) do
      %{status: :source_supported} ->
        text == align(text, context)

      _ ->
        supports_distance_or_population?(text, context)
    end
  end

  defp supports_distance_or_population?(text, context) do
    case supported_proximity(context) do
      %{distance_km: km, sources: sources} when is_number(km) and is_list(sources) ->
        String.contains?(text, "#{km}") and
          Enum.all?(sources, fn src -> String.contains?(text, Map.get(src, :url, "")) end) and
          String.contains?(String.downcase(text), "straight line")

      _ ->
        supports_population?(text, context)
    end
  end

  defp supports_population?(text, context) do
    case supported_population(context) do
      %{value: count, census_year: year, source_url: url}
      when is_integer(count) and is_integer(year) and is_binary(url) ->
        subject = Map.get(geographic_frame(context), :subject)

        text == align(text, context) or
          supported_population_paraphrase?(text, subject, count, year, url)

      _ ->
        false
    end
  end

  # Bounded factual entailment for a SINGLE dated population claim. Do not
  # compare the whole LLM response to a template: different phrasing is fine
  # when the exact subject, figure, year and source are preserved. Reject
  # competing numerals and additional geographic facts this support cannot
  # validate. This is intentionally conservative, not general NLI.
  defp supported_population_paraphrase?(text, subject, count, year, url)
       when is_binary(text) and is_binary(subject) do
    place = subject |> String.split(",", parts: 2) |> hd() |> String.trim()
    stripped = String.replace(text, url, "")
    tokens =
      Regex.scan(~r/(?<![\p{L}\p{N}])\d[\d,]*(?![\p{L}\p{N}])/u, stripped)
      |> List.flatten()
      |> Enum.map(fn token ->
        token |> String.replace(",", "") |> String.to_integer()
      end)

    city? =
      String.length(place) >= 3 and
        Regex.match?(Regex.compile!("(?<![\\p{L}])" <> Regex.escape(place) <>
          "(?![\\p{L}])", "iu"), stripped)

    claims_population? =
      Regex.match?(~r/\b(?:population|residents?|people|inhabitants|census|population\s+figure)\b/iu,
        stripped)

    exact_figure? = Enum.member?(tokens, count) and Enum.member?(tokens, year)
    no_other_numbers? = Enum.all?(tokens, &(&1 in [count, year]))

    extraneous_claim? =
      Regex.match?(
        ~r/\b(?:valley|located|northwest|northeast|southwest|southeast|distance|km|kilomet(?:e|re)rs?|miles?|founded|area|population\s+growth|grew\s+by|increased\s+by|decreased\s+by)\b/iu,
        stripped
      )

    denial? =
      Regex.match?(~r/\b(?:did\s+not\s+have|does\s+not\s+have|was\s+not|is\s+not)\s+(?:about\s+|approximately\s+)?(?:the\s+)?\d/iu,
        stripped)

    other_url? = Regex.match?(~r/https?:\/\//iu, stripped)
    # A historical count cannot be silently phrased as today's population.
    current_claim? =
      Regex.match?(~r/\b(?:currently|today|right\s+now|at\s+present|this\s+year|nearby|close\s+to|located\s+in)\b/iu,
        stripped)

    city? and claims_population? and String.contains?(text, url) and
      exact_figure? and no_other_numbers? and
      not extraneous_claim? and not denial? and not other_url? and
      not current_claim? and
      String.length(text) <= 450
  end

  defp supported_population_paraphrase?(_, _, _, _, _), do: false

  # A comparison is computed, not extracted from the model's prose. Validate
  # the original two dated claims and recompute the ordering before allowing
  # the derived relation to appear in the final response.
  @doc """
  Two Core-authored, independently verified surface forms for a population
  comparison. Models may select a presentation, never supply the conclusion,
  quantities, dates, place names, or citations.
  """
  @spec authorized_comparison_answers(map()) :: map()
  def authorized_comparison_answers(context) when is_map(context) do
    case supported_population_comparison(context) do
      %{claims: [a, b], year: year, outcome: outcome, difference: difference} ->
        first = "#{a.subject}: #{format_population(a.value)} (#{year_label(year, a)})"
        second = "#{b.subject}: #{format_population(b.value)} (#{year_label(year, b)})"

        conclusion =
          if outcome == :equal do
            "Both places have the same recorded population for that year."
          else
            winner = if a.value > b.value, do: a.subject, else: b.subject
            "#{winner} has the larger recorded population by " <>
              "#{format_population(difference)}."
          end

        detailed =
          "Comparing sourced population statements:\n" <>
            first <> "\n" <> second <> "\n" <> conclusion <>
            "\nSources: #{a.source_url} and #{b.source_url}. " <>
            provenance_note(a, b)

        short_conclusion =
          if outcome == :equal do
            "In #{year}, #{a.subject} and #{b.subject} each recorded " <>
              "#{format_population(a.value)} residents."
          else
            winner = if a.value > b.value, do: a, else: b
            loser = if a.value > b.value, do: b, else: a
            "#{winner.subject} had the larger recorded population in #{year}: " <>
              "#{format_population(winner.value)} versus " <>
              "#{format_population(loser.value)} in #{loser.subject}, " <>
              "a difference of #{format_population(difference)}."
          end

        brief =
          short_conclusion <> " Sources: #{a.source_url} and #{b.source_url}. " <>
            provenance_note(a, b)

        %{detailed: detailed, brief: brief}

      _ ->
        %{}
    end
  end

  def authorized_comparison_answers(_), do: %{}

  defp population_comparison_answer(context),
    do: Map.get(authorized_comparison_answers(context), :detailed)

  defp supported_population_comparison(context) do
    frame = geographic_frame(context)
    evidence = Map.get(context, :web_evidence) || %{}
    support = if is_map(evidence), do: Map.get(evidence, :support), else: nil

    with %{type: :location_question, relation: :population,
           comparison: :greater, subjects: names} <- frame,
         true <- is_list(names) and length(names) == 2,
         %{kind: :population_comparison, status: :source_supported,
           metric: :population, comparator: :greater_than,
           claims: [a, b], year: year, outcome: outcome,
           difference: difference, winner: winner, loser: loser} <- support,
         true <- Map.get(evidence, :status) == :retrieved,
         true <- Enum.all?([a, b], &valid_comparison_claim?/1),
         true <- Enum.map([a, b], & &1.subject) == names,
         true <- a.census_year == b.census_year and a.census_year == year,
         true <- Map.get(a, :measurement_type) == Map.get(b, :measurement_type),
         true <- compatible_source_claims?(a, b),
         true <- difference == abs(a.value - b.value),
         true <- valid_comparison_outcome?(a, b, outcome, winner, loser) do
      support
    else
      _ -> nil
    end
  end

  defp compatible_source_claims?(%{measurement_type: :official_estimate} = a,
                                         %{measurement_type: :official_estimate} = b) do
    Map.get(a, :source_type) == Map.get(b, :source_type) and
      Map.get(a, :source_table) == Map.get(b, :source_table)
  end
  defp compatible_source_claims?(_, _), do: true

  defp valid_comparison_outcome?(a, b, outcome, winner, loser) do
    cond do
      a.value == b.value ->
        outcome == :equal and is_nil(winner) and is_nil(loser)

      a.value > b.value ->
        outcome == :greater and winner == a.subject and loser == b.subject

      true ->
        outcome == :greater and winner == b.subject and loser == a.subject
    end
  end

  defp valid_comparison_claim?(%{
         subject: subject, value: value, census_year: year, source_url: url
       })
       when is_binary(subject) and is_integer(value) and value > 0 and
            is_integer(year) and year >= 1900 and is_binary(url),
       do: String.starts_with?(url, "https://")

  defp valid_comparison_claim?(_), do: false

  # Multi-subject population grounding is deliberately claim-by-claim.
  # If any figure, date or source is malformed, only that city's result is
  # treated as unavailable. No derived population or shared year is inferred.
  defp population_group_answer(context) do
    case supported_population_group(context) do
      %{results: results} ->
        lines =
          Enum.map(results, fn
            %{subject: subject, status: :source_supported,
              value: value, census_year: year, source_url: url} = entry ->
              "#{subject}: #{format_population(value)} (#{year_label(year, entry)}). Source: #{url}"

            %{subject: subject} ->
              "#{subject}: no verified dated population figure was available."
          end)

        warning =
          if Map.get(geographic_frame(context), :comparison) == :greater do
            reason =
              case get_in(context, [:web_evidence, :comparison_status]) do
                :different_reporting_years ->
                  " The reporting years differ, so a like-for-like ranking is not established."
                _ ->
                  " The available verified figures do not establish a comparison for every place."
              end

            reason
          else
            ""
          end

        "Population figures for the places you asked about:\n" <>
          Enum.join(lines, "\n") <>
          "\nThese are dated figures, not necessarily current population estimates." <>
          warning

      _ ->
        nil
    end
  end

  defp year_label(year, %{measurement_type: :official_estimate}),
    do: "#{year} official estimate"
  defp year_label(year, _), do: "#{year}"

  defp provenance_note(%{measurement_type: :official_estimate},
                       %{measurement_type: :official_estimate}),
    do: "Both figures are official annual population estimates, not census counts."
  defp provenance_note(_, _),
    do: "These are dated figures, not necessarily current estimates."

  defp format_population(value) when is_integer(value) do
    value
    |> Integer.to_string()
    |> String.reverse()
    |> String.graphemes()
    |> Enum.chunk_every(3)
    |> Enum.map_join(",", &Enum.join/1)
    |> String.reverse()
  end

  defp supported_population_group(context) do
    frame = geographic_frame(context)
    evidence = Map.get(context, :web_evidence) || %{}
    support = if is_map(evidence), do: Map.get(evidence, :support), else: nil
    subjects = if is_map(frame), do: Map.get(frame, :subjects), else: nil

    with :population <- Map.get(frame, :relation),
         :location_question <- Map.get(frame, :type),
         names when is_list(names) <- subjects,
         true <- length(names) >= 2 and length(names) <= 4,
         %{kind: :population_group, subjects: ^names, results: results} <- support,
         true <- is_list(results) and length(results) == length(names),
         true <- Enum.map(results, &Map.get(&1, :subject)) == names,
         true <- Enum.all?(results, &valid_population_group_entry?/1) do
      support
    else
      _ -> nil
    end
  end

  defp valid_population_group_entry?(%{subject: subject, status: :unavailable})
       when is_binary(subject), do: true

  defp valid_population_group_entry?(%{
         subject: subject, status: :source_supported, value: value,
         census_year: year, source_url: url
       })
       when is_binary(subject) and is_integer(value) and value > 0 and
              is_integer(year) and is_binary(url),
       do: String.starts_with?(url, "https://")

  defp valid_population_group_entry?(_), do: false

  defp supported_identity(context) do
    frame = geographic_frame(context)
    evidence = Map.get(context, :web_evidence, %{})
    support = Map.get(evidence, :support)

    if Map.get(frame, :type) == :location_question and
         Map.get(frame, :relation) == :identity and
         Map.get(evidence, :status) == :retrieved and
         is_map(support) and Map.get(support, :kind) == :identity and
         Map.get(support, :status) == :source_supported and
         is_binary(Map.get(support, :description)) and
         is_list(Map.get(support, :sources)) and
         length(Map.get(support, :sources)) == 1,
      do: support
  end

  defp supported_proximity(context) do
    frame = geographic_frame(context)
    evidence = Map.get(context, :web_evidence, %{})
    support = Map.get(evidence, :support)

    if Map.get(frame, :type) == :location_question and
         Map.get(frame, :relation) == :proximity and
         Map.get(evidence, :status) == :retrieved and
         is_map(support) and Map.get(support, :status) == :source_supported and
         is_number(Map.get(support, :distance_km)) and
         is_list(Map.get(support, :sources)) and
         length(Map.get(support, :sources)) == 2,
       do: support
  end

  defp supported_population(context) do
    frame = geographic_frame(context)
    evidence = Map.get(context, :web_evidence, %{})
    support = Map.get(evidence, :support)

    if Map.get(frame, :type) == :location_question and
         Map.get(frame, :relation) == :population and
         Map.get(evidence, :status) == :retrieved and
         is_map(support) and Map.get(support, :kind) == :population and
         Map.get(support, :status) == :source_supported and
         is_binary(Map.get(support, :source_url)) and
         is_integer(Map.get(support, :census_year)),
       do: support
  end

  defp geographic_frame(context) do
    case get_in(context, [:features, :symbolic_frame]) do
      %{type: :location_question} = frame -> frame
      _ ->
        case get_in(context, [:symbolic_frame, :event]) do
          %{type: :location_question} = frame -> frame
          _ -> %{}
        end
    end
  end
end
