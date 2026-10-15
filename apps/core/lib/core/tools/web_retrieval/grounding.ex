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
    case supported_identity(context) do
      %{title: title, description: description, sources: [%{url: url}]}
      when is_binary(title) and is_binary(description) and is_binary(url) ->
        "Wikidata describes #{title} as “#{description}”. Source: #{url}"

      _ ->
        align_distance_or_population(draft, context)
    end
  end

  def align(draft, _), do: draft

  defp align_distance_or_population(draft, context) do
    case supported_proximity(context) do
      %{distance_km: km, subject: subject, target: target, sources: sources}
      when is_number(km) and is_binary(subject) and is_binary(target) ->
        citations = Enum.map_join(sources, " and ", &Map.get(&1, :url, ""))
        "Based on Wikidata coordinates, #{subject} and #{target} are approximately " <>
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
          date_note = if is_integer(year), do: " (#{year} census)", else: ""
          "According to Wikipedia, #{subject} had a population of #{display}#{date_note}. Source: #{source}"
        else
          draft
        end

      _ -> draft
    end
  end

  @spec available_support?(map()) :: boolean()
  def available_support?(context) when is_map(context),
    do: is_map(supported_population(context)) or is_map(supported_proximity(context)) or
      is_map(supported_identity(context))

  def available_support?(_), do: false

  @spec supports?(String.t(), map()) :: boolean()
  def supports?(text, context) when is_binary(text) and is_map(context) do
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

  def supports?(_, _), do: false

  defp supports_population?(text, context) do
    case supported_population(context) do
      %{display: display, value: value, source_url: source_url} ->
        (String.contains?(text, display) or String.contains?(text, Integer.to_string(value))) and
          String.contains?(text, source_url)

      _ -> false
    end
  end

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
         is_map(support) and Map.get(support, :status) == :source_supported,
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
