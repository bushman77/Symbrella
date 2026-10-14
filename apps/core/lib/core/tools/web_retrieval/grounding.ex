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

  def align(draft, _), do: draft

  @spec supports?(String.t(), map()) :: boolean()
  def supports?(text, context) when is_binary(text) and is_map(context) do
    case supported_population(context) do
      %{display: display, value: value, source_url: source_url} ->
        (String.contains?(text, display) or String.contains?(text, Integer.to_string(value))) and
          String.contains?(text, source_url)

      _ -> false
    end
  end

  def supports?(_, _), do: false

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
