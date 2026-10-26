defmodule Core.Tools.WebRetrieval.PlaceLocation do
  @moduledoc """
  Source-attributed answer extraction for static public-place identity questions.

  A search hit alone is not a verified geographic fact. This helper requires
  an exact article identity and a short affirmative location assertion from
  that article's lead. It never extracts populations, distances, or rankings.
  Other reference topics remain subject to the normal evidence policy.
  """

  @region_aliases %{
    "ab" => "alberta", "bc" => "british columbia", "mb" => "manitoba",
    "nb" => "new brunswick", "nl" => "newfoundland and labrador",
    "ns" => "nova scotia", "nt" => "northwest territories",
    "nu" => "nunavut", "on" => "ontario", "pe" => "prince edward island",
    "qc" => "quebec", "sk" => "saskatchewan", "yt" => "yukon"
  }

  defp regex_question, do: ~r/^\s*where\s+is\s+(.+?)\s*[?!.]*\s*$/iu

  @spec answer(map()) :: {:ok, String.t()} | :no_support
  def answer(%{
        user_text: question,
        web_evidence: %{
          status: :retrieved, resolution: :reference_sources,
          request_type: :general_search, query: query, sources: sources
        }
      })
      when is_binary(question) and is_binary(query) and is_list(sources) do
    with [_, raw_place] <- Regex.run(regex_question(), question),
         place when is_binary(place) <- String.trim(raw_place),
         true <- normalize(expand_region(place)) == normalize(expand_region(query)) do
      Enum.find_value(sources, :no_support, fn source ->
        with true <- valid_source?(source, query),
             {:ok, lead} <- location_lead(source) do
          {:ok, "According to Wikipedia, " <> lead <> " Source: " <> source.url}
        else
          _ -> nil
        end
      end) || :no_support
    else
      _ -> :no_support
    end
  end

  def answer(_), do: :no_support

  defp valid_source?(
         %{provider: :wikipedia, title: title, url: url, excerpt: excerpt},
         query
       )
       when is_binary(title) and is_binary(url) and is_binary(excerpt) do
    parsed = URI.parse(url)

    # Wikipedia often names an article "Kamloops" even when the
    # request says "Kamloops, BC". Only accept such an unqualified
    # title when the article lead itself affirms the requested province.
    title_match? =
      normalize(expand_region(query)) == normalize(title) or
        qualified_title_supported_by_lead?(query, title, excerpt)

    article_match? =
      parsed.scheme == "https" and parsed.host == "en.wikipedia.org" and
        is_binary(parsed.path) and String.starts_with?(parsed.path, "/wiki/") and
        normalize(
          parsed.path
          |> String.replace_prefix("/wiki/", "")
          |> String.replace("_", " ")
          |> URI.decode()
        ) == normalize(title)

    title_match? and article_match?
  end

  defp valid_source?(_, _), do: false

  defp qualified_title_supported_by_lead?(query, title, excerpt) do
    expanded = expand_region(query)
    province_names = Map.values(@region_aliases)

    case String.split(expanded, ~r/,\s*/u, parts: 2) do
      [requested_title, region] ->
        normalize(requested_title) == normalize(title) and
          normalize(region) in Enum.map(province_names, &normalize/1) and
          case location_lead(%{title: title, excerpt: excerpt}) do
            {:ok, lead} -> asserted_in_region?(lead, region)
            :no_support -> false
          end

      _ -> false
    end
  end

  # A lead can mention several provinces. Require membership in the
  # province the user asked about, not merely a nearby-province mention.
  # Constrain optional modifiers to geographic terms so "in Alberta near
  # British Columbia" cannot pass as a British Columbia location claim.
  defp asserted_in_region?(lead, region) do
    modifier =
      "(?:(?:south-central|north-central|northeastern|northwestern|" <>
      "southeastern|southwestern|northern|southern|eastern|western|" <>
      "central|interior)\\s+|the\\s+(?:Canadian\\s+)?province\\s+of\\s+)?"

    pattern =
      Regex.compile!(
        "\\b(?:in|within)\\s+" <> modifier <>
          Regex.escape(region) <> "\\b", "iu"
      )

    Regex.match?(pattern, lead)
  end

  defp location_lead(%{title: title, excerpt: excerpt}) do
    place = title |> String.split(",", parts: 2) |> hd() |> String.trim()
    first = excerpt |> String.trim() |> String.split(~r/(?<=[.!?])\s+/u, parts: 2) |> hd()
    first = String.trim(first)

    # A Wikipedia lead may include the name's pronunciation immediately
    # after the title; this is not part of the geographic assertion.
    first =
      Regex.replace(
        Regex.compile!("^" <> Regex.escape(place) <>
          "\\s*\\([^)]{0,100}\\)\\s+", "iu"),
        first, place <> " "
      )

    # Reject unsupported named entities, numeric claims, and descriptions
    # whose lead merely repeats a keyword instead of asserting a location.
    sentence_starts_with_place? =
      Regex.match?(
        Regex.compile!("^" <> Regex.escape(place) <> "\\s+(?:is|lies|sits)\\b", "iu"),
        first
      )

    region_matches? =
      case String.split(title, ",", parts: 2) do
        [_, region] -> String.contains?(normalize(first), normalize(region))
        [_] -> true
      end

    if first != "" and String.length(first) <= 380 and
         sentence_starts_with_place? and
         geographic_relation?(first) and
         not Regex.match?(~r/\b(?:not|never|formerly|fictional|alleged)\b/iu, first) and
         not Regex.match?(~r/\d/u, first) and region_matches? and
         String.ends_with?(first, ".") do
      {:ok, concise_location_clause(first)}
    else
      :no_support
    end
  end

  # The relevant public location clause is enough to answer "where".
  # Trim trailing encyclopedic detail only when the excerpt explicitly
  # states the jurisdiction and country; do not generate new claims.
  defp concise_location_clause(first) do
    case Regex.run(~r/\A(.+?\bCanada)(?:[,;].*)?\.\z/iu, first) do
      [_, clause] -> clause <> "."
      _ -> first
    end
  end

  # "Capital city of [region]" states a jurisdiction. A bare "of" does not:
  # "a city of dreams" or "the centre of tourism" are not location evidence.
  defp geographic_relation?(sentence) do
    Regex.match?(~r/\b(?:in|within|near)\b/iu, sentence) or
      Regex.match?(~r/\bis\s+the\s+capital(?:\s+city)?\s+of\b/iu, sentence)
  end

  defp expand_region(place) do
    Enum.reduce(@region_aliases, String.trim(place), fn {abbreviation, name}, text ->
      Regex.replace(
        Regex.compile!("(?:,\\s*|\\s+)" <> abbreviation <> "\\s*$", "iu"),
        text,
        ", " <> name
      )
    end)
  end

  defp normalize(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}]+/u, " ")
    |> String.trim()
  end
end
