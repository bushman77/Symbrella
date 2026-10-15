defmodule Core.Tools.WebRetrieval.Wikidata do
  @moduledoc """
  Structured, keyless coordinate lookup via Wikidata's fixed API host.

  Resolves named places through their English Wikipedia sitelinks, then reads
  P625 coordinates. A point is usable only if its label and jurisdiction
  support the requested entity; fuzzy search is never treated as validation.
  Distances are straight-line haversine distances, NOT road distances.
  """

  @api_url "https://www.wikidata.org/w/api.php"
  @user_agent "Symbrella/0.1 (https://github.com/bushman77/Symbrella)"
  @earth_radius_km 6371.0088

  @spec proximity(String.t(), String.t(), keyword()) ::
          {:ok, map()} | {:error, term()}
  def proximity(subject, target, opts \\ [])
  def proximity(subject, target, opts)
      when is_binary(subject) and is_binary(target) do
    # "Vancouver" in "Maple Ridge, BC" normally refers to Vancouver, BC.
    # Do not apply this hint to an explicitly qualified target.
    hint = if Regex.match?(~r/,\s*(?:BC|British Columbia)\s*$/iu, subject),
      do: "British Columbia",
      else: nil

    with {:ok, origin} <- place(subject, nil, opts),
         {:ok, destination} <- place(target, hint, opts) do
      distance = haversine(origin.latitude, origin.longitude,
                           destination.latitude, destination.longitude)

      {:ok,
       %{
         kind: :proximity,
         status: :source_supported,
         method: :haversine,
         distance_km: Float.round(distance, 1),
         subject: subject,
         target: target,
         coordinates: %{
           subject: {origin.latitude, origin.longitude},
           target: {destination.latitude, destination.longitude}
         },
         sources: [Map.take(origin, [:title, :url, :provider]),
                   Map.take(destination, [:title, :url, :provider])]
       }}
    end
  end

  def proximity(_, _, _), do: {:error, :invalid_places}

  @doc """
  Latest dated Wikidata P1082 population statement for a verified city.

  The year is the statement's point-in-time qualifier, not a claim that this
  is a current-year census or the newest published government estimate.
  Refuse population values without a dated qualifier.
  """
  @spec population(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def population(name, opts \\ []) when is_binary(name) do
    with {:ok, place} <- place(name, nil, opts),
         statement when is_map(statement) <-
           latest_dated_population(Map.get(place, :population_claims, [])) do
      {:ok,
       %{
         kind: :population,
         status: :source_supported,
         method: :wikidata_dated_statement,
         subject: name,
         value: statement.value,
         display: Integer.to_string(statement.value) |> format_population(),
         census_year: statement.year,
         year: statement.year,
         source_title: place.title,
         source_url: place.url,
         source_type: :wikidata,
         sources: [Map.take(place, [:title, :url, :provider])]
       }}
    else
      {:error, _} = error -> error
      _ -> {:error, :dated_population_unavailable}
    end
  end

  def population(_, _), do: {:error, :invalid_place}

  defp format_population(digits) do
    digits
    |> String.reverse()
    |> String.graphemes()
    |> Enum.chunk_every(3)
    |> Enum.map(&Enum.join/1)
    |> Enum.join(",")
    |> String.reverse()
  end

  defp latest_dated_population(claims) when is_list(claims) do
    claims
    |> Enum.map(&dated_population/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.sort_by(fn entry -> {entry.year, entry.preferred?} end, :desc)
    |> List.first()
  end

  defp latest_dated_population(_), do: nil

  defp dated_population(%{
         "mainsnak" => %{"datavalue" => %{"value" => %{"amount" => amount}}},
         "qualifiers" => %{"P585" => dates}
       } = claim)
       when is_binary(amount) and is_list(dates) do
    year =
      Enum.find_value(dates, fn
        %{"datavalue" => %{"value" => %{"time" => time}}}
        when is_binary(time) ->
          case Regex.run(~r/^[+]?((?:18|19|20)\d{2})-\d{2}-\d{2}T/u, time) do
            [_, raw] ->
              value = String.to_integer(raw)
              if value <= Date.utc_today().year, do: value

            _ ->
              nil
          end

        _ ->
          nil
      end)

    case Integer.parse(amount) do
      {value, ""} when is_integer(year) and value > 0 and value <= 100_000_000 ->
        if Map.get(claim, "rank") == "deprecated" do
          nil
        else
          %{year: year, value: value, preferred?: Map.get(claim, "rank") == "preferred"}
        end

      _ ->
        nil
    end
  end

  defp dated_population(_), do: nil

  @spec place(String.t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, term()}
  def place(name, hint \\ nil, opts \\ [])
  def place(name, hint, opts) when is_binary(name) do
    fetch = Keyword.get(opts, :fetch, &Req.get/2)
    timeout = Keyword.get(opts, :timeout_ms, 3_500)

    place_titles(name, hint)
    |> Enum.reduce_while({:error, :place_not_verified}, fn title, _ ->
      args = [
        params: [
          action: "wbgetentities", sites: "enwiki", titles: title,
          props: "labels|descriptions|claims", languages: "en", format: "json"
        ],
        headers: [{"user-agent", @user_agent}, {"accept", "application/json"}],
        receive_timeout: timeout,
        retry: false
      ]

      reply =
        try do
          fetch.(@api_url, args)
        rescue
          _ -> {:error, :provider_failure}
        catch
          :exit, _ -> {:error, :provider_unavailable}
        end

      case read_place(reply, name, hint) do
        {:ok, _} = found -> {:halt, found}
        _ -> {:cont, {:error, :place_not_verified}}
      end
    end)
  end

  def place(_, _, _), do: {:error, :invalid_place}

  defp place_titles(name, hint) do
    trimmed = String.trim(name)
    expanded = expand_bc_suffix(trimmed)

    qualifiers = String.split(expanded, ",", parts: 2)
    title =
      case {qualifiers, hint} do
        {[plain], region} when is_binary(region) and region != "" ->
          ["#{plain}, #{region}", plain]

        # A jurisdiction in the user's question is a constraint on the
        # ENTITY, not necessarily part of its English Wikipedia page title.
        # E.g. "Kamloops, BC" is enwiki "Kamloops". Validate the original
        # requested jurisdiction against the candidate's Wikidata description
        # even when the bare title is used.
        {[plain, _region], _} ->
          [expanded, String.trim(plain)]

        _ ->
          [expanded]
      end

    # Try conventional casing for lowercase city names while still
    # requiring exact entity-label and jurisdiction validation.
    title
    |> Enum.flat_map(fn candidate ->
      canonical = candidate
        |> String.split(" ")
        |> Enum.map(&String.capitalize/1)
        |> Enum.join(" ")
      [canonical, candidate]
    end)
    |> Enum.uniq()
  end

  defp read_place({:ok, %{status: 200, body: %{"entities" => entities}}}, name, hint)
       when is_map(entities) do
    entities
    |> Map.values()
    |> Enum.find_value({:error, :place_not_verified}, fn entity ->
      case validated_entity(entity, name, hint) do
        {:ok, _} = match -> match
        _ -> false
      end
    end)
  end

  defp read_place(_, _, _), do: {:error, :unavailable}

  defp validated_entity(%{"id" => id} = entity, name, hint)
       when is_binary(id) do
    label = get_in(entity, ["labels", "en", "value"])
    desc = get_in(entity, ["descriptions", "en", "value"]) || ""
    {requested_name, requested_region} = split_name(name)
    region = requested_region || hint

    valid_identity? =
      is_binary(label) and
        normalize_name(label) == normalize_name(requested_name) and
        (is_nil(region) or
           String.contains?(String.downcase(desc), String.downcase(region))) and
        Regex.match?(~r/\b(?:city|municipality|town|village|community|capital)\b/iu, desc)

    if valid_identity? and Regex.match?(~r/^Q[1-9]\d*$/, id) do
      with [%{"mainsnak" => %{"datavalue" => %{"value" => geo}}} | _] <-
             get_in(entity, ["claims", "P625"]),
           %{"latitude" => lat, "longitude" => lon} <- geo,
           true <- valid_coords?(lat, lon),
           true <- earth_globe?(Map.get(geo, "globe")) do
        {:ok,
         %{
           title: label,
           description: desc,
           url: "https://www.wikidata.org/wiki/" <> id,
           provider: :wikidata,
           latitude: lat,
           longitude: lon,
           population_claims: get_in(entity, ["claims", "P1082"]) || []
         }}
      else
        _ -> {:error, :coordinates_unavailable}
      end
    else
      {:error, :entity_mismatch}
    end
  end

  defp validated_entity(_, _, _), do: {:error, :entity_mismatch}

  # Users write both "Kamloops BC" and "Kamloops, BC". They must resolve
  # to the same explicit province-qualified entity, never to a fuzzy match.
  defp expand_bc_suffix(name) do
    String.replace(name, ~r/(?:,\s*|\s+)BC\s*$/iu, ", British Columbia")
  end

  defp split_name(name) do
    case name |> expand_bc_suffix() |> String.split(",", parts: 2) do
      [plain, region] ->
        region =
          region |> String.trim() |> String.replace(~r/^BC$/iu, "British Columbia")
        {String.trim(plain), region}

      [plain] ->
        {String.trim(plain), nil}
    end
  end

  defp normalize_name(name) do
    name
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{N}]+/u, " ")
    |> String.trim()
  end

  defp valid_coords?(latitude, longitude) do
    is_number(latitude) and is_number(longitude) and
      latitude >= -90 and latitude <= 90 and
      longitude >= -180 and longitude <= 180
  end

  defp earth_globe?(nil), do: true
  defp earth_globe?("http://www.wikidata.org/entity/Q2"), do: true
  defp earth_globe?("https://www.wikidata.org/entity/Q2"), do: true
  defp earth_globe?(_), do: false

  defp haversine(lat1, lon1, lat2, lon2) do
    rad = :math.pi() / 180.0
    dlat = (lat2 - lat1) * rad
    dlon = (lon2 - lon1) * rad

    a = :math.pow(:math.sin(dlat / 2), 2) +
          :math.cos(lat1 * rad) * :math.cos(lat2 * rad) *
            :math.pow(:math.sin(dlon / 2), 2)

    # Clamp rounding artifacts before asin.
    2.0 * @earth_radius_km * :math.asin(min(1.0, :math.sqrt(a)))
  end
end
