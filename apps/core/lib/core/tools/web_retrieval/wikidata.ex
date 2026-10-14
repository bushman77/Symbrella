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

  @spec place(String.t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, term()}
  def place(name, hint \\ nil, opts \\ []) when is_binary(name) do
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
    expanded =
      trimmed
      |> String.replace(~r/,\s*BC\s*$/iu, ", British Columbia")

    qualifiers = String.split(expanded, ",", parts: 2)
    title =
      case {qualifiers, hint} do
        {[plain], region} when is_binary(region) and region != "" ->
          ["#{plain}, #{region}", plain]
        _ ->
          [expanded]
      end

    Enum.uniq(title)
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
           url: "https://www.wikidata.org/wiki/" <> id,
           provider: :wikidata,
           latitude: lat,
           longitude: lon
         }}
      else
        _ -> {:error, :coordinates_unavailable}
      end
    else
      {:error, :entity_mismatch}
    end
  end

  defp validated_entity(_, _, _), do: {:error, :entity_mismatch}

  defp split_name(name) do
    case String.split(name, ",", parts: 2) do
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
