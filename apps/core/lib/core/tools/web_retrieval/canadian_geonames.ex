defmodule Core.Tools.WebRetrieval.CanadianGeoNames do
  @moduledoc """
  Official Canadian names and representative coordinates, via the NRCan
  Canadian Geographical Names Database (CGNDB) API. Exact and unique match
  required; coordinate points are not addresses, boundaries or road routes.
  """

  @endpoint "https://geogratis.gc.ca/services/geoname/en/geonames.json"
  @user_agent "Symbrella/0.1 (https://github.com/bushman77/Symbrella)"
  @radius_km 6371.0088
  @provinces %{
    "bc" => "59", "british columbia" => "59",
    "ab" => "48", "alberta" => "48",
    "sk" => "47", "saskatchewan" => "47",
    "mb" => "46", "manitoba" => "46",
    "on" => "35", "ontario" => "35",
    "qc" => "24", "quebec" => "24", "québec" => "24",
    "nb" => "13", "new brunswick" => "13",
    "ns" => "12", "nova scotia" => "12",
    "pe" => "11", "pei" => "11", "prince edward island" => "11",
    "nl" => "10", "newfoundland and labrador" => "10",
    "yt" => "60", "yukon" => "60",
    "nt" => "61", "northwest territories" => "61",
    "nu" => "62", "nunavut" => "62"
  }

  @spec place(String.t(), String.t() | nil, keyword()) ::
          {:ok, map()} | {:error, atom()}
  def place(name, hint \\ nil, opts \\ [])
  def place(name, hint, opts) when is_binary(name) and is_list(opts) do
    {bare, region} = split_name(name)
    code = region_code(region || hint)

    cond do
      bare == "" or byte_size(bare) > 100 -> {:error, :invalid_place}
      (is_binary(region) or is_binary(hint)) and is_nil(code) ->
        {:error, :unsupported_province}
      true ->
        params = [q: bare, category: "O", num: 40]
        params = if code, do: Keyword.put(params, :province, code), else: params
        fetch = Keyword.get(opts, :fetch, &Req.get/2)
        timeout = Keyword.get(opts, :timeout_ms, 3_500)

        reply =
          try do
            fetch.(@endpoint,
              params: params,
              headers: [{"user-agent", @user_agent}, {"accept", "application/json"}],
              receive_timeout: timeout, retry: false)
          rescue
            _ -> {:error, :provider_failure}
          catch
            :exit, _ -> {:error, :provider_unavailable}
          end

        read_place(reply, bare, code)
    end
  end
  def place(_, _, _), do: {:error, :invalid_place}

  @spec proximity(String.t(), String.t(), keyword()) ::
          {:ok, map()} | {:error, term()}
  def proximity(subject, target, opts \\ [])

  def proximity(subject, target, opts)
      when is_binary(subject) and is_binary(target) and is_list(opts) do
    {_, region} = split_name(subject)

    with {:ok, first} <- place(subject, nil, opts),
         {:ok, second} <- place(target, region, opts),
         true <- first.id != second.id do
      distance = haversine(first.latitude, first.longitude,
        second.latitude, second.longitude)

      {:ok, %{
        kind: :proximity,
        status: :source_supported,
        method: :haversine,
        coordinate_type: :named_feature_representative_point,
        coordinate_provider: :natural_resources_canada,
        distance_km: Float.round(distance, 1),
        subject: subject, target: target,
        coordinates: %{
          subject: {first.latitude, first.longitude},
          target: {second.latitude, second.longitude}
        },
        sources: [Map.take(first, [:title, :url, :provider]),
                  Map.take(second, [:title, :url, :provider])]
      }}
    else
      false -> {:error, :identical_place}
      {:error, _} = error -> error
      _ -> {:error, :coordinates_unavailable}
    end
  end
  def proximity(_, _, _), do: {:error, :invalid_places}

  defp read_place({:ok, %{status: 200, body: %{"items" => items}}}, bare, code)
       when is_list(items) do
    candidates =
      items
      |> Enum.filter(&is_map/1)
      |> Enum.filter(fn item ->
        normalize(Map.get(item, "name")) == normalize(bare) and
          official?(item) and matches_province?(item, code)
      end)
      |> Enum.map(&candidate/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq_by(& &1.id)

    case candidates do
      [only] -> {:ok, only}
      [] -> {:error, :place_not_verified}
      _ -> {:error, :ambiguous_place}
    end
  end
  defp read_place(_, _, _), do: {:error, :coordinates_unavailable}

  defp candidate(%{"id" => id, "name" => name} = item)
       when is_binary(id) and is_binary(name) do
    with true <- Regex.match?(~r/^[A-Za-z0-9]{4,64}$/u, id),
         {:ok, lat} <- coordinate(Map.get(item, "latitude")),
         {:ok, lon} <- coordinate(Map.get(item, "longitude")),
         true <- lat >= 40 and lat <= 85 and lon >= -142 and lon <= -50 do
      %{
        id: id, title: name, latitude: lat, longitude: lon,
        provider: :natural_resources_canada,
        url: "https://geogratis.gc.ca/services/geoname/en/geonames/#{id}"
      }
    else
      _ -> nil
    end
  end
  defp candidate(_), do: nil

  defp official?(item) do
    Map.get(item, "category") in [nil, "O"] and
      Map.get(item, "status") not in ["H", "historical", "formerly official"]
  end

  defp matches_province?(_item, nil), do: true
  defp matches_province?(item, code) do
    value = Map.get(item, "province")
    is_nil(value) or value == code or
      (is_map(value) and (value["code"] == code or value["id"] == code))
  end

  defp coordinate(n) when is_number(n), do: {:ok, n / 1}
  defp coordinate(n) when is_binary(n) do
    case Float.parse(n) do
      {value, ""} -> {:ok, value}
      _ -> :error
    end
  end
  defp coordinate(_), do: :error

  defp split_name(name) do
    case String.split(String.trim(name), ~r/\s*,\s*/u, parts: 2) do
      [bare, province] -> {String.trim(bare), String.trim(province)}
      [bare] ->
        case Regex.run(~r/^(.*?)\s+(BC|AB|SK|MB|ON|QC|NB|NS|PE|NL|YT|NT|NU)$/iu,
               bare) do
          [_, city, province] -> {String.trim(city), province}
          _ -> {String.trim(bare), nil}
        end
    end
  end

  defp region_code(nil), do: nil
  defp region_code(value) when is_binary(value),
    do: Map.get(@provinces, String.downcase(String.trim(value)))

  defp normalize(value) when is_binary(value),
    do: value |> String.downcase() |> String.replace(~r/[^\p{L}\p{N}]/u, "")
  defp normalize(_), do: ""

  defp haversine(lat1, lon1, lat2, lon2) do
    r1 = lat1 * :math.pi() / 180
    r2 = lat2 * :math.pi() / 180
    dr = (lat2 - lat1) * :math.pi() / 180
    dl = (lon2 - lon1) * :math.pi() / 180
    a = :math.pow(:math.sin(dr / 2), 2) +
      :math.cos(r1) * :math.cos(r2) * :math.pow(:math.sin(dl / 2), 2)
    @radius_km * 2 * :math.atan2(:math.sqrt(a), :math.sqrt(max(0.0, 1 - a)))
  end
end
