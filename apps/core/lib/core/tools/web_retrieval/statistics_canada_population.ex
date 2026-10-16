defmodule Core.Tools.WebRetrieval.StatisticsCanadaPopulation do
  @moduledoc """
  Official annual municipal population estimates from Statistics Canada
  table 17-10-0155-01 (2021 census subdivision boundaries).

  The WDS full-table download endpoint supplies the current published CSV
  archive. A local seven-day disk cache avoids downloading the table per
  question. Records are matched to the requested municipality AND province;
  ambiguous, missing, malformed or non-integer values are never facts.

  The legacy :census_year field is populated for compatibility with the
  current geographic evidence pipeline. It contains the annual reference
  year, NOT a claim that an estimate is a census count.
  """

  @table_id "17-10-0155-01"
  @pid "17100155"
  @metadata_url "https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/17100155/en"
  @direct_zip_url "https://www150.statcan.gc.ca/n1/tbl/csv/17100155-eng.zip"
  @source_url "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015501"
  @user_agent "Symbrella/0.1 (https://github.com/bushman77/Symbrella)"
  @cache_days 7
  @max_zip_bytes 24_000_000
  @max_csv_bytes 96_000_000
  @provinces %{
    "bc" => "British Columbia", "british columbia" => "British Columbia",
    "ab" => "Alberta", "alberta" => "Alberta",
    "sk" => "Saskatchewan", "saskatchewan" => "Saskatchewan",
    "mb" => "Manitoba", "manitoba" => "Manitoba",
    "on" => "Ontario", "ontario" => "Ontario",
    "qc" => "Quebec", "quebec" => "Quebec", "québec" => "Quebec",
    "nb" => "New Brunswick", "new brunswick" => "New Brunswick",
    "ns" => "Nova Scotia", "nova scotia" => "Nova Scotia",
    "pe" => "Prince Edward Island", "prince edward island" => "Prince Edward Island",
    "nl" => "Newfoundland and Labrador", "newfoundland and labrador" => "Newfoundland and Labrador",
    "yt" => "Yukon", "yukon" => "Yukon",
    "nt" => "Northwest Territories", "northwest territories" => "Northwest Territories",
    "nu" => "Nunavut", "nunavut" => "Nunavut"
  }

  @spec population(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def population(subject, opts \\ [])
  def population(subject, opts) when is_binary(subject) and is_list(opts) do
    with {:ok, city, province} <- identify(subject),
         {:ok, csv} <- csv_data(opts),
         {:ok, %{value: count, year: year, dguid: dguid}} <-
           latest_population(csv, city, province) do
      {:ok, %{
        kind: :population,
        status: :source_supported,
        subject: subject,
        value: count, display: format_count(count),
        census_year: year, reference_year: year,
        measurement_type: :official_estimate,
        method: :annual_census_subdivision_estimate,
        source_type: :statistics_canada,
        source_table: @table_id,
        source_url: @source_url,
        geography_type: :census_subdivision,
        dguid: dguid,
        sources: [%{
          title: "Statistics Canada table #{@table_id}: #{city}, #{province}",
          url: @source_url, provider: :statistics_canada
        }]
      }}
    end
  end
  def population(_, _), do: {:error, :invalid_place}

  @doc "Parse the official full-table CSV, refusing missing or ambiguous identity."
  @spec latest_population(String.t(), String.t(), String.t()) ::
          {:ok, map()} | {:error, atom()}
  def latest_population(csv, city, province)
      when is_binary(csv) and is_binary(city) and is_binary(province) do
    rows = String.split(csv, "\n", trim: true)

    with [header | data] <- rows,
         headings <- csv_fields(String.trim_leading(header, "\uFEFF")),
         true <- Enum.all?(["REF_DATE", "GEO", "VALUE"], &(&1 in headings)) do
      indexes = Enum.into(Enum.with_index(headings), %{}, fn {name, index} ->
        {name, index}
      end)

      current_year = Date.utc_today().year

      records =
        data
        |> Enum.reduce([], fn line, acc ->
          # Filter first to avoid full CSV parsing of irrelevant municipalities.
          if String.contains?(String.downcase(line), String.downcase(city)) do
            fields = csv_fields(line)
            geo = cell(fields, indexes, "GEO") || ""
            date = cell(fields, indexes, "REF_DATE") || ""
            value = cell(fields, indexes, "VALUE") || ""
            status = cell(fields, indexes, "STATUS")
            dguid = cell(fields, indexes, "DGUID")

            if exact_geo?(geo, city, province) and
                 status not in ["..", "x", "F"] do
              case {Integer.parse(date), Integer.parse(value)} do
                {{year, ""}, {count, ""}}
                when year >= 1900 and year <= current_year and
                     count > 0 and count <= 100_000_000 ->
                  [%{year: year, value: count, dguid: dguid,
                     geo: geo} | acc]

                _ -> acc
              end
            else
              acc
            end
          else
            acc
          end
        end)

      case Enum.sort_by(records, & &1.year, :desc) do
        [best | _] = results ->
          # Refuse multiple rows for a year, including dimension mismatches.
          if Enum.count(results, &(&1.year == best.year)) == 1 do
            {:ok, best}
          else
            {:error, :ambiguous_measurement}
          end

        [] -> {:error, :municipality_not_found}
      end
    else
      _ -> {:error, :invalid_table}
    end
  end
  def latest_population(_, _, _), do: {:error, :invalid_table}

  defp exact_geo?(geo, city, province) do
    pattern =
      ~r/^(.+?)\s+\(([A-Z]{1,4})\),\s+(.+)$/u

    case Regex.run(pattern, geo) do
      [_, name, _kind, found_province] ->
        normalize(name) == normalize(city) and
          normalize(found_province) == normalize(province)

      _ -> false
    end
  end

  defp identify(subject) do
    parts = String.split(String.trim(subject), ~r/\s*,\s*/u, parts: 2)

    {name, raw_region} =
      case parts do
        [city, region] -> {city, region}
        [value] ->
          case Regex.run(~r/^(.*?)\s+(BC|AB|SK|MB|ON|QC|NB|NS|PE|NL|YT|NT|NU)$/iu,
                 value) do
            [_, city, region] -> {city, region}
            _ -> {value, nil}
          end
      end

    region =
      if is_binary(raw_region),
        do: Map.get(@provinces, String.downcase(String.trim(raw_region))),
        else: nil

    if is_binary(region) and name != "" and byte_size(name) <= 100,
      do: {:ok, String.trim(name), region},
      else: {:error, :explicit_canadian_province_required}
  end

  defp csv_data(opts) do
    cond do
      is_binary(Keyword.get(opts, :csv)) ->
        {:ok, Keyword.fetch!(opts, :csv)}

      is_binary(Keyword.get(opts, :csv_path)) ->
        File.read(Keyword.fetch!(opts, :csv_path))

      true ->
        cache = Keyword.get(opts, :cache_path, default_cache_path())
        refresh_cache(cache, opts)
    end
  end

  defp default_cache_path do
    base = System.get_env("XDG_CACHE_HOME") || Path.join(System.user_home!(), ".cache")
    Path.join([base, "symbrella", "statistics_canada_#{@pid}.csv"])
  end

  defp refresh_cache(path, opts) do
    fresh? =
      case File.stat(path, time: :posix) do
        {:ok, %{mtime: stamp}} ->
          System.system_time(:second) - stamp < @cache_days * 86_400
        _ -> false
      end

    if fresh? do
      File.read(path)
    else
      # Keep the last downloaded official release if the upstream endpoint
      # cannot be reached; never manufacture a newer year from cache age.
      case fetch_latest_csv(opts) do
        {:ok, csv} ->
          with :ok <- File.mkdir_p(Path.dirname(path)),
               :ok <- File.write(path <> ".tmp", csv),
               :ok <- File.rename(path <> ".tmp", path) do
            {:ok, csv}
          else
            _ -> {:ok, csv}
          end
        {:error, _} = error ->
          case File.read(path) do
            {:ok, cached} -> {:ok, cached}
            _ -> error
          end
      end
    end
  end

  # First use the WDS discovery service, then the independently documented
  # official direct table URL. Do not collapse errors into a generic message:
  # Termux needs to distinguish DNS/SSL, HTTP, ZIP and CSV errors.
  defp fetch_latest_csv(opts) do
    fetch = Keyword.get(opts, :fetch, &Req.get/2)
    timeout = Keyword.get(opts, :timeout_ms, 10_000)

    case metadata_download_url(fetch, timeout) do
      {:ok, url} ->
        case download_table(fetch, url, timeout) do
          {:ok, _} = success ->
            success

          {:error, discovered_error} ->
            if url == @direct_zip_url do
              {:error, {:official_download_failed, %{metadata: :ok, zip: discovered_error}}}
            else
              fallback_table(fetch, timeout, %{metadata: :ok, discovered_zip: discovered_error})
            end
        end

      {:error, metadata_error} ->
        fallback_table(fetch, timeout, %{metadata: metadata_error})
    end
  end

  defp fallback_table(fetch, timeout, previous) do
    case download_table(fetch, @direct_zip_url, timeout) do
      {:ok, _} = success -> success
      {:error, reason} ->
        {:error, {:official_download_failed, Map.put(previous, :direct_zip, reason)}}
    end
  end

  defp metadata_download_url(fetch, timeout) do
    args = [
      headers: [{"user-agent", @user_agent}, {"accept", "application/json"}],
      receive_timeout: timeout, retry: false
    ]

    case safe_fetch(fetch, @metadata_url, args, :metadata) do
      {:ok, %{status: 200, body: body}} ->
        case decode_metadata(body) do
          %{"status" => "SUCCESS", "object" => url} ->
            if valid_download_url?(url),
              do: {:ok, url},
              else: {:error, :invalid_official_download_url}

          %{"status" => "FAILED"} ->
            {:error, :metadata_api_rejected}

          _ ->
            {:error, :unexpected_metadata_response}
        end

      {:ok, %{status: status}} ->
        {:error, {:metadata_http_status, status}}

      {:error, _} = error ->
        error

      _ ->
        {:error, :unexpected_metadata_response}
    end
  end

  defp decode_metadata(%{} = body), do: body
  defp decode_metadata(body) when is_binary(body) do
    case Jason.decode(body) do
      {:ok, %{} = decoded} -> decoded
      _ -> nil
    end
  end
  defp decode_metadata(_), do: nil

  defp download_table(fetch, url, timeout) do
    args = [
      headers: [{"user-agent", @user_agent}, {"accept", "application/zip"}],
      receive_timeout: timeout, retry: false
    ]

    case safe_fetch(fetch, url, args, :zip) do
      {:ok, %{status: 200, body: data}} ->
        extract_table_csv(data)

      {:ok, %{status: status}} ->
        {:error, {:zip_http_status, status}}

      {:error, _} = error ->
        error

      _ ->
        {:error, :unexpected_zip_response}
    end
  end

  defp extract_table_csv(data) when is_binary(data) do
    cond do
      byte_size(data) > @max_zip_bytes ->
        {:error, {:zip_too_large, byte_size(data)}}

      not String.starts_with?(data, "PK") ->
        {:error, :not_a_zip_archive}

      true ->
        case :zip.extract({:binary, data}, [:memory]) do
          {:ok, files} ->
            case Enum.find(files, fn {name, _} ->
                   Path.basename(to_string(name)) == "#{@pid}.csv"
                 end) do
              {_name, csv} when is_binary(csv) and byte_size(csv) <= @max_csv_bytes ->
                if valid_csv_header?(csv),
                  do: {:ok, csv},
                  else: {:error, :invalid_csv_header}

              {_name, csv} when is_binary(csv) ->
                {:error, {:csv_too_large, byte_size(csv)}}

              _ ->
                {:error, :official_csv_missing_from_archive}
            end

          {:error, _} ->
            {:error, :invalid_zip_archive}
        end
    end
  end

  defp extract_table_csv(_), do: {:error, :invalid_zip_body}

  defp valid_csv_header?(csv) when is_binary(csv) do
    case String.split(csv, "\n", parts: 2) do
      [header, _] ->
        fields = csv_fields(String.trim_leading(header, "\uFEFF"))
        Enum.all?(["REF_DATE", "GEO", "VALUE"], &(&1 in fields))

      _ ->
        false
    end
  end

  defp safe_fetch(fetch, url, args, stage) do
    try do
      fetch.(url, args)
    rescue
      error ->
        {:error, {stage, :exception, summarize_reason(error)}}
    catch
      :exit, reason ->
        {:error, {stage, :exit, summarize_reason(reason)}}

      :throw, reason ->
        {:error, {stage, :throw, summarize_reason(reason)}}
    end
    |> case do
      {:error, reason} -> {:error, {stage, :transport, summarize_reason(reason)}}
      result -> result
    end
  end

  defp summarize_reason(reason),
    do: reason |> inspect(limit: 8, printable_limit: 180) |> String.slice(0, 260)

  defp valid_download_url?(url) when is_binary(url) do
    case URI.parse(url) do
      %URI{scheme: "https", host: "www150.statcan.gc.ca", path: path}
      when is_binary(path) ->
        String.ends_with?(path, "/#{@pid}-eng.zip")
      _ -> false
    end
  end
  defp valid_download_url?(_), do: false

  # The full table is RFC4180 CSV. Split individual records while respecting
  # quoted commas and escaped quotes; rows with embedded quoted newlines are
  # rejected rather than silently matched to a different municipality.
  defp csv_fields(line) do
    line
    |> String.trim_trailing("\r")
    |> String.to_charlist()
    |> csv_cells([], [], false)
  end

  defp csv_cells([], cells, field, false),
    do: Enum.reverse([field |> Enum.reverse() |> List.to_string() | cells])
  defp csv_cells([], _, _, true), do: []
  defp csv_cells([34, 34 | rest], cells, field, true),
    do: csv_cells(rest, cells, [34 | field], true)
  defp csv_cells([34 | rest], cells, field, quoted),
    do: csv_cells(rest, cells, field, not quoted)
  defp csv_cells([44 | rest], cells, field, false),
    do: csv_cells(rest, [field |> Enum.reverse() |> List.to_string() | cells], [], false)
  defp csv_cells([char | rest], cells, field, quoted),
    do: csv_cells(rest, cells, [char | field], quoted)

  defp cell(fields, indexes, name) do
    case Map.get(indexes, name) do
      nil -> nil
      index -> Enum.at(fields, index)
    end
  end

  defp normalize(value),
    do: value |> String.downcase() |> String.replace(~r/[^\p{L}\p{N}]/u, "")

  defp format_count(n) do
    n |> Integer.to_string() |> String.reverse()
    |> String.graphemes() |> Enum.chunk_every(3)
    |> Enum.map_join(",", &Enum.join/1) |> String.reverse()
  end
end
