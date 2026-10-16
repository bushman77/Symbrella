defmodule Core.Tools.StatisticsCanadaPopulationFallbackTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval.StatisticsCanadaPopulation

  @zip_url "https://www150.statcan.gc.ca/n1/tbl/csv/17100155-eng.zip"
  @csv "REF_DATE,GEO,VALUE,STATUS,DGUID\n" <>
         "2025,\"Kamloops (CY), British Columbia\",108793,,2021A00055933042\n"

  defp zip_data do
    {:ok, {_name, data}} =
      :zip.create(~c"17100155-test.zip", [{~c"17100155.csv", @csv}], [:memory])

    data
  end

  defp cache_path do
    path =
      Path.join(System.tmp_dir!(),
        "symbrella-statcan-test-#{System.unique_integer([:positive, :monotonic])}.csv")

    on_exit(fn ->
      File.rm(path)
      File.rm(path <> ".tmp")
    end)

    path
  end

  test "Req ZIP timeout falls back to validated official curl data and caches it" do
    fetch = fn _url, _args -> {:error, %Req.TransportError{reason: :timeout}} end

    curl_download = fn url, timeout ->
      send(self(), {:curl_download, url, timeout})
      {:ok, zip_data()}
    end

    cache = cache_path()

    assert {:ok, result} =
             StatisticsCanadaPopulation.population("Kamloops BC",
               fetch: fetch,
               curl_download: curl_download,
               timeout_ms: 1_000,
               cache_path: cache
             )

    assert_receive {:curl_download, @zip_url, 1_000}
    assert result.value == 108_793
    assert result.reference_year == 2025
    assert result.source_type == :statistics_canada
    assert {:ok, @csv} = File.read(cache)
  end

  test "a successful Req response does not invoke curl" do
    zip = zip_data()

    fetch = fn url, _args ->
      if String.contains?(url, "getFullTableDownloadCSV") do
        {:ok, %{status: 200, body: %{"status" => "SUCCESS", "object" => @zip_url}}}
      else
        {:ok, %{status: 200, body: zip}}
      end
    end

    curl_download = fn _url, _timeout ->
      send(self(), :unexpected_curl)
      {:error, :should_not_be_called}
    end

    assert {:ok, %{value: 108_793}} =
             StatisticsCanadaPopulation.population("Kamloops BC",
               fetch: fetch,
               curl_download: curl_download,
               cache_path: cache_path()
             )

    refute_received :unexpected_curl
  end

  test "malformed curl ZIP is rejected and never written to cache" do
    fetch = fn _url, _args -> {:error, %Req.TransportError{reason: :timeout}} end
    cache = cache_path()

    assert {:error, {:official_download_failed, %{curl: :not_a_zip_archive}}} =
             StatisticsCanadaPopulation.population("Kamloops BC",
               fetch: fetch,
               curl_download: fn _, _ -> {:ok, "not a ZIP"} end,
               cache_path: cache
             )

    refute File.exists?(cache)
  end

  test "HTTP rejection is not disguised as a network timeout" do
    fetch = fn _url, _args -> {:ok, %{status: 503, body: "Unavailable"}} end

    assert {:error, {:official_download_failed, %{direct_zip: {:zip_http_status, 503}}}} =
             StatisticsCanadaPopulation.population("Kamloops BC",
               fetch: fetch,
               curl_download: fn _, _ ->
                 send(self(), :unexpected_curl)
                 {:error, :should_not_be_called}
               end,
               cache_path: cache_path()
             )

    refute_received :unexpected_curl
  end

  test "curl failure preserves the original Req diagnostics" do
    fetch = fn _url, _args -> {:error, %Req.TransportError{reason: :timeout}} end

    assert {:error, {:official_download_failed, reasons}} =
             StatisticsCanadaPopulation.population("Kamloops BC",
               fetch: fetch,
               curl_download: fn _, _ -> {:error, :curl_unreachable} end,
               cache_path: cache_path()
             )

    assert reasons.curl == :curl_unreachable
    assert match?({:zip, :transport, _}, reasons.direct_zip)
  end
end
