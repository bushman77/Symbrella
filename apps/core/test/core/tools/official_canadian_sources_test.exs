defmodule Core.Tools.OfficialCanadianSourcesTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval.{CanadianGeoNames, StatisticsCanadaPopulation}
  alias Core.Tools.EvidenceResolver
  alias Core.Tools.WebRetrieval.Grounding
  alias Core.Response.{PopulationComparison, ResponseFrame, AuthoritativeAnswerPlan}

  @csv """
  REF_DATE,GEO,DGUID,VALUE,STATUS
  2021,"Kamloops (CY), British Columbia",2021A00055933042,98220,
  2024,"Kamloops (CY), British Columbia",2021A00055933042,108652,
  2025,"Kamloops (CY), British Columbia",2021A00055933042,108793,
  2021,"Maple Ridge (CY), British Columbia",2021A00055915075,90990,
  2024,"Maple Ridge (CY), British Columbia",2021A00055915075,103920,
  2025,"Maple Ridge (CY), British Columbia",2021A00055915075,105925,
  2025,"Kamloops (CY), Alberta",2021A0000X,120000,
  """

  defmodule EmptySearch do
    def search(_, _), do: {:ok, []}
  end

  defp group_frame do
    %{type: :location_question, domain: :geography, relation: :population,
      subjects: ["Kamloops BC", "Maple Ridge BC"], grounding_status: :unverified}
  end

  defp context(frame) do
    %{session_id: "official-test", user_text: "",
      features: %{symbolic_frame: frame, intent: :ask},
      symbolic_frame: %{event: frame}}
  end

  test "official municipal rows select latest year and province, not substring" do
    assert {:ok, result} = StatisticsCanadaPopulation.population("Kamloops BC", csv: @csv)
    assert result.value == 108_793
    assert result.census_year == 2025
    assert result.reference_year == 2025
    assert result.measurement_type == :official_estimate
    assert result.source_type == :statistics_canada
    assert result.source_table == "17-10-0155-01"
    assert result.display == "108,793"
    assert {:ok, other} = StatisticsCanadaPopulation.population("Maple Ridge, BC", csv: @csv)
    assert other.value == 105_925
    assert {:error, :explicit_canadian_province_required} =
      StatisticsCanadaPopulation.population("Kamloops", csv: @csv)
    assert {:error, :municipality_not_found} =
      StatisticsCanadaPopulation.population("Kamlopps BC", csv: @csv)
  end

  test "conflicting latest values never become authoritative" do
    duplicate =
      @csv <> ~s|2025,"Kamloops (CY), British Columbia",2021A00055933042,111111,| <> <<10>>

    assert {:error, :ambiguous_measurement} =
      StatisticsCanadaPopulation.population("Kamloops BC", csv: duplicate)

    assert {:error, :invalid_table} =
      StatisticsCanadaPopulation.population("Kamloops BC", csv: "wrong,header")
  end

  test "resolver chooses official estimates and propagates measurement type" do
    frame = group_frame()

    evidence =
      EvidenceResolver.resolve(context(frame), "population for both",
        :population, [provider: EmptySearch, max_attempts: 3],
        statistics_canada_opts: [csv: @csv])

    assert evidence.resolution == :source_supported
    assert evidence.provider == :statistics_canada
    assert evidence.attempted_count == 2
    assert Enum.map(evidence.support.results, & &1.value) == [108_793, 105_925]
    assert Enum.all?(evidence.support.results,
      &(&1.measurement_type == :official_estimate))

    compared =
      context(Map.put(frame, :comparison, :greater))
      |> Map.put(:web_evidence, evidence)
      |> PopulationComparison.attach()

    assert compared.web_evidence.support.winner == "Kamloops BC"
    assert compared.web_evidence.support.difference == 2868

    compared = Map.put(compared, :response_frame, ResponseFrame.build(compared))
    plan = AuthoritativeAnswerPlan.build(compared)
    assert plan.status == :source_supported

    answer = AuthoritativeAnswerPlan.render(plan, :brief)
    assert answer =~ "108,793"
    assert answer =~ "105,925"
    assert answer =~ "2,868"
    assert answer =~ "official annual population estimates"
    assert Grounding.supports?(answer, compared)
  end

  test "official place coordinates return bounded government citations" do
    fetch = fn _url, args ->
      assert args[:params][:category] == "O"
      assert args[:params][:province] == "59"
      city = args[:params][:q]

      item =
        case city do
          "Kamloops" ->
            %{"id" => "AAAAA", "name" => "Kamloops",
              "latitude" => 50.6761, "longitude" => -120.341, "province" => "59"}
          "Maple Ridge" ->
            %{"id" => "BBBBB", "name" => "Maple Ridge",
              "latitude" => 49.2167, "longitude" => -122.6, "province" => "59"}
        end

      {:ok, %{status: 200, body: %{"items" => [item]}}}
    end

    assert {:ok, proof} =
      CanadianGeoNames.proximity("Kamloops BC", "Maple Ridge BC", fetch: fetch)

    assert proof.status == :source_supported
    assert proof.coordinate_provider == :natural_resources_canada
    assert proof.coordinate_type == :named_feature_representative_point
    assert proof.distance_km > 200
    assert proof.distance_km < 260
    assert Enum.all?(proof.sources,
      &String.starts_with?(&1.url, "https://geogratis.gc.ca/"))
  end

  test "ambiguous official place names and mismatched names are not accepted" do
    duplicate = fn _url, _args ->
      {:ok, %{status: 200, body: %{"items" => [
        %{"id" => "AAAAA", "name" => "Kamloops",
          "latitude" => 50.6, "longitude" => -120.3, "province" => "59"},
        %{"id" => "BBBBB", "name" => "Kamloops",
          "latitude" => 50.7, "longitude" => -120.4, "province" => "59"}
      ]}}}
    end

    assert {:error, :ambiguous_place} = CanadianGeoNames.place("Kamloops BC", nil,
      fetch: duplicate)

    wrong_name = fn _url, _args ->
      {:ok, %{status: 200, body: %{"items" => [
        %{"id" => "AAAAA", "name" => "Kamloops North",
          "latitude" => 50.6, "longitude" => -120.3, "province" => "59"}
      ]}}}
    end

    assert {:error, :place_not_verified} =
      CanadianGeoNames.place("Kamloops BC", nil, fetch: wrong_name)
  end

  test "different measurement types cannot be compared even if years match" do
    frame = Map.put(group_frame(), :comparison, :greater)

    evidence =
      EvidenceResolver.resolve(context(group_frame()), "population both", :population,
        [provider: EmptySearch, max_attempts: 3],
        statistics_canada_opts: [csv: @csv])

    mixed =
      update_in(evidence, [:support, :results], fn [first, second] ->
        [first, %{second | measurement_type: :dated_population_statement,
                         source_type: :wikidata}]
      end)

    result =
      context(frame)
      |> Map.put(:web_evidence, mixed)
      |> PopulationComparison.attach()

    assert result.web_evidence.support.kind == :population_group
    assert result.web_evidence.comparison_status == :incompatible_measurements
    assert Grounding.align("Kamloops is larger.", result) =~
      "measurement types or source series differ"
  end
end
