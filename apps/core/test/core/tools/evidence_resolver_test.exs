defmodule Core.Tools.EvidenceResolverTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.Wikidata
  alias Core.Tools.WebRetrieval.Grounding
  alias Core.Response.ReflectionLoop

  defmodule OffTopicWikipedia do
    def search(_query, _opts) do
      {:ok, [
        %{provider: :wikipedia, title: "Economic history of Vancouver",
          url: "https://en.wikipedia.org/wiki/Economic_history_of_Vancouver",
          excerpt: "Vancouver has a long economic history."}
      ]}
    end
  end

  defmodule PopulationOnRetry do
    def search(query, _opts) do
      excerpt =
        if String.contains?(query, "2021 census population"),
          do: "Maple Ridge's population in 2021 was 90,990.",
          else: "Maple Ridge is a Canadian city."

      {:ok, [
        %{provider: :wikipedia, title: "Maple Ridge, British Columbia",
          url: "https://en.wikipedia.org/wiki/Maple_Ridge,_British_Columbia",
          excerpt: excerpt}
      ]}
    end
  end

  defmodule SuccessfulCoords do
    def proximity(subject, target, _opts) do
      {:ok, %{kind: :proximity, status: :source_supported,
        subject: subject, target: target, method: :haversine,
        distance_km: 38.5,
        sources: [
          %{title: "Maple Ridge", provider: :wikidata,
            url: "https://www.wikidata.org/wiki/Q16740"},
          %{title: "Vancouver", provider: :wikidata,
            url: "https://www.wikidata.org/wiki/Q24639"}
        ]}}
    end
  end

  defmodule FailedCoords do
    def proximity(_, _, _), do: {:error, :entity_mismatch}
  end

  defp context(relation) do
    frame = %{
      type: :location_question, subject: "Maple Ridge, BC",
      target: "Vancouver", relation: relation, grounding_status: :unverified
    }
    %{session_id: "evidence-test",
      user_text: "Is it close to Vancouver?",
      features: %{intent: :ask, conf: 0.9, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer}}
  end

  test "tries structured coordinates after irrelevant Wikipedia results" do
    result = WebRetrieval.enrich(context(:proximity),
      enabled?: true,
      provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords
    )
    evidence = result.web_evidence

    assert evidence.status == :retrieved
    assert evidence.resolution == :source_supported
    assert evidence.attempted_count == 2
    assert Enum.map(evidence.attempts, & &1.status) ==
      [:retrieved_unsupported, :source_supported]
    assert evidence.support.kind == :proximity
    assert Enum.take(evidence.sources, 2) |> Enum.map(& &1.provider) == [:wikidata, :wikidata]

    {:ok, final, review} =
      ReflectionLoop.review("Is it close to Vancouver?",
        "No, it is 100 km away.", result)

    assert review.status == :repair
    assert final =~ "38.5 km"
    assert final =~ "straight line"
    assert final =~ "www.wikidata.org/wiki/Q16740"
    assert final =~ "www.wikidata.org/wiki/Q24639"
    refute final =~ "100 km"
    assert Grounding.supports?(final, result)
  end

  test "an unverified refusal is upgraded when evidence is source-supported" do
    context = WebRetrieval.enrich(context(:proximity),
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords)

    {:ok, final, reflection} =
      ReflectionLoop.review("Is it close to Vancouver?",
        "I can't verify the distance.", context)

    assert reflection.status == :repair
    assert final =~ "38.5 km"
  end

  test "population lookup retries a tighter source query" do
    result = WebRetrieval.enrich(context(:population),
      enabled?: true, provider: PopulationOnRetry)

    assert result.web_evidence.resolution == :source_supported
    assert result.web_evidence.attempted_count == 2
    assert result.web_evidence.support.value == 90_990
    assert result.web_evidence.support.census_year == 2021
  end

  test "failed alternatives exhaust the budget; no unsupported claim is promoted" do
    result = WebRetrieval.enrich(context(:proximity),
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: FailedCoords, max_attempts: 2)

    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.attempted_count == 2
    assert result.web_evidence.support == nil

    {:ok, final, reflection} =
      ReflectionLoop.review("Is it close to Vancouver?",
        "They are definitely 100 km apart.", result)

    assert reflection.status == :repair
    assert final =~ "don't have verified distance"
    refute final =~ "100 km"
  end

  test "independent source claims are not inferred merely from a shared Wikimedia corpus" do
    result = WebRetrieval.enrich(context(:proximity),
      enabled?: true, provider: OffTopicWikipedia,
      coordinate_provider: SuccessfulCoords)

    assert result.web_evidence.resolution == :source_supported
    refute result.web_evidence.resolution == :corroborated
    assert length(result.web_evidence.support.sources) == 2
  end

  defp wikidata_reply(title) do
    case title do
      "Maple Ridge, British Columbia" ->
        entity("Q16740", "Maple Ridge", "city in British Columbia, Canada", 49.2167, -122.6)

      "Vancouver, British Columbia" ->
        {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}

      "Vancouver" ->
        entity("Q24639", "Vancouver", "city in British Columbia, Canada", 49.2827, -123.1207)

      other ->
        flunk("Unexpected title: #{other}")
    end
  end

  defp entity(id, label, description, lat, lon) do
    {:ok, %{status: 200, body: %{
      "entities" => %{id => %{
        "id" => id,
        "labels" => %{"en" => %{"value" => label}},
        "descriptions" => %{"en" => %{"value" => description}},
        "claims" => %{"P625" => [
          %{"mainsnak" => %{"datavalue" => %{"value" => %{
            "latitude" => lat, "longitude" => lon,
            "globe" => "http://www.wikidata.org/entity/Q2"
          }}}}
        ]}
      }}
    }}}
  end

  test "Wikidata resolves unpunctuated BC qualifiers without weakening entity validation" do
    fake_get = fn url, opts ->
      assert url == "https://www.wikidata.org/w/api.php"

      case opts[:params][:titles] do
        "Maple Ridge, British Columbia" ->
          wikidata_reply("Maple Ridge, British Columbia")

        "Kamloops, British Columbia" ->
          entity("Q2138", "Kamloops", "city in British Columbia, Canada",
            50.6745, -120.3273)

        other ->
          flunk("Unexpected Wikidata entity query: #{other}")
      end
    end

    assert {:ok, proof} =
      Wikidata.proximity("maple ridge bc", "kamloops bc", fetch: fake_get)

    assert proof.kind == :proximity
    assert proof.distance_km > 0
    assert proof.method == :haversine
  end

  test "Wikidata provider validates two city entities and computes straight-line distance" do
    fake_get = fn url, opts ->
      assert url == "https://www.wikidata.org/w/api.php"
      assert opts[:params][:action] == "wbgetentities"
      assert opts[:params][:sites] == "enwiki"
      wikidata_reply(opts[:params][:titles])
    end

    assert {:ok, proof} =
      Wikidata.proximity("Maple Ridge, BC", "Vancouver", fetch: fake_get)

    assert proof.method == :haversine
    assert proof.status == :source_supported
    assert_in_delta proof.distance_km, 38.5, 0.3
    assert Enum.map(proof.sources, & &1.url) == [
      "https://www.wikidata.org/wiki/Q16740",
      "https://www.wikidata.org/wiki/Q24639"
    ]
  end

  test "Wikidata rejects the wrong city rather than treating search matches as verified" do
    fake_get = fn _url, opts ->
      title = opts[:params][:titles]
      case title do
        "Maple Ridge, British Columbia" -> wikidata_reply(title)
        "Vancouver, British Columbia" ->
          {:ok, %{status: 200, body: %{"entities" => %{"-1" => %{"id" => "-1", "missing" => ""}}}}}
        "Vancouver" ->
          entity("Q200", "Vancouver", "city in Washington, United States", 45.6, -122.67)
      end
    end

    assert {:error, :place_not_verified} =
      Wikidata.proximity("Maple Ridge, BC", "Vancouver", fetch: fake_get)
  end

  test "malformed geographic coordinates are rejected" do
    fake_get = fn _url, opts ->
      title = opts[:params][:titles]
      case title do
        "Maple Ridge, British Columbia" ->
          entity("Q16740", "Maple Ridge", "city in British Columbia, Canada", 190.0, -122.6)
        _ ->
          wikidata_reply(title)
      end
    end

    assert {:error, :place_not_verified} =
      Wikidata.proximity("Maple Ridge, BC", "Vancouver", fetch: fake_get)
  end
end
