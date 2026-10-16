defmodule Core.Tools.PluralPopulationGroundingTest do
  use ExUnit.Case, async: true

  alias Core.Response.{ReflectionLoop, ResponseFrame}
  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.Grounding

  @kamloops "https://www.wikidata.org/wiki/Q473209"
  @maple "https://www.wikidata.org/wiki/Q16740"

  defmodule DatedPopulation do
    def population("Kamloops BC", _opts),
      do: {:ok, proof("Kamloops BC", 97_902, 2021,
        "https://www.wikidata.org/wiki/Q473209")}

    def population("Maple Ridge BC", _opts),
      do: {:ok, proof("Maple Ridge BC", 90_990, 2021,
        "https://www.wikidata.org/wiki/Q16740")}

    defp proof(subject, value, year, url) do
      %{
        kind: :population, status: :source_supported, subject: subject,
        value: value, display: Integer.to_string(value),
        census_year: year, source_type: :wikidata, source_url: url,
        sources: [%{title: subject, provider: :wikidata, url: url}]
      }
    end
  end

  defmodule DifferentlyDatedPopulation do
    def population("Kamloops BC", opts),
      do: DatedPopulation.population("Kamloops BC", opts)

    def population("Maple Ridge BC", _opts),
      do: {:ok, %{
        kind: :population, status: :source_supported,
        subject: "Maple Ridge BC",
        value: 91_200, census_year: 2022,
        source_type: :wikidata,
        source_url: "https://www.wikidata.org/wiki/Q16740",
        sources: [%{title: "Maple Ridge BC", provider: :wikidata,
          url: "https://www.wikidata.org/wiki/Q16740"}]
      }}
  end

  defmodule MismatchedPopulation do
    def population(_subject, _opts),
      do: {:ok, %{
        kind: :population, status: :source_supported,
        subject: "Different place",
        value: 12_345, census_year: 2021,
        source_type: :wikidata,
        source_url: "https://www.wikidata.org/wiki/Q1111",
        sources: [%{title: "Different place", provider: :wikidata,
          url: "https://www.wikidata.org/wiki/Q1111"}]
      }}
  end

  defmodule PartialPopulation do
    def population("Kamloops BC", opts),
      do: DatedPopulation.population("Kamloops BC", opts)
    def population("Maple Ridge BC", _opts), do: {:error, :no_dated_statement}
  end

  defmodule UnavailablePopulation do
    def population(_, _), do: {:error, :no_dated_statement}
  end

  defmodule UndatedPopulation do
    def population("Kamloops BC", _opts),
      do: {:ok, %{
        kind: :population, status: :source_supported, subject: "Kamloops BC",
        value: 12_345, census_year: nil,
        source_url: "https://www.wikidata.org/wiki/Q473209",
        sources: [%{title: "Kamloops", provider: :wikidata,
          url: "https://www.wikidata.org/wiki/Q473209"}]
      }}

    def population(_, _opts), do: {:error, :no_dated_statement}
  end

  defmodule NoExcerpts do
    def search(_, _), do: {:ok, []}
  end

  defp context do
    frame = %{
      type: :location_question,
      domain: :geography,
      relation: :population,
      subjects: ["Kamloops BC", "Maple Ridge BC"],
      grounding_status: :unverified,
      antecedent_source: :wm_comprehension
    }

    %{
      user_text: "What are the populations of each?",
      session_id: "plural-grounding-test",
      features: %{
        intent: :ask, conf: 0.8, symbolic_frame: frame,
        discourse_reference: %{
          pronoun: "each", source: :wm_comprehension,
          antecedents: ["Kamloops BC", "Maple Ridge BC"]
        }
      },
      symbolic_frame: %{event: frame},
      decision: %{tone: :neutral, mode: :scribe, action: :answer},
      mood: %{}
    }
  end

  defp enrich(provider, opts \\ []) do
    WebRetrieval.enrich(context(), [
      enabled?: true, provider: NoExcerpts, coordinate_provider: provider
    ] ++ opts)
  end

  test "router recognizes a plural location frame without a scalar subject" do
    route = WebRetrieval.route_for(context())
    assert route.relation == :population
    assert route.request_type == :geographic_population
    assert route.trigger == :symbolic_frame
    assert route.query =~ "Kamloops BC"
    assert route.query =~ "Maple Ridge BC"
  end

  test "independent dated counts propagate through retrieval, response frame and reflection" do
    enriched = enrich(DatedPopulation)
    proof = enriched.web_evidence

    assert proof.status == :retrieved
    assert proof.resolution == :source_supported
    assert proof.support.kind == :population_group
    assert proof.support.subjects == ["Kamloops BC", "Maple Ridge BC"]
    assert proof.attempted_count == 2
    assert Enum.map(proof.support.results, &{&1.subject, &1.value, &1.census_year}) == [
      {"Kamloops BC", 97_902, 2021},
      {"Maple Ridge BC", 90_990, 2021}
    ]
    assert Enum.map(proof.sources, & &1.url) == [@kamloops, @maple]

    frame = ResponseFrame.build(enriched)
    assert frame.subject == nil
    assert frame.subjects == ["Kamloops BC", "Maple Ridge BC"]
    assert frame.evidence.kind == :population_group
    assert Enum.map(frame.evidence.entries, & &1.value) == [97_902, 90_990]

    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "verified_evidence=population_group"
    assert prompt =~ "97902"
    assert prompt =~ "90990"
    assert prompt =~ @kamloops
    assert prompt =~ @maple
    refute prompt =~ "subject=nil; target=nil; reference_source"

    {:ok, final, review} =
      ReflectionLoop.review(
        "What are the populations of each?",
        "Which place are you referring to?",
        enriched
      )

    assert review.status == :repair
    assert :geographic_claim_unverified in review.issues
    assert final =~ "Kamloops BC: 97,902 (2021)"
    assert final =~ "Maple Ridge BC: 90,990 (2021)"
    assert final =~ @kamloops
    assert final =~ @maple
    refute final =~ "Which place"
    assert Grounding.supports?(final, enriched)

    refute Grounding.supports?("Kamloops has 17,800 people, Maple Ridge 90,990.", enriched)
    refute Grounding.supports?("Kamloops 97,902 in 2026 and Maple Ridge 90,990.", enriched)
  end

  test "independent sources retain different dates without deriving a shared census year" do
    enriched = enrich(DifferentlyDatedPopulation)
    entries = enriched.web_evidence.support.results

    assert Enum.map(entries, & &1.census_year) == [2021, 2022]
    answer = Grounding.align("No data", enriched)
    assert answer =~ "Kamloops BC: 97,902 (2021)"
    assert answer =~ "Maple Ridge BC: 91,200 (2022)"
    refute answer =~ "2021 census for both"
    assert Grounding.supports?(answer, enriched)
  end

  test "mismatched entity proof is not attributed to a requested city" do
    enriched = enrich(MismatchedPopulation, max_attempts: 2)
    assert enriched.web_evidence.resolution == :exhausted
    assert Enum.all?(enriched.web_evidence.support.results,
      &(&1.status == :unavailable))
    answer = Grounding.align("Kamloops has 12,345 residents", enriched)
    refute answer =~ "12,345"
    assert answer =~ "no verified dated population"
  end

  test "partial lookup reports one verified figure and explicitly marks the other unavailable" do
    enriched = enrich(PartialPopulation, max_attempts: 2)
    evidence = enriched.web_evidence
    assert evidence.resolution == :partial_support
    assert Enum.map(evidence.support.results, & &1.status) ==
      [:source_supported, :unavailable]
    assert evidence.attempted_count == 3

    frame = ResponseFrame.build(enriched)
    assert frame.evidence.kind == :population_group
    assert Enum.at(frame.evidence.entries, 1).status == :unavailable
    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "unavailable_subjects"
    assert prompt =~ "Maple Ridge BC"
    refute prompt =~ "90990"

    {:ok, reply, reviewed} =
      ReflectionLoop.review(
        "What are the populations of each?",
        "Both cities have 97,902 residents.", enriched
      )
    assert reviewed.status == :repair
    assert reply =~ "Kamloops BC: 97,902 (2021)"
    assert reply =~ "Maple Ridge BC: no verified dated population figure was available."
    refute reply =~ "Maple Ridge BC: 97,902"
    assert Grounding.supports?(reply, enriched)
  end

  test "when neither place has dated evidence, never ask which place" do
    enriched = enrich(UnavailablePopulation, max_attempts: 2)
    assert enriched.web_evidence.resolution == :exhausted
    assert enriched.web_evidence.support.kind == :population_group
    assert enriched.web_evidence.sources == []

    frame = ResponseFrame.build(enriched)
    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "verified_evidence=none"
    assert prompt =~ "requested_population_subjects"

    {:ok, reply, reviewed} =
      ReflectionLoop.review(
        "What are the populations of each?",
        "Which place are you referring to?", enriched
      )

    assert reviewed.status == :repair
    assert reply =~ "Kamloops BC: no verified dated population"
    assert reply =~ "Maple Ridge BC: no verified dated population"
    refute reply =~ "Which place"
    assert Grounding.supports?(reply, enriched)
  end

  test "undated or malformed provider counts are not promoted to verified numbers" do
    enriched = enrich(UndatedPopulation, max_attempts: 2)
    assert enriched.web_evidence.resolution == :exhausted
    assert Enum.all?(enriched.web_evidence.support.results,
      &(&1.status == :unavailable))

    reply = Grounding.align("Kamloops has 12,345 people.", enriched)
    refute reply =~ "12,345"
    assert reply =~ "no verified dated population"
  end
end
