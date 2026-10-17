defmodule Core.Response.PopulationComparisonTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.Comprehension
  alias Core.Semantic.EventFrames
  alias Core.Response.{PopulationComparison, VerifiedPopulation, ResponseFrame, ReflectionLoop}
  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.Grounding

  @cities ["Kamloops BC", "Maple Ridge BC"]
  @urls [
    "https://www.wikidata.org/wiki/Q473209",
    "https://www.wikidata.org/wiki/Q16740"
  ]

  defmodule NoExcerpts do
    def search(_, _), do: {:ok, []}
  end

  defmodule KnownPopulation do
    def population(subject, _) do
      data =
        case subject do
          "Kamloops BC" -> {97_902, 2021, "https://www.wikidata.org/wiki/Q473209"}
          "Maple Ridge BC" -> {90_990, 2021, "https://www.wikidata.org/wiki/Q16740"}
        end

      {count, year, url} = data

      {:ok, %{
        kind: :population,
        status: :source_supported,
        subject: subject,
        value: count,
        display: Integer.to_string(count),
        census_year: year,
        source_url: url,
        source_type: :wikidata,
        sources: [%{title: subject, url: url, provider: :wikidata}]
      }}
    end
  end

  defp session, do: "population-reasoning-#{System.unique_integer([:positive])}"

  defp question_context(session_id, frame) do
    %{
      session_id: session_id,
      user_text: "Which of those cities has the larger population?",
      features: %{intent: :unknown, symbolic_frame: frame},
      symbolic_frame: %{event: frame},
      decision: %{action: :answer},
      mood: %{}
    }
  end

  defp frame do
    %{
      type: :location_question,
      domain: :geography,
      relation: :population,
      comparison: :greater,
      subjects: @cities,
      antecedent_source: :wm_comprehension,
      grounding_status: :unverified
    }
  end

  defp evidence(years \\ [2021, 2021], counts \\ [97_902, 90_990]) do
    results =
      Enum.zip([@cities, counts, years, @urls])
      |> Enum.map(fn {subject, value, year, url} ->
        %{
          subject: subject, status: :source_supported, value: value,
          census_year: year, source_url: url, source_type: :wikidata
        }
      end)

    %{
      status: :retrieved, resolution: :source_supported,
      relation: :population, provider: :wikidata,
      support: %{
        kind: :population_group, status: :source_supported,
        subjects: @cities, results: results
      },
      sources: Enum.map(results, fn r ->
        %{title: r.subject, url: r.source_url, provider: :wikidata, excerpt: "Dated count"}
      end),
      attempted_count: 2, attempts: []
    }
  end

  test "demonstrative comparison keeps both named entities through three turns" do
    sid = session()

    first = %{
      session_id: sid, sentence: "How far is Kamloops BC from Maple Ridge BC?",
      intent: :ask, trace: [],
      symbolic_frame: %{type: :location_question, relation: :proximity,
        subject: "Kamloops BC", target: "Maple Ridge BC"},
      comprehension: %{degraded?: true, recognized_topics: ["distance"], uncertain: []}
    }

    Comprehension.store(first, comprehension_wm?: true)

    second =
      %{session_id: sid, sentence: "What are the populations of each?", trace: []}
      |> Comprehension.recall(comprehension_wm?: true)
      |> EventFrames.attach()

    assert second.symbolic_frame.subjects == @cities
    assert second.discourse_reference.antecedents == @cities

    second =
      Map.put(second, :comprehension,
        %{degraded?: true, recognized_topics: ["population"], uncertain: ["each"]})

    Comprehension.store(second, comprehension_wm?: true)

    third =
      %{session_id: sid, sentence: "Which of those cities has the larger population?", trace: []}
      |> Comprehension.recall(comprehension_wm?: true)
      |> EventFrames.attach()

    assert third.discourse_reference.source == :wm_comprehension
    assert third.discourse_reference.antecedents == @cities
    assert third.symbolic_frame.relation == :population
    assert third.symbolic_frame.comparison == :greater
    assert third.symbolic_frame.subjects == @cities

    third =
      Map.put(third, :comprehension,
        %{degraded?: true, recognized_topics: [], uncertain: ["which"]})

    stored = Comprehension.store(third, comprehension_wm?: true)
    assert stored.trace != []
    assert Comprehension.context_from(third, third.comprehension).entities == @cities

    unrelated =
      Comprehension.context_from(%{sentence: "My favorite game is Ultima.",
        wm_context: %{entities: @cities}, symbolic_frame: %{}},
        %{recognized_topics: [], uncertain: []})

    assert unrelated.entities == []
  end

  test "comparison survives SemanticInput to bounded response-context transport" do
    sid = session()
    si = %{
      session_id: sid,
      sentence: "Which of those cities has the larger population?",
      intent: :unknown,
      symbolic_frame: frame(),
      comprehension: %{degraded?: true, uncertain: ["which"]}
    }

    embedded = Core.Response.Context.from_si(si)
    assert embedded.symbolic_frame.event.comparison == :greater
    assert embedded.symbolic_frame.event.subjects == @cities

    context =
      Core.Response.Context.from_response_parts(
        si.sentence,
        %{turn_context: embedded, session_id: sid, intent: :unknown},
        %{action: :answer},
        %{},
        %{wm_items: []}
      )

    assert PopulationComparison.comparison_subjects(context) == @cities
  end

  test "retrieved claims survive in a separate same-session cache and generate a verified comparison" do
    sid = session()
    initial = question_context(sid, frame())
    first_evidence =
      WebRetrieval.enrich(initial,
        enabled?: true, provider: NoExcerpts, coordinate_provider: KnownPopulation)

    assert first_evidence.web_evidence.support.kind == :population_group
    assert first_evidence.web_evidence.resolution == :source_supported

    assert :ok = VerifiedPopulation.record(sid, first_evidence.web_evidence)
    remembered = VerifiedPopulation.recall(sid, @cities)
    assert remembered.support.kind == :population_group
    assert remembered.attempted_count == 0
    assert remembered.evidence_origin == :prior_verified_population

    assert PopulationComparison.recall(
      Map.put(initial, :user_text, "Which city had more residents in 2021?")
    ).web_evidence.evidence_origin == :prior_verified_population

    refute Map.has_key?(
      PopulationComparison.recall(
        Map.put(initial, :user_text, "Which city had more residents in 2022?")
      ), :web_evidence
    )

    refute Map.has_key?(
      PopulationComparison.recall(
        Map.put(initial, :user_text, "Which city currently has more residents?")
      ), :web_evidence
    )

    comparative =
      initial
      |> PopulationComparison.recall()
      |> PopulationComparison.attach()

    assert comparative.web_evidence.support.kind == :population_comparison
    assert comparative.web_evidence.support.winner == "Kamloops BC"
    assert comparative.web_evidence.support.difference == 6_912
    assert comparative.web_evidence.support.year == 2021
    assert comparative.web_evidence.attempted_count == 0

    answer = Grounding.align("Maple Ridge has more people.", comparative)
    assert answer =~ "Kamloops BC: 97,902 (2021)"
    assert answer =~ "Maple Ridge BC: 90,990 (2021)"
    assert answer =~ "Kamloops BC has the larger recorded population by 6,912"
    assert Enum.all?(@urls, &String.contains?(answer, &1))
    assert Grounding.supports?(answer, comparative)
    refute Grounding.supports?("Maple Ridge BC has more people.", comparative)

    response_frame = ResponseFrame.build(comparative)
    assert response_frame.comparison == :greater
    assert response_frame.evidence.kind == :population_comparison
    prompt = ResponseFrame.prompt(response_frame)
    assert prompt =~ "verified_evidence=population_comparison"
    assert prompt =~ "difference=6912"

    {:ok, repaired, review} =
      ReflectionLoop.review("Which of those cities has the larger population?",
        "Maple Ridge is bigger.", comparative)

    assert :geographic_claim_unverified in review.issues
    assert review.status == :repair
    assert repaired == answer
  end

  test "equal verified same-year population values produce a tie rather than a false winner" do
    result =
      question_context(session(), frame())
      |> Map.put(:web_evidence, evidence([2021, 2021], [90_990, 90_990]))
      |> PopulationComparison.attach()

    assert result.web_evidence.support.outcome == :equal
    assert result.web_evidence.support.winner == nil
    assert result.web_evidence.support.difference == 0

    answer = Grounding.align("Kamloops is larger.", result)
    assert answer =~ "Both places have the same recorded population"
    refute answer =~ "has the larger recorded population by"
    assert Grounding.supports?(answer, result)
  end

  test "cache isolation, expiry and entity-specific lookups prevent cross-session evidence reuse" do
    sid = session()
    assert :ok = VerifiedPopulation.record(sid, evidence(), now_ms: 100_000)

    assert VerifiedPopulation.recall(sid, @cities, now_ms: 100_500)
    refute VerifiedPopulation.recall(session(), @cities, now_ms: 100_500)
    refute VerifiedPopulation.recall(sid, ["Kamloops BC", "Vancouver BC"],
      now_ms: 100_500)
    refute VerifiedPopulation.recall(sid, @cities, now_ms: 800_001)
  end

  test "unequal years, partial evidence and tampered rankings never become verified comparisons" do
    context = question_context(session(), frame())

    different =
      context
      |> Map.put(:web_evidence, evidence([2021, 2022]))
      |> PopulationComparison.attach()

    assert different.web_evidence.support.kind == :population_group
    assert different.web_evidence.comparison_status == :different_reporting_years
    warning = Grounding.align("Kamloops is bigger.", different)
    assert warning =~ "reporting years differ"
    refute warning =~ "has the larger recorded population by"

    partial_proof = evidence()
    results = partial_proof.support.results

    incomplete =
      put_in(partial_proof, [:support, :results],
        [hd(results), %{subject: "Maple Ridge BC", status: :unavailable}])

    partial =
      context
      |> Map.put(:web_evidence, %{incomplete |
        resolution: :partial_support,
        support: %{incomplete.support | status: :partial_support}})
      |> PopulationComparison.attach()

    assert partial.web_evidence.comparison_status == :insufficient_verified_values
    assert Grounding.align("Kamloops is bigger.", partial) =~
      "do not establish a comparison"

    valid =
      context
      |> Map.put(:web_evidence, evidence())
      |> PopulationComparison.attach()

    bad =
      put_in(valid, [:web_evidence, :support, :winner], "Maple Ridge BC")

    refute Grounding.available_support?(bad)
    refute Grounding.supports?("Kamloops is bigger.", bad)

    undated = evidence([2021, nil])
    sid = session()
    assert :ok = VerifiedPopulation.record(sid, undated)
    refute VerifiedPopulation.recall(sid, @cities)

    undated_context =
      context
      |> Map.put(:web_evidence, undated)
      |> PopulationComparison.attach()

    assert undated_context.web_evidence.support.kind == :population_group
  end
end
