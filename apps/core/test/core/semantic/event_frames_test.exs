defmodule Core.Semantic.EventFramesTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.EventFrames

  test "lexical parsing of a population question does not grant source authority" do
    frame =
      EventFrames.location_question(
        "Which had the larger population in 2021, Vancouver or Burnaby?"
      )

    assert %{type: :location_question, domain: :geography,
             relation: :population, grounding_status: :unverified} = frame
    refute Map.has_key?(frame, :verified)
    refute Map.has_key?(frame, :sources)
    refute Map.has_key?(frame, :source_urls)
  end

  test "attaches forgot-medication frame with sleep inability consequence" do
    si = %{
      sentence: "good morning symbrella, i forgot my quetiapine and now i can not sleep",
      intent: :health_support,
      confidence: 0.85,
      topic_domain: :health_sleep_medication,
      lifg_choices: [
        %{token_index: 3, lemma: "quetiapine", id: "quetiapine|entity|medication", pos: "entity"}
      ],
      trace: []
    }

    out = EventFrames.attach(si)

    assert Map.drop(out.symbolic_frame, [:confidence]) == %{
             type: :health_support_event,
             subject: :user,
             event: :forgot_medication,
             medication: "quetiapine",
             consequence: :sleep_inability,
             temporal_context: :now,
             domain: :health_support,
             polarity: :negative
           }

    assert out.symbolic_frame.confidence == 0.95

    assert [%{stage: :event_frame, decision: :attached} | _] = out.trace
  end

  test "preserves the speaker as the agent of an obligation" do
    si = %{
      sentence: "I've got to leave.",
      tokens: [
        %{index: 0, phrase: "I've"},
        %{index: 1, phrase: "got"},
        %{index: 2, phrase: "to"},
        %{index: 3, phrase: "leave."}
      ],
      sentence_signatures: [
        %{id: :have_verb_to_infinitive,
          roles: %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3}}
      ],
      lifg_choices: [
        %{token_index: 1, id: "got|verb|1"},
        %{token_index: 3, id: "leave|noun|4"}
      ],
      trace: []
    }

    result = EventFrames.attach(si)
    assert result.symbolic_frame.subject == :speaker
    assert result.symbolic_frame.action == "leave"
    assert result.symbolic_frame.modality == :obligation
    assert result.symbolic_frame.uncertain_roles == [:complement]

    assert get_in(Core.Response.Context.symbolic_frame(result), [:event, :subject]) == :speaker

    context =
      Core.Response.Context.from_response_parts(
        "I've got to leave.",
        %{symbolic_frame: result.symbolic_frame,
          turn_context: Core.Response.Context.from_si(result)},
        %{},
        %{},
        %{}
      )

    assert get_in(context, [:symbolic_frame, :event, :subject]) == :speaker
  end

  # Synthetic offline provider: tests routing and evidence propagation,
  # not the truth of any actual municipality's population.
  defmodule DatedPopulation do
    def population(subject, _opts) when subject in ["Vancouver", "Burnaby"] do
      count = if subject == "Vancouver", do: 100_000, else: 80_000
      url = "https://example.invalid/population/#{subject}/2021"

      {:ok, %{
        kind: :population, status: :source_supported,
        subject: subject, value: count, census_year: 2021,
        source_type: :wikidata, source_url: url,
        sources: [%{title: subject, url: url, provider: :wikidata}]
      }}
    end

    def population(_, _), do: {:error, :not_found}
  end

  defmodule WrongYearPopulation do
    def population(subject, opts) do
      case Core.Semantic.EventFramesTest.DatedPopulation.population(subject, opts) do
        {:ok, result} ->
          {:ok, %{result | census_year: 2022}}

        error -> error
      end
    end
  end

  defmodule NoSources do
    def search(_, _), do: {:ok, []}
  end

  defp named_comparison_context(question) do
    alias Core.Response.Context

    si = EventFrames.attach(%{sentence: question, intent: :ask, trace: []})
    Context.from_response_parts(
      question,
      %{intent: :ask, session_id: "explicit-city-grounding", turn_context: Context.from_si(si)},
      %{action: :answer}, %{}, %{wm_items: []}
    )
  end

  test "real chat-shaped named comparison triggers two-source proof with offline provider" do
    alias Core.Response.{AuthoritativeAnswerPlan, PopulationComparison, ResponseFrame}
    alias Core.Tools.WebRetrieval

    question = "Which had the larger population in 2021, Vancouver or Burnaby?"
    context = named_comparison_context(question)

    enriched = WebRetrieval.enrich(context,
      enabled?: true,
      coordinate_provider: DatedPopulation,
      provider: NoSources
    )

    assert enriched.web_evidence.resolution == :source_supported
    assert enriched.web_evidence.support.subjects == ["Vancouver", "Burnaby"]
    assert Enum.map(enriched.web_evidence.support.results, & &1.census_year) == [2021, 2021]

    verified = PopulationComparison.attach(enriched)
    assert verified.web_evidence.support.kind == :population_comparison
    assert verified.web_evidence.support.difference == 20_000

    frame = ResponseFrame.build(verified)
    assert %{kind: :verified_population_comparison} =
      AuthoritativeAnswerPlan.build(Map.put(verified, :response_frame, frame))
  end

  test "explicit 2021 request rejects a provider returning 2022 instead" do
    alias Core.Response.PopulationComparison
    alias Core.Tools.WebRetrieval

    context = named_comparison_context(
      "Which had the larger population in 2021, Vancouver or Burnaby?"
    )

    result = WebRetrieval.enrich(context,
      enabled?: true,
      coordinate_provider: WrongYearPopulation,
      provider: NoSources
    )

    assert result.web_evidence.resolution != :source_supported
    refute Map.has_key?(PopulationComparison.attach(result).web_evidence.support,
      :winner)
  end

  test "directly named population comparison questions preserve both explicit entities" do
    cases = [
      {"Which had the larger population in 2021, Vancouver or Burnaby?",
       ["Vancouver", "Burnaby"]},
      {"Which city had the larger population in 2021, Vancouver or Port Moody?",
       ["Vancouver", "Port Moody"]},
      {"Which town has the higher population in 2021, Port Moody or Burnaby?",
       ["Port Moody", "Burnaby"]},
      {"Which had the larger population in 2021, Burnaby or New Westminster?",
       ["Burnaby", "New Westminster"]},
      {"Which had the larger population in 2021, Vancouver or New Westminster?",
       ["Vancouver", "New Westminster"]},
      {"Which had the larger population in 2021, Kamloops, BC or Maple Ridge,  BC",
       ["Kamloops, BC", "Maple Ridge, BC"]}
    ]

    Enum.each(cases, fn {question, expected} ->
      assert %{type: :location_question, relation: :population,
               comparison: :greater, subjects: ^expected,
               grounding_status: :unverified,
               antecedent_source: :explicit_user_question} =
        EventFrames.location_question(question)

      si = EventFrames.attach(%{sentence: question, intent: :ask, trace: []})
      assert si.symbolic_frame.subjects == expected
      assert si.symbolic_frame.comparison == :greater
    end)
  end

  test "explicit city comparison travels to retrieval routing without invented counts" do
    alias Core.Response.{Context, PopulationComparison}
    alias Core.Tools.WebRetrieval

    question = "Which had the larger population in 2021, Vancouver or Burnaby?"
    si = EventFrames.attach(%{sentence: question, intent: :ask, trace: []})
    embedded = Context.from_si(si)

    context = Context.from_response_parts(
      question,
      %{intent: :ask, session_id: "explicit-city-frame", turn_context: embedded},
      %{action: :answer},
      %{},
      %{wm_items: []}
    )

    assert PopulationComparison.comparison_subjects(context) == ["Vancouver", "Burnaby"]
    assert %{relation: :population, request_type: :geographic_population,
             trigger: :symbolic_frame} = WebRetrieval.route_for(context)
    refute Map.has_key?(context, :web_evidence)
  end

  test "question parsing cannot manufacture comparisons without two explicit names" do
    refute EventFrames.location_question(
      "Which had the larger population in 2021, Vancouver?"
    )

    refute EventFrames.location_question(
      "Which had the larger population in 2021, Vancouver or what?"
    )

    refute EventFrames.location_question(
      "Which had the larger population in 2021, Vancouver or Vancouver?"
    )

    refute EventFrames.location_question(
      "Which city had the larger population in 2021, Vancouver?"
    )

    refute EventFrames.location_question(
      "Which city had the larger population in 2021, Vancouver or why?"
    )
  end

  test "does not attach frame for unrelated text" do
    si = %{sentence: "can you explain working memory", intent: :ask, trace: []}

    refute Map.has_key?(EventFrames.attach(si), :symbolic_frame)
  end
end
