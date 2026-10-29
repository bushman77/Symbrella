defmodule Core.Response.SymbolicCompositionalEventTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Discourse.Ledger
  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{Composition, SentenceMeaning}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  setup do
    before = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)

    on_exit(fn ->
      if before == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, before)
      end
    end)

    :ok
  end

  defp input(actor \\ "bird", predicate \\ "flying",
             relation \\ "into", landmark \\ "tree") do
    words = ["The", actor, "is", predicate, relation, "the", landmark]
    pos = ["determiner", "noun", "verb", "verb",
           "preposition", "determiner", "noun"]

    rows =
      Enum.zip(words, pos)
      |> Enum.map(fn {word, category} ->
        surface = String.downcase(word)
        %{
          id: "#{surface}|#{category}|1",
          norm: surface,
          pos: category,
          semantic_atoms: if(word == predicate,
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])
        }
      end)

    %Core.SemanticInput{
      sentence: Enum.join(words, " ") <> ".",
      source: :test,
      intent: :statement,
      confidence: 0.42,
      session_id: "compositional-event-#{System.unique_integer([:positive])}",
      tokens:
        words |> Enum.with_index()
        |> Enum.map(fn {word, index} ->
          %{index: index, phrase: word, n: 1}
        end),
      active_cells: rows,
      lifg_choices:
        rows |> Enum.with_index()
        |> Enum.map(fn {row, index} ->
          %{token_index: index, id: row.id}
        end)
    }
  end

  defp run(si) do
    si
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
    |> SentenceMeaning.attach()
  end

  defp with_live_conflicts(si) do
    adjective = %{
      id: "flying|adjective|1",
      norm: "flying",
      pos: "adjective",
      semantic_atoms: []
    }

    choices = Enum.map(si.lifg_choices, fn
      %{token_index: 2} = choice ->
        Map.merge(choice, %{
          id: "is|auxiliary|0",
          candidate_probs: %{
            "is|auxiliary|0" => 0.337,
            "is|verb|1" => 0.270
          }
        })

      %{token_index: 3} = choice ->
        Map.merge(choice, %{
          id: "flying|adjective|1",
          candidate_probs: %{
            "flying|adjective|1" => 0.113,
            "flying|verb|1" => 0.096
          }
        })

      choice -> choice
    end)

    %{si | active_cells: [adjective | si.active_cells], lifg_choices: choices}
  end

  test "live-shaped lexical reconsideration reaches SentenceMeaning and response" do
    si = input() |> with_live_conflicts() |> run()

    assert %{kind: :progressive_event_property, status: :derived} =
      si.compositional_derivation

    meaning = si.sentence_meaning
    assert meaning == SentenceMeaning.build(si)
    assert meaning.compositional_evidence.formally_verified?
    assert meaning.compositional_evidence.event_semantics.spatial.arrival ==
      :not_established
    assert meaning.compositional_evidence.factual_authority? == false

    assert %{operation: :explain_reported_event, status: :eligible,
      realizer: :symbolic_generation} = plan = SemanticStrategy.select(si)

    assert {:ok, response, :symbolic_generation, metadata} =
      SemanticStrategy.realize(
        plan, si.sentence,
        %{risk_bucket: :low, intent: :statement},
        %{action: :answer}, si
      )

    assert response =~ "bird flying into a tree"
    assert response =~ "arrival and actual occurrence aren't established"
    assert metadata.compositional_proof_verified?
    assert metadata.event_occurrence_verified? == false
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false

    assert Enum.find(si.lifg_choices, &(&1.token_index == 3)).id ==
      "flying|adjective|1"
  end

  test "response attachment preserves proof and emits an LLM-free event explanation" do
    si = input() |> with_live_conflicts() |> run()
    projected = Core.Response.Attach.maybe_build_response_plan(si, [])

    assert projected.compositional_derivation == si.compositional_derivation
    assert projected.structural_competition == si.structural_competition
    assert projected.response_text =~ "bird flying into a tree"
    assert projected.response_meta.response_source == :symbolic_generation
    assert projected.response_meta.response_reflection.compositional_proof_verified?
    assert projected.response_meta.response_reflection.llm_called? == false
  end

  test "locative event uses independently checked stationary relation" do
    si = input("cat", "sleeping", "under", "table") |> run()

    assert si.sentence_meaning.compositional_evidence.formally_verified?
    assert {:ok, reply, reflection} =
      SymbolicGeneration.generate(
        si.sentence, %{risk_bucket: :low, intent: :statement},
        %{action: :answer}, si
      )

    assert reply =~ "cat sleeping under a table"
    assert reply =~ "not independent proof"
    assert reflection.event_occurrence_verified? == false
  end

  defp record_proved_report(session, actor, activity, relation, landmark) do
    si =
      input(actor, activity, relation, landmark)
      |> Map.put(:session_id, session)
      |> run()

    assert {:ok, %{kind: :progressive_event_property,
                   factual_authority?: false}} = Composition.verified(si)
    Ledger.record(si)
    si
  end

  test "two proved activity reports answer earlier versus latest without asserting identity" do
    session = "temporal-bird-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved_report(session, "bird", "sitting", "on", "fence")
    record_proved_report(session, "bird", "flying", "into", "tree")

    assert [%{kind: :reported_progressive_event, seq: 2, action: :fly,
              reading: :directional, arrival: :not_established,
              event_occurrence_verified?: false, referent_verified?: false,
              factual_authority?: false},
            %{kind: :reported_progressive_event, seq: 1, action: :sit,
              reading: :locative, factual_authority?: false}] =
             Ledger.entries(session)

    assert {:warm, earlier, earlier_meta} =
             Response.memory_reply(%{
               text: "What was the bird doing?",
               intent: :ask, session_id: session, confidence: 0.9
             })

    assert earlier ==
             "Earlier, you described a bird sitting on a fence. This is report order, not verified event chronology."
    assert earlier_meta.memory_source == :same_session_progressive_report
    assert earlier_meta.requested_temporal_role == :earlier
    assert earlier_meta.actual_event_chronology_verified? == false
    assert earlier_meta.same_actor_identity_verified? == false
    assert earlier_meta.llm_called? == false

    assert {:warm, latest, latest_meta} =
             Response.memory_reply(%{
               text: "What is the bird doing now?",
               intent: :ask, session_id: session, confidence: 0.9
             })

    assert latest ==
             "You most recently described a bird flying into a tree. This does not verify current activity, arrival, or that it is the same individual."
    assert latest_meta.requested_temporal_role == :latest
    assert latest_meta.report_seq == 2
    assert latest_meta.event_occurrence_verified? == false

    assert {:ok, ^latest, _} =
             Core.Semantic.TemporalActivityRecall.reply(session,
               "what is the bird doing now")

    # Ordinary activity questions keep their pre-existing episodic reader.
    assert :not_applicable ==
             Core.Semantic.TemporalActivityRecall.reply(session,
               "What is the bird doing?")
  end

  test "report timeline is actor-scoped and session-scoped, not global recency" do
    alias Core.Semantic.TemporalActivityRecall
    session = "temporal-actors-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved_report(session, "bird", "sitting", "on", "fence")
    record_proved_report(session, "cat", "sleeping", "under", "table")
    record_proved_report(session, "bird", "flying", "into", "tree")

    assert {:ok, bird, _} =
             TemporalActivityRecall.reply(session, "What is the bird doing now?")
    assert bird =~ "a bird flying into a tree"
    refute bird =~ "cat"

    assert {:ok, cat, _} =
             TemporalActivityRecall.reply(session, "What is the cat doing now?")
    assert cat =~ "a cat sleeping under a table"

    assert :not_applicable ==
             TemporalActivityRecall.reply(session, "What was the cat doing?")
    assert :not_applicable ==
             TemporalActivityRecall.reply(session <> "-other", "What is the bird doing now?")
    assert :not_applicable ==
             TemporalActivityRecall.reply(session, "What is the dog doing now?")
    assert :not_applicable ==
             TemporalActivityRecall.reply(session, "What was the bird doing now?")
  end

  test "more than one earlier report is ambiguous and never guessed by recency" do
    alias Core.Semantic.TemporalActivityRecall
    session = "temporal-ambiguous-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)

    record_proved_report(session, "bird", "sitting", "on", "fence")
    record_proved_report(session, "bird", "sleeping", "under", "tree")
    record_proved_report(session, "bird", "flying", "into", "tree")

    assert {:ok, question, meta} =
             TemporalActivityRecall.reply(session, "What was the bird doing?")
    assert question =~ "several earlier descriptions"
    assert meta.requested_temporal_role == :ambiguous_earlier
    assert meta.retrieval_found? == false

    assert {:ok, latest, _} =
             TemporalActivityRecall.reply(session, "What is the bird doing now?")
    assert latest =~ "flying into a tree"
  end

  test "forged or unsupported composition cannot become a temporal report" do
    alias Core.Semantic.TemporalActivityRecall
    session = "temporal-forgery-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(session) end)
    si =
      input("bird", "flying", "into", "tree")
      |> Map.put(:session_id, session)
      |> run()

    tampered =
      put_in(si.compositional_derivation.event_semantics.spatial.arrival,
        :confirmed)
    assert :not_applicable == Composition.verified(tampered)
    Ledger.record(tampered)

    assert [%{kind: :uninterpreted}] = Ledger.entries(session)
    assert :not_applicable ==
             TemporalActivityRecall.reply(session, "What is the bird doing now?")

    Ledger.record(%{si | source: :assistant})
    assert length(Ledger.entries(session)) == 1
  end

  test "forged, stale, unscored, or question-shaped events never generate" do
    si = input() |> with_live_conflicts() |> run()
    options = %{risk_bucket: :low, intent: :statement}
    decision = %{action: :answer}

    tampered =
      put_in(si.sentence_meaning.compositional_evidence.event_semantics.spatial.arrival,
        :confirmed)

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, options, decision, tampered)

    altered_proof =
      put_in(si.compositional_derivation.event_semantics.spatial.arrival,
        :confirmed)

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, options, decision, altered_proof)

    missing_scores =
      %{si | lifg_choices: Enum.map(si.lifg_choices, fn
        %{token_index: 3} = choice -> Map.delete(choice, :candidate_probs)
        choice -> choice
      end)}

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, options, decision, missing_scores)

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, options, decision,
        %{si | source: :assistant})

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, options, %{action: :suppress}, si)

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, %{risk_bucket: :high, intent: :statement},
        decision, si)

    assert :not_applicable =
      SymbolicGeneration.generate(si.sentence, %{risk_bucket: :low, intent: :ask},
        decision, si)
  end
end
