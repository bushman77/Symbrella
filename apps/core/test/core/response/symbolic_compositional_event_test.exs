defmodule Core.Response.SymbolicCompositionalEventTest do
  use ExUnit.Case, async: false

  alias Core.Response
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
