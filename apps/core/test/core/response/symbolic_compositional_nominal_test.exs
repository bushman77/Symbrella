defmodule Core.Response.SymbolicCompositionalNominalTest do
  use ExUnit.Case, async: true

  alias Core.Response.{SemanticStrategy, SymbolicGeneration}
  alias Core.Semantic.{Composition, SentenceMeaning}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input(words, positions) do
    rows =
      Enum.zip(words, positions)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{pos}|1", norm: norm, pos: pos, semantic_atoms: []}
      end)

    %Core.SemanticInput{
      sentence: Enum.join(words, " ") <> ".",
      source: :test,
      intent: :unknown,
      tokens:
        words
        |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices:
        rows
        |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{token_index: index, id: row.id} end)
    }
  end

  defp run(si) do
    si
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
    |> SentenceMeaning.attach()
  end

  defp base do
    input(~w(The small black bird), ~w(determiner adjective adjective noun))
    |> run()
  end

  @features %{risk_bucket: :low, intent: :unknown}
  @decision %{action: :answer}

  test "verified stacked nominal property plans and realizes a conversation continuation" do
    si = base()
    assert {:ok, proof} = Composition.verified(si)
    assert proof.factual_authority? == false
    assert si.sentence_meaning.compositional_evidence.kind == :nominal_property
    assert si.sentence_meaning.compositional_evidence.formally_verified?

    assert %{operation: :clarify_nominal_description,
             status: :eligible, realizer: :symbolic_generation,
             missing: [:dialogue_goal]} = plan = SemanticStrategy.select(si)

    assert {:ok, reply, :symbolic_generation, metadata} =
      SemanticStrategy.realize(plan, si.sentence, @features, @decision, si)

    assert reply ==
      "You're describing a small black bird. What would you like to know about it?"
    assert metadata.strategy_operation == :clarify_nominal_description
    assert metadata.compositional_proof_verified?
    assert metadata.exact_dictionary_sense_verified? == false
    assert metadata.referent_verified? == false
    assert metadata.factual_claims? == false
    assert metadata.llm_called? == false
  end

  test "word-order remains compositional rather than sentence-template-specific" do
    si =
      input(~w(The white small cat), ~w(determiner adjective adjective noun))
      |> run()

    assert {:ok, proof} = Composition.verified(si)
    assert proof.construction == :stacked_adjective_noun
    assert {:ok, reply, metadata} =
      SymbolicGeneration.generate(si.sentence, @features, @decision, si)
    assert reply =~ "a white small cat"
    assert metadata.factual_claims? == false
  end

  test "stale or forged proofs cannot speak through nominal generation" do
    si = base()
    plan = SemanticStrategy.select(si)

    stale = %{si | lifg_choices: Enum.map(si.lifg_choices, fn
      %{token_index: 2} = choice -> %{choice | id: "black|noun|15"}
      choice -> choice
    end)}
    assert SemanticStrategy.select(stale) == :not_applicable
    assert SymbolicGeneration.generate(
      stale.sentence, @features, @decision, stale) == :not_applicable

    forged = %{si | compositional_derivation:
      %{si.compositional_derivation | result: {:var, "fake"}}}
    assert SemanticStrategy.select(forged) == :not_applicable
    assert SemanticStrategy.realize(plan, forged.sentence,
      @features, @decision, forged) == :not_applicable

    assert SymbolicGeneration.generate("Another sentence", @features,
      @decision, si) == :not_applicable
  end

  test "question-shaped, high-risk and disallowed actions do not realize" do
    si = base()
    question = %{si | sentence: "The small black bird?"}
    assert SemanticStrategy.select(question) == :not_applicable
    assert SymbolicGeneration.generate(
      question.sentence, @features, @decision, question) == :not_applicable

    assert SymbolicGeneration.generate(si.sentence,
      %{risk_bucket: :high, intent: :unknown}, @decision, si) == :not_applicable

    assert SemanticStrategy.realize(SemanticStrategy.select(si), si.sentence,
      @features, %{action: :suppress}, si) == :not_applicable
  end

  test "no whole-utterance nominal proof means no nominal response" do
    si =
      input(~w(The small black bird flies),
        ~w(determiner adjective adjective noun verb))
      |> run()

    assert si.compositional_derivation == nil
    assert SemanticStrategy.select(si) == :not_applicable
    assert SymbolicGeneration.generate(si.sentence,
      @features, @decision, si) == :not_applicable
  end
end
