defmodule Core.Semantic.CompositionTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition
  alias Core.Semantic.Composition.{Expression, Proof, Reducer}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input(modifier \\ "black", noun \\ "bird") do
    words = ["The", modifier, noun]
    ids = [
      "the|determiner|1",
      modifier <> "|adjective|1",
      noun <> "|noun|1"
    ]

    %Core.SemanticInput{
      sentence: Enum.join(words, " "),
      source: :test,
      tokens: Enum.with_index(words, fn phrase, index ->
        %{phrase: phrase, index: index, n: 1}
      end),
      active_cells: [
        %{id: Enum.at(ids, 0), norm: "the", pos: "determiner"},
        %{id: Enum.at(ids, 1), norm: modifier, pos: "adjective"},
        %{id: Enum.at(ids, 2), norm: noun, pos: "noun"}
      ],
      lifg_choices: Enum.with_index(ids, fn id, token_index ->
        %{id: id, token_index: token_index}
      end)
    }
  end

  defp run(input) do
    input
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  test "derives and replays a black bird property without asserting existence" do
    si = run(input())

    assert %{
      status: :derived,
      kind: :nominal_property,
      result_type: {:arrow, :entity, :truth},
      factual_authority?: false,
      quantifier_applied?: false,
      steps: steps
    } = si.compositional_derivation

    assert length(steps) == 2
    assert si.compositional_derivation.result ==
      {:lambda, "x", :entity,
       {:and, {:predicate, :bird, {:var, "x"}},
        {:predicate, :black, {:var, "x"}}}}

    assert Proof.verify(si.compositional_derivation) == :ok
    assert {:ok, _} = Composition.verified(si)
    assert :ok = Core.SemanticInput.validate_contract(si)

    meaning = Core.Semantic.SentenceMeaning.attach(si).sentence_meaning
    assert meaning.compositional_evidence.formally_verified?
    assert meaning.compositional_evidence.factual_authority? == false
    refute meaning.status in [:verified, :factually_verified]
    assert si.episodic_candidates == []
  end

  test "same reducer composes a different adjective and noun" do
    si = run(input("small", "cat"))
    assert si.compositional_derivation.result ==
      {:lambda, "x", :entity,
       {:and, {:predicate, :cat, {:var, "x"}},
        {:predicate, :small, {:var, "x"}}}}
    assert :ok = Proof.verify(si.compositional_derivation)
  end

  test "unmodeled adjective cannot gain semantic authority from POS alone" do
    si = run(input("blue", "bird"))
    assert si.compositional_derivation == nil
    assert Composition.verified(si) == :not_applicable
  end

  test "a dictionary row is required for every selected lexical role" do
    si = input()
    without_noun = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.pos == "noun"))}

    assert run(without_noun).compositional_derivation == nil
  end

  test "forged proof step is rejected by replay checker" do
    si = run(input())
    proof = si.compositional_derivation

    [first | rest] = proof.steps
    forged = %{proof | steps: [%{first | after: {:var, "forged"}} | rest]}

    assert {:error, _} = Proof.verify(forged)
    assert Composition.verified(%{si | compositional_derivation: forged}) ==
      :not_applicable
  end

  test "stale derivation fails if lexical selection changes after composition" do
    si = run(input())
    changed = %{si | lifg_choices: [
      %{token_index: 1, id: "black|noun|99"},
      %{token_index: 2, id: "bird|noun|1"}
    ]}

    assert Composition.verified(changed) == :not_applicable
  end

  test "typed application rejects a noun in an entity argument position" do
    {:ok, noun} = Expression.noun("bird")
    assert {:error, :incompatible_application} =
      Expression.type_of({:apply, noun, noun})
  end

  test "substitution alpha-renames rather than capturing a free variable" do
    term = {:lambda, "y", :entity, {:var, "x"}}
    replacement = {:var, "y"}

    assert Reducer.substitute(term, "x", replacement) ==
      {:lambda, "y_1", :entity, {:var, "y"}}
  end

  test "unsupported predicate symbols fail type checking" do
    assert {:error, :unapproved_predicate} =
      Expression.type_of(
        {:lambda, "x", :entity, {:predicate, :made_up_meaning, {:var, "x"}}})
  end
end
