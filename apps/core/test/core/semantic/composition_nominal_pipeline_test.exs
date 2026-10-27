defmodule Core.Semantic.CompositionNominalPipelineTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition
  alias Core.Semantic.Composition.Proof
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
      tokens:
        words
        |> Enum.with_index()
        |> Enum.map(fn {word, index} -> %{phrase: word, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices:
        rows
        |> Enum.with_index()
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
  end

  test "three-word nominal phrase reaches structural preference and lambda proof" do
    input = input(~w(small black bird), ~w(adjective adjective noun))
    out = run(input)

    assert Enum.any?(out.sentence_signatures, fn sig ->
      sig.id == :noun_phrase and sig.span == {0, 2} and
        sig.roles == %{modifier_1: 0, modifier_2: 1, head: 2}
    end)

    assert {:ok, %{preferred: preferred}} = StructuralCompetition.verified(out)
    assert preferred.construction == :noun_phrase
    assert preferred.span == {0, 2}
    assert preferred.matched_roles == 3
    assert preferred.conflicting_roles == 0

    assert {:ok, proof} = Composition.verified(out)
    assert proof.kind == :nominal_property
    assert proof.construction == :stacked_adjective_noun
    assert proof.span == {0, 2}
    assert proof.result_type == {:arrow, :entity, :truth}
    assert length(proof.steps) == 4
    assert Proof.verify(proof) == :ok
    assert proof.factual_authority? == false
    assert proof.quantifier_applied? == false
    assert Enum.map(proof.premises.lexical_witnesses, & &1.word) ==
      ~w(small black bird)

    assert proof.result ==
      {:lambda, "x", :entity,
       {:and,
        {:and,
         {:predicate, :bird, {:var, "x"}},
         {:predicate, :black, {:var, "x"}}},
        {:predicate, :small, {:var, "x"}}}}

    assert :ok == Core.SemanticInput.validate_contract(out)
    assert out.lifg_choices == input.lifg_choices
  end

  test "determiner phrase admits the complete nominal but applies no quantifier" do
    out =
      input(~w(The small black bird),
        ~w(determiner adjective adjective noun))
      |> run()

    assert {:ok, %{preferred: preferred}} = StructuralCompetition.verified(out)
    assert preferred.roles ==
      %{determiner: 0, modifier_1: 1, modifier_2: 2, head: 3}

    assert {:ok, proof} = Composition.verified(out)
    assert proof.span == {0, 3}
    assert proof.construction == :stacked_adjective_noun
    assert proof.quantifier_applied? == false
    assert proof.factual_authority? == false
    assert Enum.map(proof.premises.lexical_witnesses, & &1.word) ==
      ~w(the small black bird)
    assert Proof.verify(proof) == :ok
  end

  test "selected nonadjective cannot be promoted merely by structural role" do
    si = input(~w(small black bird), ~w(adjective adjective noun))
    wrong_noun = %{id: "black|noun|1", norm: "black", pos: "noun", semantic_atoms: []}

    conflicting = %{si |
      active_cells: [wrong_noun | si.active_cells],
      lifg_choices: Enum.map(si.lifg_choices, fn
        %{token_index: 1} = choice -> %{choice | id: wrong_noun.id}
        choice -> choice
      end)
    }

    out = run(conflicting)
    assert out.structural_competition.status == :preferred
    assert out.structural_competition.preferred.conflicting_roles == 1
    assert out.compositional_derivation == nil
    assert Composition.verified(out) == :not_applicable
    assert Enum.find(out.lifg_choices, &(&1.token_index == 1)).id ==
      wrong_noun.id
  end

  test "missing independent adjective or determiner evidence cannot yield proof" do
    si = input(~w(The small black bird), ~w(determiner adjective adjective noun))

    missing_adj = %{si |
      active_cells: Enum.reject(si.active_cells, &(&1.norm == "black"))
    }

    assert run(missing_adj).compositional_derivation == nil

    missing_det = %{si |
      active_cells: Enum.reject(si.active_cells, &(&1.norm == "the"))
    }
    assert run(missing_det).compositional_derivation == nil
  end

  test "legacy single adjective noun composition remains available" do
    out = input(~w(black bird), ~w(adjective noun)) |> run()

    assert out.structural_competition.status == :no_complete_candidate
    assert {:ok, proof} = Composition.verified(out)
    assert proof.construction == :adjective_noun
    assert proof.span == {0, 1}
    assert proof.result_type == {:arrow, :entity, :truth}
    assert Proof.verify(proof) == :ok
    assert proof.factual_authority? == false
  end

  test "stale winners and tampered proofs fail current-turn verification" do
    si = input(~w(small black bird), ~w(adjective adjective noun))
    out = run(si)
    assert {:ok, original} = Composition.verified(out)

    stale = %{out | lifg_choices: Enum.map(out.lifg_choices, fn
      %{token_index: 0} = choice -> %{choice | id: "small|noun|1"}
      choice -> choice
    end)}
    assert Composition.verified(stale) == :not_applicable

    forged = %{original | result: {:var, "fake"}}
    assert Composition.verified(%{out | compositional_derivation: forged}) ==
      :not_applicable
  end

  test "embedded nominal constituent never wins as a complete sentence" do
    si = input(~w(small black bird runs),
      ~w(adjective adjective noun verb))
    out = run(si)

    refute out.structural_competition.status == :preferred
    refute match?(%{construction: :stacked_adjective_noun},
      out.compositional_derivation)
  end
end
