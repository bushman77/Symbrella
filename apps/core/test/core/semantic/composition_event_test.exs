defmodule Core.Semantic.CompositionEventTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition
  alias Core.Semantic.Composition.{Expression, Event, Proof}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input(actor \\ "bird", predicate \\ "flying",
             preposition \\ "into", landmark \\ "tree") do
    words = ["The", actor, "is", predicate, preposition, "the", landmark]
    roles = ["determiner", "noun", "verb", "verb", "preposition", "determiner", "noun"]

    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)

    rows =
      Enum.zip(words, roles)
      |> Enum.map(fn {word, pos} ->
        normalized = String.downcase(word)
        %{
          id: normalized <> "|" <> pos <> "|1",
          norm: normalized,
          pos: pos,
          semantic_atoms:
            if(word == predicate, do: ["tag:form-of", "tag:present", "tag:participle"], else: [])
        }
      end)

    %Core.SemanticInput{
      sentence: Enum.join(words, " ") <> ".",
      source: :test,
      tokens: tokens,
      active_cells: rows,
      lifg_choices: rows
        |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{token_index: index, id: row.id} end)
    }
  end

  defp run(si) do
    si
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  test "directional progressive sentence yields a checked event property, not arrival" do
    si = run(input())

    assert Enum.any?(si.sentence_signatures,
      &(&1.id == :progressive_directional_candidate and &1.span == {0, 6}))
    assert si.structural_competition.status == :preferred

    assert %{
      kind: :progressive_event_property,
      status: :derived,
      result_type: {:arrow, :event, :truth},
      factual_authority?: false,
      quantifier_applied?: false,
      event_semantics: semantics
    } = si.compositional_derivation

    assert semantics.predicate == :fly
    assert semantics.actor == %{kind: :bird, referent: :unresolved}
    assert semantics.spatial == %{
      relation: :into,
      landmark_kind: :tree,
      reading: :directional,
      arrival: :not_established
    }
    assert semantics.event_existence == :not_asserted
    assert si.compositional_derivation.steps != []
    assert Proof.verify(si.compositional_derivation) == :ok
    assert {:ok, _} = Composition.verified(si)
    assert :ok = Core.SemanticInput.validate_contract(si)

    meaning = Core.Semantic.SentenceMeaning.attach(si).sentence_meaning
    assert meaning.compositional_evidence.event_semantics == semantics
    assert meaning.compositional_evidence.factual_authority? == false
    refute meaning.status in [:verified, :factually_verified]
    assert si.episodic_candidates == []
  end

  test "locative progressive sentence composes using the same reducer" do
    si = run(input("cat", "sleeping", "under", "table"))

    assert si.structural_competition.status == :preferred
    assert si.compositional_derivation.construction == :progressive_locative_candidate
    assert si.compositional_derivation.event_semantics.spatial == %{
      relation: :under,
      landmark_kind: :table,
      reading: :locative,
      arrival: :not_established
    }
    assert :ok = Proof.verify(si.compositional_derivation)
  end

  test "into cannot be reinterpreted as arrival" do
    assert {:error, :unapproved_event_symbol} =
      Expression.type_of({:event_spatial, :arrived, :tree, {:var, "e"}},
        %{"e" => :event})

    assert :unsupported =
      Event.compose(%{predicate: "sitting", actor: "bird",
                      preposition: "into", landmark: "tree"})
  end

  test "unknown directional construction abstains rather than guessing" do
    si = run(input("bird", "flying", "towards", "tree"))
    assert si.compositional_derivation == nil
    assert Composition.verified(si) == :not_applicable
  end

  test "morphology must attest present participle" do
    si = input()
    rows = Enum.map(si.active_cells, fn cell ->
      if cell.norm == "flying", do: %{cell | semantic_atoms: []}, else: cell
    end)
    out = run(%{si | active_cells: rows})
    refute Enum.any?(out.sentence_signatures,
      &(&1.id == :progressive_directional_candidate))
    assert out.compositional_derivation == nil
  end

  test "missing current lexical witness blocks proof" do
    si = input()
    rows = Enum.reject(si.active_cells, &(&1.norm == "tree"))
    assert run(%{si | active_cells: rows}).compositional_derivation == nil
  end

  test "altered selected sense invalidates attached derivation" do
    si = run(input())
    changed = %{si | lifg_choices:
      Enum.map(si.lifg_choices, fn choice ->
        if choice.token_index == 4,
          do: %{choice | id: "into|noun|invalid"},
          else: choice
      end)}

    assert Composition.verified(changed) == :not_applicable
  end

  test "the semantic summary is projected from the normal form" do
    assert {:ok, derivation} =
      Event.compose(%{
        predicate: "flying", actor: "bird",
        preposition: "into", landmark: "tree"
      })

    semantics = derivation.semantics
    assert {:ok, ^semantics} = Event.project(derivation.result)
    assert :ok = Proof.verify(derivation)
    assert :ok = Event.verify(derivation)
  end

  test "mathematically valid proof cannot authorize a forged arrival summary" do
    assert {:ok, derivation} =
      Event.compose(%{
        predicate: "flying", actor: "bird",
        preposition: "into", landmark: "tree"
      })

    forged = put_in(derivation, [:semantics, :spatial, :arrival], :confirmed)

    # The pure lambda derivation is still valid: only its reported summary
    # was changed. The event-specific verifier MUST catch that mismatch.
    assert Proof.verify(forged) == :ok
    assert Event.verify(forged) == {:error, :invalid_event_semantics}

    si = run(input())
    injected =
      put_in(si, [:compositional_derivation, :event_semantics, :spatial, :arrival],
        :confirmed)

    assert Event.verify(injected.compositional_derivation) ==
      {:error, :invalid_event_semantics}
    assert Composition.verified(injected) == :not_applicable
  end

  test "normal form with extra unchecked event claims cannot be summarized" do
    assert {:ok, derivation} =
      Event.compose(%{
        predicate: "flying", actor: "bird",
        preposition: "into", landmark: "tree"
      })

    altered = {:lambda, "e", :event,
      {:and, elem(derivation.result, 3),
       {:event_actor_kind, :dog, {:var, "e"}}}}

    assert :unsupported = Event.project(altered)
  end

  test "forged event proof cannot pass replay verification" do
    si = run(input())
    proof = si.compositional_derivation
    [step | rest] = proof.steps
    forged = %{proof | steps: [%{step | after: {:var, "forged"}} | rest]}

    assert {:error, :invalid_derivation} = Proof.verify(forged)
    assert Composition.verified(%{si | compositional_derivation: forged}) ==
      :not_applicable
  end
end
