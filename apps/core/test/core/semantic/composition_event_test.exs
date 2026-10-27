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

  test "abstention identifies conflicting POS winners without overriding LIFG" do
    si = input()
    conflicting =
      %{si | lifg_choices:
        Enum.map(si.lifg_choices, fn
          %{token_index: 3} = choice -> %{choice | id: "flying|adjective|1"}
          %{token_index: 6} = choice -> %{choice | id: "tree|proper_noun|1"}
          choice -> choice
        end)}

    out = run(conflicting)

    assert out.structural_competition.status == :preferred
    assert out.compositional_derivation == nil

    assert %{reason: :lexical_role_blocked, meta: %{blockers: blockers}} =
      Enum.find(out.trace, &(&1.stage == :semantic_composition))

    assert Enum.any?(blockers, fn blocker ->
      blocker.role == :predicate and blocker.token_index == 3 and
        blocker.selected_pos == "adjective" and blocker.expected_pos == ["verb"] and
        blocker.reason == :selected_pos_conflict and
        blocker.compatible_dictionary_pos?
    end)

    assert Enum.any?(blockers, fn blocker ->
      blocker.role == :location_head and blocker.token_index == 6 and
        blocker.selected_pos == "proper_noun" and blocker.expected_pos == ["noun"] and
        blocker.reason == :selected_pos_conflict and
        blocker.compatible_dictionary_pos?
    end)

    assert Enum.find(out.lifg_choices, &(&1.token_index == 3)).id ==
      "flying|adjective|1"
    assert Composition.verified(out) == :not_applicable
  end

  test "abstention distinguishes an unattested selected auxiliary" do
    si = input()
    rows = Enum.reject(si.active_cells, &(&1.norm == "is"))
    out = run(%{si | active_cells: rows})

    assert out.compositional_derivation == nil
    assert %{meta: %{blockers: blockers}} =
      Enum.find(out.trace, &(&1.stage == :semantic_composition))

    assert Enum.any?(blockers, fn blocker ->
      blocker.role == :auxiliary and blocker.token_index == 2 and
        blocker.reason == :selected_sense_not_attested and
        not blocker.compatible_dictionary_pos?
    end)
  end

  defp with_live_conflicts(si) do
    adjective = %{
      id: "flying|adjective|1",
      norm: "flying",
      pos: "adjective",
      semantic_atoms: []
    }

    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: 2} = choice ->
          Map.merge(choice, %{
            id: "is|auxiliary|0",
            scores: %{
              "is|auxiliary|0" => 0.337,
              "is|verb|1" => 0.270
            }
          })

        %{token_index: 3} = choice ->
          Map.merge(choice, %{
            id: "flying|adjective|1",
            scores: %{
              "flying|adjective|1" => 0.113,
              "flying|verb|1" => 0.096
            }
          })

        choice ->
          choice
      end)

    %{si | active_cells: [adjective | si.active_cells],
      lifg_choices: choices}
  end

  test "attested losing verb and auxiliary form a provisional proof without changing winners" do
    si = with_live_conflicts(input())
    out = run(si)

    assert out.structural_competition.status == :preferred
    assert out.compositional_derivation.kind == :progressive_event_property
    assert :ok = Proof.verify(out.compositional_derivation)
    assert {:ok, _} = Composition.verified(out)

    witnesses = out.compositional_derivation.premises.lexical_witnesses

    assert Enum.any?(witnesses, fn witness ->
      witness.role == :predicate and
        witness.sense_id == "flying|verb|1" and
        witness.original_lifg_id == "flying|adjective|1" and
        witness.selection_basis == :construction_alternative
    end)

    assert Enum.any?(witnesses, fn witness ->
      witness.role == :auxiliary and
        witness.sense_id == "is|verb|1" and
        witness.original_lifg_id == "is|auxiliary|0" and
        witness.selection_basis == :construction_alternative
    end)

    assert Enum.find(out.lifg_choices, &(&1.token_index == 3)).id ==
      "flying|adjective|1"
    assert Enum.find(out.lifg_choices, &(&1.token_index == 2)).id ==
      "is|auxiliary|0"

    assert out.compositional_derivation.event_semantics.spatial.arrival ==
      :not_established
    assert out.compositional_derivation.factual_authority? == false
  end

  test "admission audit reports all witnessed event roles" do
    out = input() |> with_live_conflicts() |> run()

    audit = Core.Semantic.Composition.Diagnosis.admission_audit(out)
    assert length(audit) == 7
    assert Enum.all?(audit, &(&1.status == :admitted))
    assert Enum.any?(audit, fn entry ->
      entry.role == :predicate and
        entry.selection_basis == :construction_alternative
    end)
  end

  test "audit distinguishes aggregate morphology from tagged scored sense" do
    si = with_live_conflicts(input())

    rows =
      Enum.map(si.active_cells, fn
        %{id: "flying|verb|1"} = cell -> %{cell | semantic_atoms: []}
        cell -> cell
      end)

    # Another attested verb row makes the word-level syntax legal.
    # It does NOT authorize a different, untagged scored sense.
    morphology_row = %{
      id: "flying|verb|2",
      norm: "flying",
      pos: "verb",
      semantic_atoms: ["tag:form-of", "tag:present", "tag:participle"]
    }

    out = run(%{si | active_cells: [morphology_row | rows]})

    assert out.structural_competition.status == :preferred
    assert out.compositional_derivation == nil

    audit = Core.Semantic.Composition.Diagnosis.admission_audit(out)
    assert Enum.any?(audit, fn entry ->
      entry.role == :predicate and
        entry.status == :rejected and
        entry.reason == :scored_role_morphology_missing and
        entry.scored_role_candidates == 1 and
        entry.exact_dictionary_witnesses == 1 and
        entry.morphology_qualified_witnesses == 0 and
        entry.any_dictionary_role_morphology?
    end)

    assert %{meta: %{admission_audit: ^audit}} =
      Enum.find(out.trace, &(&1.stage == :semantic_composition))
  end

  test "Core candidate_probs authorizes scored alternatives without Stage1 scores key" do
    si = with_live_conflicts(input())
    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: index} = choice when index in [2, 3] ->
          choice
          |> Map.put(:candidate_probs, Map.fetch!(choice, :scores))
          |> Map.delete(:scores)
        choice -> choice
      end)

    out = run(%{si | lifg_choices: choices})
    assert out.structural_competition.status == :preferred
    assert %{kind: :progressive_event_property} = out.compositional_derivation
    assert :ok = Proof.verify(out.compositional_derivation)
    assert {:ok, _} = Composition.verified(out)
    assert out.compositional_derivation.event_semantics.spatial.arrival ==
      :not_established

    witnesses = out.compositional_derivation.premises.lexical_witnesses
    assert Enum.any?(witnesses,
      &(&1.role == :predicate and &1.selection_basis == :construction_alternative))
    assert Enum.any?(witnesses,
      &(&1.role == :auxiliary and &1.selection_basis == :construction_alternative))

    assert Enum.find(out.lifg_choices, &(&1.token_index == 3)).id ==
      "flying|adjective|1"
  end

  test "missing Core candidate_probs blocks an unselected lexical alternative" do
    si = with_live_conflicts(input())
    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: 3} = choice -> Map.delete(choice, :scores)
        choice -> choice
      end)

    out = run(%{si | lifg_choices: choices})
    assert out.compositional_derivation == nil

    audit = Core.Semantic.Composition.Diagnosis.admission_audit(out)
    assert Enum.any?(audit, fn entry ->
      entry.role == :predicate and
        entry.status == :rejected and
        entry.reason == :missing_or_invalid_score_evidence
    end)
  end

  test "unscored dictionary alternative must not be promoted" do
    si = with_live_conflicts(input())

    choices = Enum.map(si.lifg_choices, fn
      %{token_index: 3} = choice ->
        %{choice | scores: %{"flying|adjective|1" => 0.113}}
      choice -> choice
    end)

    out = run(%{si | lifg_choices: choices})
    assert out.compositional_derivation == nil
  end

  test "forged scored alternative without matching dictionary sense is rejected" do
    si = with_live_conflicts(input())
    rows = Enum.reject(si.active_cells, &(&1.id == "flying|verb|1"))

    # The syntax detector must also abstain when its own morphology witness
    # disappears: score membership never substitutes for actual dictionary POS.
    out = run(%{si | active_cells: rows})
    assert out.compositional_derivation == nil
  end

  test "invalid participle morphology cannot yield a compositional verb" do
    si = with_live_conflicts(input())
    rows =
      Enum.map(si.active_cells, fn
        %{id: "flying|verb|1"} = row -> %{row | semantic_atoms: []}
        row -> row
      end)

    out = run(%{si | active_cells: rows})
    assert out.compositional_derivation == nil
  end

  test "proof is invalidated when a scored alternative changes" do
    out = input() |> with_live_conflicts() |> run()
    assert {:ok, _} = Composition.verified(out)

    changed = %{out | lifg_choices:
      Enum.map(out.lifg_choices, fn
        %{token_index: 3} = choice ->
          %{choice | scores: %{"flying|adjective|1" => 0.113}}
        choice -> choice
      end)}

    assert Composition.verified(changed) == :not_applicable
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
    forged_proof =
      put_in(si.compositional_derivation,
        [:event_semantics, :spatial, :arrival], :confirmed)
    injected = %{si | compositional_derivation: forged_proof}

    assert Event.verify(injected.compositional_derivation) ==
      {:error, :invalid_event_semantics}
    assert Composition.verified(injected) == :not_applicable
  end

  test "conflicting summaries cannot override the verified normal form" do
    assert {:ok, derivation} =
      Event.compose(%{
        predicate: "flying", actor: "bird",
        preposition: "into", landmark: "tree"
      })

    both = Map.put(derivation, :event_semantics, derivation.semantics)
    assert :ok = Event.verify(both)

    mismatched =
      put_in(both, [:event_semantics, :spatial, :arrival], :confirmed)

    assert Event.verify(mismatched) == {:error, :invalid_event_semantics}
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
