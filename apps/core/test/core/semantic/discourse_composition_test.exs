defmodule Core.Semantic.DiscourseCompositionTest do
  use ExUnit.Case, async: true

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Discourse.References
  alias Core.Semantic.{Composition, DiscourseComposition, SentenceMeaning}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp session, do: "discourse-lambda-#{System.unique_integer([:positive])}"

  defp input(words, pos, session) do
    rows =
      Enum.zip(words, pos)
      |> Enum.map(fn {word, part} ->
        surface = String.downcase(word)
        %{id: "#{surface}|#{part}|1", norm: surface, pos: part,
          semantic_atoms: if(word == "sitting",
            do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
      end)

    %Core.SemanticInput{
      session_id: session,
      sentence: Enum.join(words, " ") <> ".",
      source: :test,
      intent: :unknown,
      tokens: words |> Enum.with_index()
        |> Enum.map(fn {word, index} -> %{phrase: word, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices: rows |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{id: row.id, token_index: index} end)
    }
  end

  defp original(session) do
    input(~w(The small black bird),
      ~w(determiner adjective adjective noun), session)
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  defp followup(session) do
    first = original(session)
    assert {:ok, _} = Composition.verified(first)

    context = WMComprehension.context_from(first, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })

    input(~w(It is sitting on the fence),
      ~w(pronoun verb verb preposition determiner noun), session)
    |> Map.put(:wm_context, context)
    |> References.attach()
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> SentenceMeaning.attach()
  end

  test "cross-turn verified roles and two typed properties yield a provisional report" do
    si = followup(session())
    assert si.discourse_reference.antecedent == "small black bird"
    assert %{construction: :progressive_anaphoric_locative_candidate,
      span: {0, 5}} = si.structural_competition.preferred

    assert {:ok, meaning} = DiscourseComposition.verified(si)
    assert meaning.kind == :anaphoric_spatial_report
    assert meaning.status == :provisional
    assert meaning.source_frame == :progressive_anaphoric_locative_candidate
    assert meaning.antecedent.head == "bird"
    assert meaning.antecedent.modifiers == ["small", "black"]
    assert meaning.relation == :on
    assert meaning.event_semantics.predicate == :sit
    assert meaning.event_semantics.spatial.landmark_kind == :fence
    assert meaning.event_semantics.event_existence == :not_asserted
    assert meaning.compositional_evidence.nominal_property.formally_verified?
    assert meaning.compositional_evidence.event_property.formally_verified?
    assert meaning.compositional_evidence.typed_cross_turn_join_verified? == true
    assert meaning.compositional_evidence.typed_cross_turn_join.formally_verified?
    assert meaning.compositional_evidence.typed_cross_turn_join.type ==
      {:arrow, :entity, {:arrow, :event, :truth}}
    assert meaning.compositional_evidence.typed_cross_turn_join.quantifier_applied? == false
    assert meaning.compositional_evidence.typed_cross_turn_join.referent_identity == :unresolved
    assert meaning.factual_authority? == false
    assert :event_occurrence in meaning.unresolved
    assert :referent_identity in meaning.unresolved
    refute :typed_cross_turn_join in meaning.unresolved
    assert :ok = Core.SemanticInput.validate_contract(si)
  end

  test "a scored alternative participle can support role without changing LIFG" do
    si = followup(session())
    adjective = %{id: "sitting|adjective|1", norm: "sitting", pos: "adjective",
      semantic_atoms: []}

    contested = %{si |
      active_cells: [adjective | si.active_cells],
      lifg_choices: Enum.map(si.lifg_choices, fn
        %{token_index: 2} = choice ->
          choice
          |> Map.put(:id, adjective.id)
          |> Map.put(:candidate_probs, %{
            adjective.id => 0.12, "sitting|verb|1" => 0.11
          })
        choice -> choice
      end)
    }
    # Rebuild the current structural competition; it must remain unchanged
    # by the event-specific lexical alternative.
    out =
      contested
      |> StructuralCompetition.attach()
      |> SentenceMeaning.attach()

    assert {:ok, proof} = DiscourseComposition.verified(out)
    predicate = Enum.find(
      proof.compositional_evidence.lexical_witnesses, &(&1.role == :predicate))
    assert predicate.selection_basis == :construction_alternative
    assert predicate.original_lifg_id == adjective.id
    assert predicate.sense_id == "sitting|verb|1"
    assert proof.factual_authority? == false

    changed = %{out | lifg_choices: Enum.map(out.lifg_choices, fn
      %{token_index: 2} = choice ->
        put_in(choice, [:candidate_probs, "sitting|verb|1"], 0.09)
      choice -> choice
    end)}
    assert DiscourseComposition.verified(changed) == :not_applicable
  end

  test "forged typed join evidence cannot pass a revalidated sentence meaning" do
    si = followup(session())
    assert {:ok, _} = DiscourseComposition.verified(si)

    counterfeit =
      update_in(si, [Access.key!(:sentence_meaning),
        Access.key!(:compositional_evidence),
        Access.key!(:typed_cross_turn_join)], fn proof ->
        %{proof | formally_verified?: false}
      end)

    assert DiscourseComposition.verified(counterfeit) == :not_applicable
    assert DiscourseComposition.build(counterfeit) == si.sentence_meaning

    changed_link =
      update_in(si, [Access.key!(:sentence_meaning),
        Access.key!(:compositional_evidence),
        Access.key!(:typed_cross_turn_join),
        Access.key!(:result)], fn _ -> {:var, "forged"} end)

    assert DiscourseComposition.verified(changed_link) == :not_applicable
  end

  test "missing morphology, altered antecedent or absent session must abstain" do
    si = followup(session())

    untagged = %{si | active_cells: Enum.map(si.active_cells, fn
      %{norm: "sitting"} = row -> %{row | semantic_atoms: []}
      row -> row
    end)}
    assert DiscourseComposition.build(untagged) == nil

    missing_sense = %{si |
      active_cells: Enum.reject(si.active_cells, &(&1.norm == "sitting"))
    }
    assert DiscourseComposition.build(missing_sense) == nil

    fake = %{si | discourse_reference:
      %{si.discourse_reference | antecedent_head: "cat"}}
    assert DiscourseComposition.verified(fake) == :not_applicable

    unrelated_session = %{si | session_id: session()}
    assert DiscourseComposition.verified(unrelated_session) == :not_applicable

    assistant = %{si | source: :assistant}
    assert DiscourseComposition.verified(assistant) == :not_applicable
  end

  test "unscoped and conversationally unrelated phrases do not bind the bird" do
    si = followup(session())

    assert DiscourseComposition.build(%{si |
      discourse_reference: nil}) == nil

    changed_sentence = %{si | sentence: "It is sitting on the table."}
    assert DiscourseComposition.verified(changed_sentence) == :not_applicable

    question = %{si | sentence: "Is it sitting on the fence?"}
    assert DiscourseComposition.build(question) == nil

    changed_structure = %{si | structural_competition:
      Map.put(si.structural_competition, :status, :unresolved)}
    assert DiscourseComposition.build(changed_structure) == nil
  end
end
