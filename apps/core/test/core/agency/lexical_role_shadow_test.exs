defmodule Core.Agency.LexicalRoleShadowTest do
  use ExUnit.Case, async: true

  alias Core.Agency.LexicalRoleShadow
  alias Core.Semantic.Composition
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input do
    words = ~w(The bird is flying into the tree)
    positions = ~w(determiner noun verb verb preposition determiner noun)

    cells =
      words
      |> Enum.zip(positions)
      |> Enum.map(fn {word, pos} ->
        norm = String.downcase(word)

        %{
          id: "#{norm}|#{pos}|1",
          norm: norm,
          pos: pos,
          semantic_atoms:
            if(norm == "flying", do: ["tag:form-of", "tag:present", "tag:participle"],
              else: [])
        }
      end)

    choices =
      cells
      |> Enum.with_index()
      |> Enum.map(fn {cell, index} ->
        %{token_index: index, id: cell.id}
      end)

    adjective = %{id: "flying|adjective|1", norm: "flying", pos: "adjective"}

    choices =
      Enum.map(choices, fn
        %{token_index: 3} = choice ->
          choice
          |> Map.put(:id, adjective.id)
          |> Map.put(:candidate_probs,
            %{"flying|adjective|1" => 0.70, "flying|verb|1" => 0.30})

        choice ->
          choice
      end)

    %Core.SemanticInput{
      source: :test,
      sentence: Enum.join(words, " ") <> ".",
      tokens: Enum.with_index(words, fn word, index ->
        %{phrase: word, index: index, n: 1}
      end),
      active_cells: [adjective | cells],
      lifg_choices: choices
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  test "finds a scored and attested verb-role alternative without replacing LIFG" do
    si = input()
    original_choices = si.lifg_choices
    original_cells = si.active_cells
    original_proof = si.compositional_derivation

    assert {:ok, _} = StructuralCompetition.verified(si)

    report = LexicalRoleShadow.evaluate(si)

    assert report.status == :provisional_role_alternatives
    assert report.reason == :scored_attested_role_witnesses
    assert report.construction == :progressive_directional_candidate
    assert report.blocked_role_count == 1
    assert report.unresolved_role_count == 0
    assert [%{
      role: :predicate,
      token_index: 3,
      selected_pos: "adjective",
      candidate_pos: "verb",
      selection_basis: :construction_alternative,
      original_lifg_id: "flying|adjective|1",
      candidate_sense_id: "flying|verb|1",
      evidence: :current_turn_scored_attested_role_witness
    }] = report.alternatives

    assert report.formal_derivation_verified? ==
             match?({:ok, _}, Composition.verified(si))

    refute report.lexical_sense_verified?
    refute report.lifg_modified?
    refute report.learning_applied?
    refute report.factual_authority?
    assert si.lifg_choices == original_choices
    assert si.active_cells == original_cells
    assert si.compositional_derivation == original_proof
  end

  test "unscored dictionary alternative does not authorize reconsideration" do
    si = input()

    si = %{si | lifg_choices: Enum.map(si.lifg_choices, fn
      %{token_index: 3} = c -> Map.delete(c, :candidate_probs)
      c -> c
    end)}

    report = LexicalRoleShadow.evaluate(si)

    assert report.status == :abstained
    assert report.reason == :no_scored_attested_alternative
    assert report.blocked_role_count == 1
    assert report.alternatives == []
  end

  test "a morphology-incompatible verb cannot serve as a role alternative" do
    si = input()

    si = %{si | active_cells: Enum.map(si.active_cells, fn
      %{id: "flying|verb|1"} = row -> %{row | semantic_atoms: []}
      other -> other
    end)}

    report = LexicalRoleShadow.evaluate(si)
    # Structural detection itself may abstain once morphology is no longer
    # attested; either way, no alternative gains authority.
    assert report.status == :abstained
    assert report.alternatives == []
    refute report.lexical_sense_verified?
  end

  test "a tied alternative sense remains unresolved" do
    si = input()
    extra = %{id: "flying|verb|2", norm: "flying", pos: "verb",
              semantic_atoms: ["tag:form-of", "tag:present", "tag:participle"]}

    si = %{si | active_cells: [extra | si.active_cells],
      lifg_choices: Enum.map(si.lifg_choices, fn
        %{token_index: 3} = c ->
          %{c | candidate_probs: Map.put(c.candidate_probs, extra.id, 0.30)}
        c -> c
      end)}

    report = LexicalRoleShadow.evaluate(si)
    assert report.status == :abstained
    assert report.alternatives == []
    assert report.reason == :no_scored_attested_alternative
  end

  test "a second unresolved role prevents claiming full role reconsideration" do
    si = input()

    choices = Enum.map(si.lifg_choices, fn
      %{token_index: 6} = c ->
        %{c | id: "tree|proper_noun|1"}
      c -> c
    end)

    # The previous structural result was computed for different LIFG choices.
    # A changed winner must invalidate the old result rather than bypassing
    # structural verification.
    stale = %{si | lifg_choices: choices}
    stale_report = LexicalRoleShadow.evaluate(stale)
    assert stale_report.status == :abstained
    assert stale_report.reason == :structure_not_independently_verified

    # Rebuild the evidence for this exact two-conflict state, as the live
    # pipeline does after LIFG chooses its winners.
    si =
      stale
      |> StructuralCompetition.attach()
      |> Composition.attach()

    assert {:ok, _} = StructuralCompetition.verified(si)
    report = LexicalRoleShadow.evaluate(si)

    assert report.status == :partial_role_alternatives
    assert report.blocked_role_count == 2
    assert report.unresolved_role_count == 1
    assert length(report.alternatives) == 1
    refute report.learning_applied?
  end

  test "unrevalidated structural claims cannot yield alternative authority" do
    si = input()
    forged = %{si | structural_competition: %{status: :preferred,
      preferred: %{construction: :progressive_directional_candidate}}}

    report = LexicalRoleShadow.evaluate(forged)
    assert report.status == :abstained
    assert report.reason == :structure_not_independently_verified
    assert report.alternatives == []
  end

  test "default-on trace records evidence despite a caller's disable flag" do
    si = input()

    default_trace = LexicalRoleShadow.attach(si)
    disabled_attempt = LexicalRoleShadow.attach(si, reflective_role_shadow?: false)

    assert Enum.any?(default_trace.trace, &match?(%{stage: :reflective_role_shadow}, &1))
    assert Enum.any?(disabled_attempt.trace, &match?(%{stage: :reflective_role_shadow}, &1))

    attached = default_trace

    assert attached.lifg_choices == si.lifg_choices
    assert attached.active_cells == si.active_cells
    assert attached.compositional_derivation == si.compositional_derivation
    assert attached.structural_competition == si.structural_competition
    assert :ok == Core.SemanticInput.validate_contract(attached)

    assert %{decision: :provisional_role_alternatives, meta: meta} =
      Enum.find(attached.trace, &(&1.stage == :reflective_role_shadow))

    assert meta.alternative_count == 1
    assert [%{role: :predicate, token_index: 3}] = meta.roles
    assert [%{role: :predicate, token_index: 3,
              reason: :selected_pos_conflict}] = meta.blocked_roles
    assert meta.selected_choices_sha256 ==
             LexicalRoleShadow.selected_choices_sha256(si)
    assert byte_size(meta.selected_choices_sha256) == 64
    refute meta.lifg_modified?
    refute meta.learning_applied?
    refute meta.lexical_sense_verified?
    refute Map.has_key?(meta, :candidate_sense_id)
  end

  test "default trace records structural abstention rather than inventing a decision" do
    si = input()
    changed = %{si | structural_competition: %{status: :preferred}}

    result = LexicalRoleShadow.attach(changed)
    assert %{decision: :abstained,
             reason: :structure_not_independently_verified,
             meta: %{alternative_count: 0, blocked_roles: []}} =
             Enum.find(result.trace, &match?(%{stage: :reflective_role_shadow}, &1))

    assert result.lifg_choices == si.lifg_choices
    refute result == changed
  end
end
