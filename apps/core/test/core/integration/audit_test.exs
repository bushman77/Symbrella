defmodule Core.Integration.AuditTest do
  use ExUnit.Case, async: true

  alias Core.Integration.Audit
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp input do
    choices = [
      %{token_index: 0, id: "he|pronoun|1"},
      %{token_index: 1, id: "bought|verb|1"},
      %{token_index: 2, id: "some|determiner|1"},
      %{token_index: 3, id: "bread|noun|2"}
    ]

    %{
      sentence: "He bought some bread.",
      source: :user,
      intent: :unknown,
      trace: [],
      tokens:
        ~w(He bought some bread.)
        |> Enum.with_index()
        |> Enum.map(fn {phrase, index} ->
          %{phrase: phrase, index: index, n: 1}
        end),
      active_cells: [
        %{id: "he|pronoun|1", norm: "he", pos: "pronoun"},
        %{
          id: "bought|verb|1",
          norm: "bought",
          pos: "verb",
          semantic_atoms: [
            "tag:form-of",
            "tag:past",
            "tag:participle"
          ],
          definition: "(form-of, participle, past) simple past and past participle of buy."
        },
        %{id: "some|determiner|1", norm: "some", pos: "determiner"},
        %{id: "bread|noun|2", norm: "bread", pos: "noun"}
      ],
      lifg_choices: choices,
      atl_slate: %{winners: choices}
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> EpisodicProjection.attach()
    |> SentenceMeaning.attach()
  end

  test "independently checks a valid provisional interpretation" do
    si = input()

    assert si.sentence_meaning.kind == :structural_hypothesis
    assert si.sentence_meaning.participants.predicate.lemma == "buy"

    assert %{
             status: :consistent,
             violations: [],
             checks: %{
               sentence_meaning: :pass,
               structural_preference: :pass,
               episodic_projection: :pass,
               provisional_authority: :pass
             },
             enforcement: :observe_only
           } = Audit.check(si)
  end

  test "detects a forged actor referent" do
    si = input()

    altered =
      put_in(
        si,
        [:sentence_meaning, :participants, :actor, :referent],
        "john"
      )

    audit = Audit.check(altered)

    assert audit.status == :inconsistent
    assert :sentence_meaning in audit.violations
    assert :provisional_authority in audit.violations
  end

  test "detects an injected episodic candidate" do
    si = input()
    altered = %{si | episodic_candidates: [%{entity: "john"}]}

    audit = Audit.check(altered)

    assert audit.status == :inconsistent
    assert :episodic_projection in audit.violations
  end

  test "detects tampered structural competition" do
    si = input()

    altered =
      put_in(
        si,
        [:structural_competition, :preferred, :construction],
        :arbitrary_claim
      )

    audit = Audit.check(altered)

    assert audit.status == :inconsistent
    assert :structural_preference in audit.violations
  end

  test "attachment preserves existing semantic evidence" do
    si = input()
    out = Audit.attach(si)

    assert out.integration_audit.status == :consistent
    assert out.sentence_meaning == si.sentence_meaning
    assert out.episodic_candidates == si.episodic_candidates
    assert out.lifg_choices == si.lifg_choices
    assert out.atl_slate == si.atl_slate
    assert hd(out.trace).stage == :integration_audit
  end
end
