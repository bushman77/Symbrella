defmodule Core.Syntax.SignaturesTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.Syntax.Signatures
  alias Core.TokenFilters

  # Minimal deterministic dictionary fixture. No DB reads or mutations.
  @cells [
    %{id: "got|verb|1", norm: "got", pos: "verb", semantic_atoms: ["tag:no-past-participle"]},
    %{id: "got|verb|4", norm: "got", pos: "verb", semantic_atoms: ["tag:form-of", "tag:past"]},
    %{
      id: "got|verb|5",
      norm: "got",
      pos: "verb",
      semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]
    },
    %{id: "leave|verb|1", norm: "leave", pos: "verb", semantic_atoms: []},
    %{id: "study|verb|1", norm: "study", pos: "verb", semantic_atoms: []},
    %{id: "the|determiner|1", norm: "the", pos: "determiner", semantic_atoms: []},
    %{id: "station|noun|1", norm: "station", pos: "noun", semantic_atoms: []},
    %{
      id: "running|verb|1",
      norm: "running",
      pos: "verb",
      semantic_atoms: ["tag:form-of", "tag:participle", "tag:present"]
    }
  ]

  defp input(sentence, cells \\ @cells) do
    %SemanticInput{sentence: sentence, active_cells: cells}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  defp ids(sentence, cells \\ @cells) do
    sentence
    |> input(cells)
    |> Signatures.detect()
    |> Enum.map(& &1.id)
  end

  test "simple-past candidate is found without selecting a BrainCell" do
    assert :finite_past_candidate in ids("I got sick.")
    assert :finite_past_candidate in ids("I went to the doctor and got sick.")
    assert :finite_past_candidate in ids("They got to the station.")
  end

  test "have plus past participle supports a perfect construction" do
    assert :perfect_predicate in ids("I have got very cold.")
    assert :perfect_predicate in ids("They had got very cold.")
  end

  test "have + verb + to + verb is not confused with to + determiner" do
    assert :have_verb_to_infinitive in ids("I've got to study.")
    assert :have_verb_to_infinitive in ids("I've got to leave.")
    refute :have_verb_to_infinitive in ids("I have got to the station.")
    assert :perfect_predicate in ids("I have got to the station.")
  end

  test "ambiguous 's permits a have analysis without fixing contraction meaning" do
    signatures = Signatures.detect(input("He's got to leave."))
    assert Enum.any?(signatures, &(&1.id == :have_verb_to_infinitive))

    assert Enum.any?(signatures, fn s ->
             s.id == :perfect_predicate and :have_or_be_contraction in s.evidence
           end)

    refute :perfect_predicate in ids("He's running.")
  end

  test "unavailable dictionary evidence does not fabricate a construction" do
    assert [] == Signatures.detect(input("I have got to leave.", []))
  end

  test "signatures use original token indices" do
    signatures = Signatures.detect(input("I've got to leave."))
    sig = Enum.find(signatures, &(&1.id == :have_verb_to_infinitive))
    assert sig.span == {0, 3}
    assert sig.roles == %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3}
  end

  test "attach preserves sense candidates and records standard trace" do
    si = input("I have got very cold.")
    si = %{si | sense_candidates: %{2 => [%{id: "got|verb|4", score: 0.7}]}}
    out = Signatures.attach(si)

    assert %SemanticInput{} = out
    assert out.sense_candidates == si.sense_candidates
    assert out.active_cells == si.active_cells
    assert :perfect_predicate in Enum.map(out.sentence_signatures, & &1.id)
    assert [%{stage: :sentence_signatures, decision: :observed, meta: meta} | _] = out.trace
    assert meta.count >= 1
    assert :ok = SemanticInput.validate_contract(out)
  end
end
