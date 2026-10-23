defmodule Core.Social.DeclarativeEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.Social.DeclarativeEvidence

  @finite_evidence [:contiguous_surface, :attested_nominal_subject,
    :attested_nominal_object, :attested_finite_present_verb]

  defp input do
    names = [
      "Earlier,", "the", "editor", "observed", "that",
      "the", "team", "verifies", "those", "claims", "before", "noon."
    ]

    tokens =
      names
      |> Enum.with_index()
      |> Enum.map(fn {phrase, index} ->
        %{index: index, phrase: phrase, n: 1, kind: :word}
      end)

    specs = [
      {5, "the", "determiner"}, {6, "team", "noun"},
      {7, "verifies", "verb"}, {8, "those", "determiner"},
      {9, "claims", "noun"}
    ]

    cells =
      Enum.map(specs, fn {index, word, pos} ->
        %{id: "#{word}|#{pos}|1", norm: word,
          token_index: index, pos: pos}
      end)

    roles = %{
      subject_determiner: 5, subject_head: 6, predicate: 7,
      object_determiner: 8, object_head: 9
    }

    frame = %{
      id: :finite_present_svo_candidate,
      source: :core_syntax, span: {5, 9}, roles: roles,
      evidence: @finite_evidence
    }

    %{
      source: :colony_external,
      sentence: "Earlier, the editor observed that the team verifies those claims before noon.",
      tokens: tokens,
      active_cells: cells,
      sentence_signatures: [frame],
      atl_slate: %{winners: Enum.map(cells, fn cell ->
        %{token_index: cell.token_index, id: cell.id}
      end)}
    }
  end

  test "extracts a local SVO with independently selected roles without interpreting full sentence" do
    report = DeclarativeEvidence.analyze(input())

    assert report.mode == :read_only
    assert report.scope == :local_clause_not_whole_sentence
    assert report.status == :candidate_local_clauses
    assert report.examined_frames == 1
    assert report.abstentions == []
    assert [candidate] = report.candidates
    assert candidate.source_frame == :finite_present_svo_candidate
    assert candidate.span == {5, 9}
    assert candidate.surface == "the team verifies those claims"
    assert candidate.subject.surface == "team"
    assert candidate.subject.selected_sense_id == "team|noun|1"
    assert candidate.predicate.surface == "verifies"
    assert candidate.predicate.selected_sense_id == "verifies|verb|1"
    assert candidate.object.surface == "claims"
    assert candidate.object.selected_sense_id == "claims|noun|1"
    assert candidate.tense_hypothesis == :finite_present
    assert candidate.clause_attachment == :not_established
    assert candidate.polarity == :not_composed
    assert candidate.assertion_status == :not_asserted
    assert candidate.statement_truth == :not_evaluated
    assert candidate.verification == :external_unverified
    refute Map.has_key?(candidate, :proposition)
  end

  test "rejects unsupported syntax and does not guess from plausible token order" do
    si = input()
    assert DeclarativeEvidence.analyze(%{si | sentence_signatures: []}).candidates == []

    forged =
      update_in(si.sentence_signatures, fn frames ->
        Enum.map(frames, &Map.put(&1, :source, :external_hint))
      end)

    report = DeclarativeEvidence.analyze(%{si | sentence_signatures: forged})
    assert report.candidates == []
    assert [%{reason: :unrecognized_or_unattested_signature}] = report.abstentions

    weak = update_in(si.sentence_signatures, fn frames ->
      Enum.map(frames, &Map.put(&1, :evidence, [:contiguous_surface]))
    end)
    report = DeclarativeEvidence.analyze(%{si | sentence_signatures: weak})
    assert report.candidates == []
    assert [%{reason: :missing_grammar_evidence}] = report.abstentions
  end

  test "rejects missing dictionary witness and mismatched lexical selection" do
    si = input()
    no_dictionary = %{si | active_cells:
      Enum.reject(si.active_cells, &(&1.token_index == 7))}
    assert [%{reason: :selected_sense_not_attested_at_token}] =
      DeclarativeEvidence.analyze(no_dictionary).abstentions

    for id <- ["verifies|noun|1", "verifies|verb|fallback", "verifies|verb|2"] do
      winners = Enum.map(si.atl_slate.winners, fn
        %{token_index: 7} = item -> %{item | id: id}
        item -> item
      end)
      test_si = put_in(si, [:atl_slate, :winners], winners)
      report = DeclarativeEvidence.analyze(test_si)
      assert report.candidates == []
      assert length(report.abstentions) == 1
    end

    doubled = put_in(si, [:atl_slate, :winners],
      si.atl_slate.winners ++ [%{token_index: 7, id: "verifies|verb|2"}])
    assert [%{reason: :missing_or_ambiguous_lexical_selection}] =
      DeclarativeEvidence.analyze(doubled).abstentions
  end

  test "requires the actual source phrase and consecutive lexical tokens" do
    si = input()

    fabricated = %{si | sentence:
      "Earlier, the editor said that the team contradicts those claims before noon."}
    assert [%{reason: :local_surface_not_in_original_source}] =
      DeclarativeEvidence.analyze(fabricated).abstentions

    punctuation = %{si | sentence:
      "Earlier, the editor observed that the team verifies. Those claims before noon."}
    assert [%{reason: :local_surface_not_in_original_source}] =
      DeclarativeEvidence.analyze(punctuation).abstentions

    gapped = %{si | tokens: Enum.reject(si.tokens, &(&1.index == 8))}
    assert [%{reason: :noncontiguous_or_missing_lexical_tokens}] =
      DeclarativeEvidence.analyze(gapped).abstentions
  end

  test "supports a separately witnessed bounded regular-past local clause" do
    si = input()

    tokens = Enum.map(si.tokens, fn
      %{index: 7} = t -> %{t | phrase: "verified"}
      t -> t
    end)
    cells = Enum.map(si.active_cells, fn
      %{token_index: 7} = row ->
        %{row | id: "verified|verb|1", norm: "verified"}
      row -> row
    end)
    winners = Enum.map(si.atl_slate.winners, fn
      %{token_index: 7} = choice -> %{choice | id: "verified|verb|1"}
      choice -> choice
    end)
    past = %{
      id: :bounded_regular_past_transitive, source: :core_syntax,
      span: {5, 9}, roles: hd(si.sentence_signatures).roles,
      evidence: [:contiguous_surface, :attested_nominal_subject,
        :attested_dictionary_verb, :bounded_past_surface, :attested_nominal_object]
    }
    changed = %{si |
      sentence: "Earlier, the editor observed that the team verified those claims before noon.",
      tokens: tokens, active_cells: cells,
      atl_slate: %{winners: winners}, sentence_signatures: [past]}

    report = DeclarativeEvidence.analyze(changed)
    assert [%{tense_hypothesis: :past, predicate: %{surface: "verified"},
      assertion_status: :not_asserted}] = report.candidates
  end

  test "declines personal or untrusted source types without data or writes" do
    si = %{input() | source: :user}
    assert DeclarativeEvidence.analyze(si).status == :unsupported_source
    assert DeclarativeEvidence.analyze(si).candidates == []
    assert DeclarativeEvidence.analyze(%{}).status == :unsupported_source
  end
end
