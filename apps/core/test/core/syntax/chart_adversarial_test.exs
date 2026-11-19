defmodule Core.Syntax.ChartAdversarialTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.StructuralCompetition
  alias Core.SemanticInput
  alias Core.Syntax.{Chart, Grammar, Signatures}
  alias Core.TokenFilters

  defp cell(word, pos, tags \\ []) do
    %{id: "#{word}|#{pos}|1", norm: word, pos: pos, semantic_atoms: tags}
  end

  defp fixture_cells do
    [
      cell("the", "determiner"), cell("a", "determiner"),
      cell("an", "determiner"), cell("this", "determiner"),
      cell("these", "determiner"), cell("those", "determiner"),
      cell("each", "determiner"), cell("every", "determiner"),
      cell("several", "determiner"), cell("most", "determiner"),
      cell("site", "noun"), cell("page", "noun"),
      cell("pages", "noun", ["tag:form-of", "tag:plural"]),
      cell("wallet", "noun"),
      cell("i", "pronoun"),
      cell("nothing", "noun"), cell("nobody", "noun"),
      cell("none", "noun"),
      cell("ready", "adjective"), cell("open", "adjective"),
      cell("is", "auxiliary"), cell("are", "auxiliary"),
      cell("was", "auxiliary"),
      cell("hardly", "adverb"), cell("barely", "adverb"),
      cell("rarely", "adverb"), cell("without", "preposition")
    ]
  end

  defp input(sentence, cells \\ fixture_cells()) do
    %SemanticInput{sentence: sentence, source: :colony_external, active_cells: cells}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  defp report(si), do: Chart.diagnose(si, Signatures.chart_nodes(si))

  defp chart_signatures(si) do
    si
    |> Signatures.detect()
    |> Enum.filter(&(&1.id == :copular_predication))
  end

  test "closed lexical negation cues all abstain even with positive-looking clause frames" do
    # Synthetic "nothing", "nobody" and "none" noun witnesses are deliberate:
    # this tests the reported promotion path, not live LTM coverage.
    for sentence <- [
      "Nothing is ready.",
      "Nobody is ready.",
      "None is ready.",
      "The site is hardly open.",
      "The site is barely open.",
      "The site is rarely open.",
      "The site is open without pages."
    ] do
      si = input(sentence)
      assert %{status: :abstained, reason: :negation, signatures: []} = report(si),
        "unexpected negation diagnosis: #{sentence}"
      assert chart_signatures(si) == []
    end

    for cue <- ~w(none nothing nobody nowhere neither nor hardly barely scarcely rarely seldom without) do
      assert Grammar.negative?(cue), "missing closed negative cue #{cue}"
    end
  end

  test "first-person pronoun and incompatible subject specifiers fail closed" do
    for sentence <- [
      "I is ready.",
      "A pages are ready.",
      "These site is ready.",
      "Each pages are ready.",
      "Several site is ready."
    ] do
      si = input(sentence)
      assert %{status: :abstained, reason: :agreement_conflict, signatures: []} =
        report(si), "unexpected agreement diagnosis: #{sentence}"

      assert chart_signatures(si) == [],
        "unsupported full-span grammatical promotion: #{sentence}"
    end
  end

  test "positive controls retain compatible determiner quantifier and first-person agreement" do
    for sentence <- [
      "The site is open.",
      "The pages are ready.",
      "Each page is ready.",
      "These pages are ready.",
      "Most pages are ready.",
      "Every wallet is ready.",
      "I was ready."
    ] do
      si = input(sentence)
      assert %{status: :candidate, reason: nil} = report(si),
        "over-rejected positive: #{sentence}"

      assert [%{id: :copular_predication, factual_authority?: false} = sig] =
        chart_signatures(si), "missing or ambiguous positive: #{sentence}"

      assert :no_negation_cue_in_closed_list in sig.evidence
      refute :positive_polarity in sig.evidence
    end

    assert Grammar.pronoun_number("i") == :first_person_singular
  end

  test "quantifiers need attested determiner POS and an explicit Brain role requirement" do
    si = input("Each page is ready.")
    assert [%{roles: %{subject_quantifier: 0},
              required_features: %{subject_quantifier: [:pos_determiner]}}] =
      chart_signatures(si)

    ungrounded =
      fixture_cells()
      |> Enum.reject(&(&1.norm == "each" and &1.pos == "determiner"))

    si = input("Each page is ready.", ungrounded)
    assert %{status: :abstained, reason: :lexical_witness_missing, signatures: []} =
      report(si)
    assert chart_signatures(si) == []
  end

  test "polysemous function-word surfaces cannot bypass agreement or POS witnesses" do
    with_alternatives = fixture_cells() ++ [
      cell("a", "noun"), cell("each", "noun"),
      cell("i", "noun"), cell("these", "noun")
    ]

    for {sentence, reason} <- [
      {"A pages are ready.", :agreement_conflict},
      {"Each pages are ready.", :agreement_conflict},
      {"These site is ready.", :agreement_conflict},
      {"I is ready.", :agreement_conflict}
    ] do
      si = input(sentence, with_alternatives)
      assert %{status: :abstained, reason: ^reason, signatures: []} = report(si)
      assert chart_signatures(si) == []
    end

    missing_quantifier_witness =
      with_alternatives
      |> Enum.reject(&(&1.norm == "each" and &1.pos == "determiner"))

    si = input("Each page is ready.", missing_quantifier_witness)
    assert %{status: :abstained, reason: :lexical_witness_missing,
             signatures: []} = report(si)
    assert chart_signatures(si) == []
  end

  test "Colony chart signatures precede 64 legacy constituents for the real Brain cap" do
    si = input("The site is open.")
    emitted = Signatures.detect(si)
    assert [%{id: :copular_predication} | _] = emitted

    filler = %{
      id: :noun_phrase, span: {0, 1},
      roles: %{determiner: 0, head: 1},
      required_features: %{determiner: [:pos_determiner], head: [:pos_noun]},
      evidence: [:nominal_constituent, :pos_pattern],
      source: :core_syntax
    }

    fillers = List.duplicate(filler, 64)
    choices = [
      %{token_index: 0, id: "the|determiner|1"},
      %{token_index: 1, id: "site|noun|1"},
      %{token_index: 2, id: "is|auxiliary|1"},
      %{token_index: 3, id: "open|adjective|1"}
    ]

    # Baseline Brain cap genuinely hides an appended signature.
    appended =
      si
      |> Map.put(:sentence_signatures, fillers ++ emitted)
      |> Map.put(:lifg_choices, choices)
      |> StructuralCompetition.resolve()

    assert appended.status == :no_complete_candidate
    assert appended.candidates == []

    # With the exact chart-first order from detect/1, no Brain code changes
    # are necessary: the full-span candidate survives the first-64 cap.
    prepended =
      si
      |> Map.put(:sentence_signatures, emitted ++ fillers)
      |> Map.put(:lifg_choices, choices)
      |> StructuralCompetition.resolve()

    assert prepended.status == :preferred
    assert Enum.any?(prepended.candidates,
      &(&1.construction == :copular_predication))
    assert prepended.preferred.construction == :copular_predication
  end

  test "a seven-item polysemous width-one span has a distinct fail-closed label" do
    # Synthetic token "site": noun, adjective, preposition, adverb.
    # Five terminals plus two unary derivations exceeds 6 without changing it.
    cells = fixture_cells() ++ [
      cell("site", "adjective"), cell("site", "preposition"),
      cell("site", "adverb")
    ]

    si = input("The site is ready.", cells)
    assert %{status: :abstained, reason: :lexical_ambiguity_budget,
             signatures: []} = report(si)
    assert chart_signatures(si) == []
    assert Chart.limits() == %{tokens: 48, per_span: 6, total: 400, results: 12}
  end

  test "polarity evidence only certifies checking the closed cue list" do
    si = input("The site is open.")
    assert [%{evidence: evidence}] = chart_signatures(si)

    assert :no_negation_cue_in_closed_list in evidence
    refute :positive_polarity in evidence
    assert :positive_polarity not in Enum.flat_map(
      chart_signatures(input("The pages are ready.")), & &1.evidence)
  end
end
