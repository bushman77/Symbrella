defmodule Core.Syntax.PredicateEvidenceTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.Syntax.{PredicateEvidence, RoleCompetition}
  alias Brain.LIFG.{GrammarEvidence, SenseFocus, Stage1}
  alias Brain.LIFG.Stage1.PosBias

  defp input(words, cells, signatures \\ []) do
    {tokens, _end} =
      words
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {word, index}, start ->
        stop = start + byte_size(word)
        {%{index: index, phrase: word, kind: :word, n: 1,
           mw: false, span: {start, stop}}, stop + 1}
      end)

    %SemanticInput{
      sentence: Enum.join(words, " "),
      tokens: tokens,
      active_cells: cells,
      sentence_signatures: signatures
    }
  end

  defp cell(word, pos, atoms \\ []) do
    %{id: "#{word}|#{pos}|1", norm: word, pos: pos,
      semantic_atoms: atoms, activation: 0.5}
  end

  test "modal question yields one shared predicate with an attested verb alternative" do
    cells = [
      cell("you", "pronoun"),
      cell("write", "noun"), cell("write", "verb")
    ]
    si = input(~w(Can you write me a basic GenServer in MyExample), cells)
    out = RoleCompetition.attach(si)

    assert [%{token_index: 2, basis: :modal_question_predicate}] =
             out.role_competition.predicate_proposals
    assert %{role: :predicate, pos: "verb", status: :supported,
             basis: :modal_question_predicate} = out.role_competition.roles[2]
    assert out.trace |> hd() |> Map.get(:stage) == :role_competition
    assert hd(out.trace).meta.predicate_proposals == 1
    assert :ok = SemanticInput.validate_contract(out)

    scores = %{"write|noun|1" => 0.50, "write|verb|1" => 0.48,
               "write|verb|2" => 0.48}
    context = GrammarEvidence.prepare(out)
    ranked = GrammarEvidence.apply_role_competition(scores, context, 2)
    assert ranked["write|verb|1"] > ranked["write|noun|1"]
    assert ranked["write|verb|2"] > ranked["write|noun|1"]
    assert ranked["write|verb|1"] == ranked["write|verb|2"]

    candidate_rows = [
      cell("write", "noun"),
      cell("write", "verb"),
      %{cell("write", "verb") | id: "write|verb|2"}
    ]
    focus = SenseFocus.plan(out, 2, candidate_rows)
    assert focus.mode == :focused
    assert focus.focus_pos == "verb"
    assert focus.context_evaluations == 2
    assert focus.context_skipped == 1

    # Older local POS rules are a fallback, not additive on the same evidence.
    idx2 = Enum.at(out.tokens, 2)
    assert PosBias.window(%{si: out}, idx2, "verb") == 0.0
    assert PosBias.window(%{si: si}, idx2, "verb") == 0.34
  end

  test "Stage1 consumes the same role and retains the complete scored sense slate" do
    nouns_and_verbs = [
      cell("you", "pronoun"), cell("write", "noun"),
      cell("write", "verb"), %{cell("write", "verb") | id: "write|verb|2"}
    ]

    base = input(~w(Can you write me a module), nouns_and_verbs)
    si = %{RoleCompetition.attach(base) |
      sense_candidates: %{2 => Enum.filter(nouns_and_verbs, &(&1.norm == "write"))}}

    assert {:ok, %{choices: choices}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               db_backfill?: false, semantic_control?: false,
               weights: [lex_fit: 0.0, context_fit: 0.0, rel_prior: 0.0,
                         activation: 1.0, intent_bias: 0.0])

    choice = Enum.find(choices, &(&1.token_index == 2))
    assert choice
    assert choice.sense_focus.mode == :focused
    assert String.contains?(choice.chosen_id, "|verb|")
    assert choice.scores["write|verb|1"] > choice.scores["write|noun|1"]
    assert Map.keys(choice.scores) |> Enum.sort() ==
             Enum.sort(["write|noun|1", "write|verb|1", "write|verb|2"])
  end

  test "supports modal statements, determined subjects, modifiers and imperatives" do
    cases = [
      {~w(Could you please not write a file), 4, :modal_question_predicate},
      {~w(Will the worker file a report), 3, :modal_question_predicate},
      {~w(The worker can write a module), 3, :modal_declarative_predicate},
      {~w(We will write a report), 2, :modal_declarative_predicate},
      {~w(Please write a note), 1, :imperative_predicate}
    ]

    for {words, index, basis} <- cases do
      witnesses = [
        cell("you", "pronoun"), cell("we", "pronoun"),
        cell("the", "determiner"), cell("worker", "noun"),
        cell("write", "verb"), cell("file", "verb")
      ]
      out = input(words, witnesses) |> RoleCompetition.attach()
      assert %{basis: ^basis, role: :predicate, pos: "verb",
               status: :supported} = out.role_competition.roles[index]
    end
  end

  test "counted did-not transitive supplies a dictionary-witnessed verb role before LIFG" do
    words = ~w(Three links did not support the claims)

    cells = [
      cell("links", "noun"),
      cell("did", "verb"),
      cell("not", "adverb"),
      cell("support", "noun"),
      cell("support", "verb"),
      cell("the", "determiner"),
      cell("claims", "noun")
    ]

    si = input(words, cells)
    out = RoleCompetition.attach(si)

    assert [%{token_index: 4, basis: :counted_negative_transitive_predicate,
              pos: "verb", evidence: evidence}] = out.role_competition.predicate_proposals
    assert :explicit_did_not_negation in evidence

    assert %{role: :predicate, pos: "verb", status: :supported,
             basis: :counted_negative_transitive_predicate} =
             out.role_competition.roles[4]

    scores = %{"support|noun|1" => 0.51, "support|verb|1" => 0.38}
    updated = GrammarEvidence.apply_role_competition(
      scores, GrammarEvidence.prepare(out), 4
    )

    assert updated["support|verb|1"] > updated["support|noun|1"]
    assert updated["support|noun|1"] == 0.51
    assert Map.keys(updated) |> Enum.sort() == Map.keys(scores) |> Enum.sort()

    focus = SenseFocus.plan(out, 4, Enum.filter(cells, &(&1.norm == "support")))
    assert focus.mode == :focused
    assert focus.focus_pos == "verb"

    assert out.active_cells == si.active_cells
    assert out.sentence_signatures == si.sentence_signatures
    assert :ok = SemanticInput.validate_contract(out)
  end

  test "counted negative predicate abstains on missing verb, missing negator or nonnegative surface" do
    words = ~w(Three links did not support the claims)
    witnesses = [
      cell("links", "noun"), cell("did", "verb"),
      cell("not", "adverb"), cell("support", "noun"),
      cell("support", "verb"), cell("the", "determiner"),
      cell("claims", "noun")
    ]

    cases = [
      {words, Enum.reject(witnesses, &(&1.norm == "support" and &1.pos == "verb"))},
      {words, Enum.reject(witnesses, &(&1.norm == "not"))},
      {~w(Three links did perhaps support the claims), witnesses},
      {~w(Three links might not support the claims), witnesses},
      {~w(Three links did not support claims), witnesses}
    ]

    for {surface, rows} <- cases do
      out = input(surface, rows)
      assert PredicateEvidence.detect(out) == []
      refute Map.has_key?(RoleCompetition.attach(out).role_competition.roles, 4)
    end
  end

  test "COLONY-09 counted plural present feeds one attested POS role into Stage1" do
    cells = [
      cell("reports", "noun", ["tag:form-of", "tag:plural"]),
      cell("support", "noun"), cell("support", "verb"),
      cell("the", "determiner"), cell("claims", "noun")
    ]

    initial = input(~w(Four reports support the claims), cells)
    si = Core.Syntax.Signatures.attach(initial)
    out = RoleCompetition.attach(si)

    assert [%{token_index: 2, basis: :counted_present_transitive_predicate,
              role: :predicate, pos: "verb"}] =
             out.role_competition.predicate_proposals
    assert %{status: :supported, pos: "verb"} = out.role_competition.roles[2]

    scores = %{"support|noun|1" => 0.46, "support|verb|1" => 0.40}
    revised = GrammarEvidence.apply_role_competition(
      scores, GrammarEvidence.prepare(out), 2)

    assert revised["support|verb|1"] > revised["support|noun|1"]
    assert revised["support|noun|1"] == 0.46

    focus = SenseFocus.plan(out, 2, Enum.filter(cells, &(&1.norm == "support")))
    assert focus.mode == :focused
    assert focus.focus_pos == "verb"

    for invalid_cells <- [
          Enum.reject(cells, &(&1.norm == "reports")),
          Enum.reject(cells, &(&1.norm == "support" and &1.pos == "verb"))
        ] do
      invalid =
        input(~w(Four reports support the claims), invalid_cells)
        |> Core.Syntax.Signatures.attach()
        |> RoleCompetition.attach()

      assert invalid.role_competition.predicate_proposals == []
    end
  end

  test "plural present supplies POS evidence without asserting an event" do
    cells = [
      cell("the", "determiner"),
      cell("artists", "noun", ["tag:form-of", "tag:plural"]),
      cell("draw", "verb"), cell("draw", "noun"),
      cell("pictures", "noun")
    ]

    si = input(~w(The artists draw pictures), cells)
    out = RoleCompetition.attach(si)

    assert %{basis: :plural_present_predicate, status: :supported,
             pos: "verb"} = out.role_competition.roles[2]
    assert %{mode: :focused, focus_pos: "verb"} =
             SenseFocus.plan(out, 2, Enum.filter(cells, &(&1.norm == "draw")))
    assert si.sentence_signatures == []
    assert out.episodic_candidates == []

    no_plural = %{si | active_cells:
      Enum.reject(cells, &(&1.norm == "artists"))}
    assert PredicateEvidence.detect(no_plural) == []

    past = %{si | active_cells:
      Enum.map(cells, fn
        %{norm: "draw", pos: "verb"} = entry ->
          %{entry | semantic_atoms: ["tag:form-of", "tag:past"]}
        entry -> entry
      end)}
    assert PredicateEvidence.detect(past) == []
  end

  test "complete finite present signature also shares an adjudicated predicate" do
    evidence = [:contiguous_surface, :attested_nominal_subject,
                :attested_nominal_object, :attested_finite_present_verb]
    signature = %{id: :finite_present_svo_candidate, source: :core_syntax,
      span: {0, 4},
      roles: %{subject_head: 1, predicate: 2, object_head: 4},
      evidence: evidence}
    cells = [
      cell("the", "determiner"), cell("artist", "noun"),
      cell("draws", "verb", ["tag:form-of", "tag:present",
                           "tag:indicative", "tag:singular", "tag:third-person"]),
      cell("a", "determiner"), cell("picture", "noun")
    ]

    out =
      input(~w(The artist draws a picture), cells, [signature])
      |> RoleCompetition.attach()

    assert %{basis: :finite_present_svo_predicate, status: :supported} =
             out.role_competition.roles[2]
    scores = %{"draws|verb|1" => 0.5}
    ctx = GrammarEvidence.prepare(out)
    assert GrammarEvidence.apply_role_competition(scores, ctx, 2)["draws|verb|1"] == 0.8
    assert GrammarEvidence.apply_finite_present(scores, ctx, 2) == scores
  end

  test "missing verb witness, unsupported word order and bare nouns abstain" do
    cases = [
      {~w(Can you write a note), [cell("you", "pronoun"), cell("write", "noun")]},
      {~w(Can you a write), [cell("you", "pronoun"), cell("write", "verb")]},
      {~w(The book cover), [cell("book", "noun"), cell("cover", "noun")]},
      {~w(The artist will book), [cell("the", "determiner"),
        cell("artist", "noun"), cell("book", "noun")]},
      {~w(Write), [cell("write", "verb")]}
    ]
    for {words, cells} <- cases do
      assert PredicateEvidence.detect(input(words, cells)) == []
    end
  end

  test "Stage1 production roles repair define verb and artist common noun without choosing senses" do
    rows = [
      cell("define", "noun"), cell("define", "verb"),
      cell("artist", "noun"), cell("artist", "proper_noun"),
      %{cell("artist", "noun") | id: "artist|noun|2"}
    ]

    si =
      input(~w(define artist), rows)
      |> Map.put(:intent, :define)
      |> Core.Syntax.Signatures.attach()
      |> RoleCompetition.attach()
      |> Map.put(:sense_candidates, %{
        0 => Enum.filter(rows, &(&1.norm == "define")),
        1 => Enum.filter(rows, &(&1.norm == "artist"))
      })

    assert %{pos: "verb", status: :supported} = si.role_competition.roles[0]
    assert %{pos: "noun", status: :supported} = si.role_competition.roles[1]

    assert {:ok, %{choices: choices}} =
      Stage1.run(si,
        scores: :all, preserve_sense_candidates: true,
        db_backfill?: false, semantic_control?: false,
        weights: [lex_fit: 0.0, context_fit: 0.0, rel_prior: 0.0,
                  activation: 1.0, intent_bias: 0.0])

    verb = Enum.find(choices, &(&1.token_index == 0))
    noun = Enum.find(choices, &(&1.token_index == 1))
    assert verb.chosen_id == "define|verb|1"
    assert String.starts_with?(noun.chosen_id, "artist|noun|")
    assert verb.scores["define|verb|1"] > verb.scores["define|noun|1"]
    assert noun.scores["artist|noun|1"] > noun.scores["artist|proper_noun|1"]
    assert noun.scores["artist|noun|1"] == noun.scores["artist|noun|2"]
    assert Map.keys(noun.scores) |> Enum.sort() ==
      Enum.sort(["artist|noun|1", "artist|noun|2", "artist|proper_noun|1"])
  end

end
