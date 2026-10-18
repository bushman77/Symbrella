defmodule Core.Semantic.WhObjectQuestionTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.SentenceMeaning
  alias Core.Syntax.Signatures
  alias Brain.LIFG.RoleCompetition

  @sentence "What did the artist draw?"

  defp input do
    tokens =
      ~w(What did the artist draw?)
      |> Enum.with_index()
      |> Enum.map(fn {word, i} -> %{index: i, phrase: word, n: 1} end)

    cells = [
      %{id: "what|pronoun|1", norm: "what", pos: "pronoun"},
      %{id: "did|auxiliary|0", norm: "did", pos: "auxiliary"},
      %{id: "the|determiner|1", norm: "the", pos: "determiner"},
      %{id: "artist|adjective|1", norm: "artist", pos: "adjective"},
      %{id: "artist|noun|1", norm: "artist", pos: "noun"},
      %{id: "draw|noun|17", norm: "draw", pos: "noun"},
      %{id: "draw|verb|76", norm: "draw", pos: "verb"}
    ]

    choices = [
      %{token_index: 0, id: "what|pronoun|1", lemma: "what", score: 0.12},
      %{token_index: 1, id: "did|auxiliary|0", lemma: "did", score: 0.23},
      %{token_index: 2, id: "the|determiner|1", lemma: "the", score: 0.06},
      %{token_index: 3, id: "artist|noun|1", lemma: "artist", score: 0.15},
      %{token_index: 4, id: "draw|verb|76", lemma: "draw", score: 0.007}
    ]

    base = %{sentence: @sentence, source: :test, tokens: tokens,
      active_cells: cells, lifg_choices: choices, trace: []}

    signatures = Signatures.detect(base)
    grammar = RoleCompetition.resolve(Map.put(base, :sentence_signatures, signatures))
    atl = Brain.ATL.reduce(choices, tokens, signatures)

    base
    |> Map.put(:sentence_signatures, signatures)
    |> Map.put(:role_competition, grammar)
    |> Map.put(:atl_slate, atl)
  end

  test "complete independently witnessed question is interpreted with an unbound object" do
    si = input()

    assert %{status: :resolved, roles: %{subject_head: %{sense_id: "artist|noun|1"},
                                        predicate: %{sense_id: "draw|verb|76"}}} =
             Enum.find(si.atl_slate.constituents, &(&1.id == :wh_aux_object_question))

    meaning = SentenceMeaning.build(si)

    assert meaning.status == :structurally_interpreted
    assert meaning.kind == :wh_object_question
    assert meaning.speech_act == :request_information
    assert meaning.sentence == @sentence
    assert meaning.roles.subject.surface == "the artist"
    assert meaning.roles.subject.head_sense_id == "artist|noun|1"
    assert meaning.roles.predicate.sense_id == "draw|verb|76"
    assert meaning.roles.object == %{role: :patient, status: :unbound}
    assert meaning.unresolved == [:patient]
    assert meaning.grounding.exact_lexical_meanings == :not_verified

    assert %{type: :information_request,
      queried: %{type: :participant_slot, role: :patient, status: :unbound},
      content: %{subject: %{surface: "the artist"},
                 predicate: %{surface: "draw", sense_id: "draw|verb|76"}},
      epistemic_status: :question_not_assertion,
      provenance: %{frame: :wh_aux_object_question}} = meaning.proposition

    refute Map.has_key?(meaning.proposition.content, :patient)
  end

  test "an ATL noun sense for predicate cannot become a structural question" do
    si = input()

    choices =
      Enum.map(si.lifg_choices, fn
        %{token_index: 4} = choice -> %{choice | id: "draw|noun|17"}
        choice -> choice
      end)

    atl = Brain.ATL.reduce(choices, si.tokens, si.sentence_signatures)
    wrong = %{si | lifg_choices: choices, atl_slate: atl}

    assert %{status: :partially_resolved, unresolved_roles: missing} =
             Enum.find(atl.constituents, &(&1.id == :wh_aux_object_question))
    assert :predicate in missing
    assert SentenceMeaning.build(wrong).status == :not_yet_interpreted
  end

  test "stale ATL role IDs cannot override actual lexical winners" do
    si = input()

    slate =
      update_in(si.atl_slate.constituents, fn entries ->
        Enum.map(entries, fn
          %{id: :wh_aux_object_question, roles: roles} = question ->
            altered = put_in(roles, [:predicate, :sense_id], "draw|verb|999")
            %{question | roles: altered}

          other -> other
        end)
      end)

    assert SentenceMeaning.build(%{si | atl_slate: slate}).status == :not_yet_interpreted
  end

  test "missing grammar adjudication cannot manufacture resolved question meaning" do
    si = input()
    missing = %{si | role_competition: %{}}
    assert SentenceMeaning.build(missing).status == :not_yet_interpreted
  end

  test "unsupported or injected signature cannot create meaning" do
    si = input()

    no_frame = %{si | sentence_signatures: []}
    assert SentenceMeaning.build(no_frame).status == :not_yet_interpreted

    forged = %{si | sentence_signatures: [
      %{id: :wh_aux_object_question, source: :unknown, span: {0, 4},
        roles: %{wh: 0, auxiliary: 1, subject_determiner: 2,
                 subject_head: 3, predicate: 4}, evidence: []}
    ]}
    assert SentenceMeaning.build(forged).status == :not_yet_interpreted
  end

  test "extra words or lack of question punctuation fail closed" do
    si = input()

    extra = %{si | sentence: "What did the artist draw today?",
      tokens: si.tokens ++ [%{index: 5, phrase: "today?", n: 1}]}
    assert SentenceMeaning.build(extra).status == :not_yet_interpreted

    not_question = %{si | sentence: "What did the artist draw"}
    assert SentenceMeaning.build(not_question).status == :not_yet_interpreted
  end
end
