defmodule Core.Semantic.SentenceMeaningQuestionDiagnosticTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.SentenceMeaning

  test "does not invent a question if no signature exists" do
    assert %{status: :no_question_signature} =
      SentenceMeaning.question_diagnostic(%{sentence_signatures: [], atl_slate: %{}})
  end

  test "reports unresolved ATL constituent independently of grammar" do
    frame = %{
      id: :wh_aux_topic_question, span: {0, 5},
      roles: %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4}
    }

    assert %{status: :atl_constituent_unresolved} =
      SentenceMeaning.question_diagnostic(%{
        sentence_signatures: [frame],
        atl_slate: %{constituents: []},
        tokens: []
      })
  end

  test "comma between indexed words is not mistaken for missing vocabulary" do
    roles = %{wh: 0, auxiliary: 1, subject: 2, predicate: 3, preposition: 4}
    frame = %{id: :wh_aux_topic_question, span: {0, 7}, roles: roles}

    atl_roles =
      Map.new(roles, fn {role, index} ->
        {role, %{token_index: index, sense_id: "#{role}|noun|1"}}
      end)

    tokens = [
      {0, "what"}, {1, "do"}, {2, "you"}, {3, "know"},
      {4, "about"}, {5, "growth"}, {7, "and"}
    ] |> Enum.map(fn {index, word} -> %{index: index, n: 1, phrase: word} end)

    si = %{
      sentence: "what do you know about growth, and",
      tokens: tokens,
      sentence_signatures: [frame],
      atl_slate: %{constituents: [%{
        id: :wh_aux_topic_question,
        span: {0, 7},
        status: :resolved,
        roles: atl_roles
      }]}
    }

    assert %{status: :punctuation_gap_witnessed, indices: [6]} =
      SentenceMeaning.question_diagnostic(si)

    assert %{status: :punctuation_gap_witnessed, indices: [6]} =
      SentenceMeaning.question_diagnostic(%{si | sentence: "what do you know about growth—and"})

    assert %{status: :punctuation_gap_witnessed, indices: [6]} =
      SentenceMeaning.question_diagnostic(%{si | sentence: "what do you know about growth: and"})

    assert %{status: :missing_token_surfaces, indices: [6]} =
      SentenceMeaning.question_diagnostic(%{si | sentence: "what do you know about growth and"})
  end
end
