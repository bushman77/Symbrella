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
end
