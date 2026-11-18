defmodule Core.Integration.CoverageTest do
  use ExUnit.Case, async: true

  alias Core.Integration.Coverage
  alias Core.Pipeline.Input

  @passage "John went to the store. " <>
             "He bought some bread. " <>
             "When he returned home, he made a sandwich. " <>
             "What did John buy?"

  test "identifies a segmented passage without claiming comprehension" do
    si =
      Input.prepare(@passage)
      |> Map.put(:sentence_meaning, %{
        kind: :unknown,
        status: :not_yet_interpreted
      })

    assert %{
             status: :segmented_only,
             sentence_count: 4,
             question_count: 1,
             per_sentence_semantics: :not_run,
             whole_input_semantics: %{
               kind: :unknown,
               status: :not_yet_interpreted
             },
             discourse_resolution: :not_evaluated,
             question_answer_binding: :not_evaluated,
             factual_authority?: false
           } = Coverage.assess(si)
  end

  test "distinguishes missing semantic analysis from failed analysis" do
    si = Input.prepare("He bought some bread.")

    assert %{
             status: :not_analyzed,
             sentence_count: 1,
             whole_input_semantics: %{
               status: :not_observed
             }
           } = Coverage.assess(si)
  end

  test "rejects invalid canonical alignment" do
    si = Input.prepare(@passage)
    first = hd(si.tokens)

    altered = %{
      si
      | tokens:
          List.replace_at(
            si.tokens,
            0,
            %{first | span: {1, 5}}
          )
    }

    assert %{
             status: :unavailable,
             reason: :invalid_alignment
           } = Coverage.assess(altered)
  end

  test "does not mutate the semantic input" do
    si = Input.prepare(@passage)

    original = si
    _report = Coverage.assess(si)

    assert si == original
  end
end
