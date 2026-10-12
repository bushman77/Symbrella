defmodule Brain.LIFG.GrammarEvidenceTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.GrammarEvidence, as: GE

  @cells [
    %{id: "got|verb|1", semantic_atoms: ["tag:no-past-participle"],
      definition: "Expressing obligation; used with have."},
    %{id: "got|verb|4", semantic_atoms: ["tag:form-of", "tag:past"]},
    %{id: "got|verb|5", semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]}
  ]
  @scores %{"got|verb|1" => 0.5, "got|verb|4" => 0.5, "got|verb|5" => 0.5}

  defp context(signatures), do: GE.prepare(%{active_cells: @cells, sentence_signatures: signatures})
  defp sign(id, roles), do: %{id: id, roles: roles}

  test "weight zero preserves exact scores" do
    ctx = context([sign(:finite_past_candidate, %{verb: 1})])
    assert GE.apply(@scores, ctx, 1, 0.0) == @scores
  end

  test "finite past favors the simple-past sense, not obligation" do
    ctx = context([sign(:finite_past_candidate, %{verb: 1})])
    result = GE.apply(@scores, ctx, 1, 1.0)
    assert result["got|verb|4"] > result["got|verb|1"]
    assert result["got|verb|5"] == 0.5
  end

  test "perfect favors the past participle" do
    ctx = context([sign(:perfect_predicate, %{auxiliary: 0, verb: 1})])
    result = GE.apply(@scores, ctx, 1, 1.0)
    assert result["got|verb|5"] > result["got|verb|1"]
    assert result["got|verb|4"] == 0.5
  end

  test "larger obligation-compatible frame outweighs the perfect hypothesis" do
    ctx = context([
      sign(:perfect_predicate, %{auxiliary: 0, verb: 1}),
      sign(:have_verb_to_infinitive, %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3})
    ])
    result = GE.apply(@scores, ctx, 1, 1.0)
    assert result["got|verb|1"] > result["got|verb|5"]
  end

  test "infinitive complement supports a verb, without deleting the noun" do
    ctx = context([sign(:have_verb_to_infinitive, %{verb: 1, infinitive_marker: 2, complement: 3})])
    scores = %{"leave|noun|4" => 0.5, "leave|verb|1" => 0.5}
    result = GE.apply(scores, ctx, 3, 1.0)
    assert result["leave|verb|1"] > result["leave|noun|4"]
    assert Map.keys(result) |> Enum.sort() == Map.keys(scores) |> Enum.sort()
  end

  test "unrelated token index is unchanged" do
    ctx = context([sign(:finite_past_candidate, %{verb: 1})])
    assert GE.apply(@scores, ctx, 99, 1.0) == @scores
  end

  test "broad have-verb-to pattern alone is not treated as obligation" do
    ctx = GE.prepare(%{
      active_cells: [%{id: "tried|verb|1", semantic_atoms: ["tag:past", "tag:participle"], definition: "Past form of try."}],
      sentence_signatures: [sign(:have_verb_to_infinitive, %{verb: 1, complement: 3})]
    })
    assert GE.apply(%{"tried|verb|1" => 0.5}, ctx, 1, 1.0) == %{"tried|verb|1" => 0.5}
  end
end
