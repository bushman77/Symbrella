defmodule Core.Syntax.FinitePresentWhRolesTest do
  use ExUnit.Case, async: true

  alias Core.Syntax.Signatures
  alias Brain.LIFG.{RoleCompetition, GrammarEvidence, SenseFocus}

  @morphology ~w(tag:form-of tag:indicative tag:present tag:singular tag:third-person)

  defp cell(word, pos, sense \\ 1, atoms \\ []) do
    %{id: "#{word}|#{pos}|#{sense}", norm: word, word: word, pos: pos,
      semantic_atoms: atoms}
  end

  defp tokens(words), do: words |> Enum.with_index()
    |> Enum.map(fn {word, i} -> %{index: i, phrase: word, n: 1} end)

  defp artist_cells do
    [
      cell("the", "determiner"),
      cell("artist", "adjective"),
      cell("artist", "noun"),
      cell("draws", "verb", 1, @morphology),
      cell("draws", "noun"),
      cell("a", "determiner"),
      cell("picture", "noun")
    ]
  end

  test "finite present SVO supports the existing noun subject, never the adjective" do
    si = %{
      sentence: "The artist draws a picture.",
      tokens: tokens(~w(The artist draws a picture)),
      active_cells: artist_cells()
    }

    signatures = Signatures.detect(si)
    assert Enum.any?(signatures, &(&1.id == :finite_present_svo_candidate))

    out = RoleCompetition.resolve(Map.put(si, :sentence_signatures, signatures))
    assert %{role: :subject_head, pos: "noun",
             basis: :determined_subject_before_attested_present_predicate} =
             out.roles[1]

    context =
      si
      |> Map.put(:sentence_signatures, signatures)
      |> Map.put(:role_competition, out)
      |> GrammarEvidence.prepare()

    boosted = GrammarEvidence.apply_role_competition(
      %{"artist|noun|1" => 0.2, "artist|adjective|1" => 0.2}, context, 1)

    assert_in_delta boosted["artist|noun|1"], 0.5, 1.0e-8
    assert boosted["artist|adjective|1"] == 0.2

    focus =
      SenseFocus.plan(%{role_competition: out}, 1,
        [cell("artist", "noun"), cell("artist", "adjective")])

    assert focus.mode == :focused
    assert focus.focus_pos == "noun"
  end

  test "WH auxiliary object question supports noun subject and verb predicate" do
    si = %{
      sentence: "What did the artist draw?",
      tokens: tokens(~w(What did the artist draw)),
      active_cells: [
        cell("what", "pronoun"), cell("did", "auxiliary"),
        cell("the", "determiner"), cell("artist", "adjective"),
        cell("artist", "noun"), cell("draw", "noun"), cell("draw", "verb")
      ]
    }

    signatures = Signatures.detect(si)
    assert Enum.any?(signatures, fn sig ->
      sig.id == :wh_aux_object_question and sig.roles.subject_head == 3 and
        sig.roles.predicate == 4
    end)

    out = RoleCompetition.resolve(Map.put(si, :sentence_signatures, signatures))
    assert %{pos: "noun", basis: :determined_subject_in_wh_aux_question} = out.roles[3]
    assert %{pos: "verb", basis: :predicate_in_wh_aux_question} = out.roles[4]

    ctx = GrammarEvidence.prepare(%{si |
      sentence_signatures: signatures, role_competition: out})

    scores = GrammarEvidence.apply_role_competition(
      %{"draw|noun|1" => 0.2, "draw|verb|1" => 0.2}, ctx, 4)

    assert scores["draw|noun|1"] == 0.2
    assert_in_delta scores["draw|verb|1"], 0.5, 1.0e-8

    focus = SenseFocus.plan(%{role_competition: out}, 4,
      [cell("draw", "noun"), cell("draw", "verb")])

    assert %{mode: :focused, focus_pos: "verb"} = focus
  end

  test "question without an attested lexical verb cannot imply a verb role" do
    si = %{
      sentence: "What did the artist flim?",
      tokens: tokens(~w(What did the artist flim)),
      active_cells: [
        cell("the", "determiner"), cell("artist", "noun"),
        cell("flim", "noun")
      ]
    }

    signatures = Signatures.detect(si)
    refute Enum.any?(signatures, &(&1.id == :wh_aux_object_question))

    out = RoleCompetition.resolve(Map.put(si, :sentence_signatures, signatures))
    refute Map.has_key?(out.roles, 4)
  end

  test "a noun adjective overlap without a full sentence cannot claim present SVO support" do
    si = %{
      sentence: "The artist.",
      tokens: tokens(~w(The artist)),
      active_cells: artist_cells()
    }

    signatures = Signatures.detect(si)
    refute Enum.any?(signatures, &(&1.id == :finite_present_svo_candidate))

    out = RoleCompetition.resolve(Map.put(si, :sentence_signatures, signatures))
    refute Map.has_key?(out.roles, 1)
  end
end
