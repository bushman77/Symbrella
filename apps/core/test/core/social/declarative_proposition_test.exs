defmodule Core.Social.DeclarativePropositionTest do
  use ExUnit.Case, async: true

  alias Core.Social.DeclarativeProposition

  defp fixture(sentence, opts \\ []) do
    words =
      sentence
      |> String.trim_trailing(".")
      |> String.split(" ", trim: true)

    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, idx} ->
        %{index: idx, phrase: word, kind: :word, n: 1}
      end)

    words = Enum.map(words, &String.downcase/1)
    specs = Keyword.get(opts, :roles, [])

    cells =
      Enum.map(specs, fn {idx, pos} ->
        word = Enum.at(words, idx)
        %{
          id: "#{word}|#{pos}|1",
          norm: word,
          word: word,
          token_index: idx,
          pos: pos
        }
      end)

    winners =
      Enum.map(cells, fn cell ->
        %{id: cell.id, token_index: cell.token_index}
      end)

    signatures =
      Keyword.get(opts, :noun_phrases, [])
      |> Enum.map(fn {determiner, head} ->
        %{
          id: :noun_phrase,
          source: :core_syntax,
          roles: %{determiner: determiner, head: head}
        }
      end)

    past =
      if Keyword.get(opts, :finite_past?, false) do
        [%{id: :finite_past_candidate, source: :core_syntax,
           roles: %{verb: Keyword.get(opts, :verb_index, 2)}}]
      else
        []
      end

    %{
      source: :colony_external,
      sentence: sentence,
      tokens: tokens,
      active_cells: cells,
      sentence_signatures: signatures ++ past,
      atl_slate: %{winners: winners}
    }
  end

  defp produced do
    fixture("The checker produced 13 links.",
      roles: [{1, "noun"}, {2, "verb"}, {4, "noun"}],
      noun_phrases: [{0, 1}],
      finite_past?: true)
  end

  defp contradicted do
    fixture("Ten links contradicted the original claims.",
      roles: [{1, "noun"}, {2, "verb"}, {4, "adjective"}, {5, "noun"}],
      noun_phrases: [{3, 5}],
      finite_past?: true)
  end

  defp supported do
    fixture("Three links supported the claims.",
      roles: [{1, "noun"}, {2, "verb"}, {4, "noun"}],
      noun_phrases: [{3, 4}],
      finite_past?: true)
  end

  defp negated do
    fixture("The checker did not verify those claims.",
      roles: [{1, "noun"}, {2, "verb"}, {3, "adverb"}, {4, "verb"}, {6, "noun"}],
      noun_phrases: [{0, 1}, {5, 6}])
  end

  test "reported action binds an attested counted object, not a verified event" do
    proposition = DeclarativeProposition.interpret(produced())

    assert proposition.status == :attributed_structural_interpretation
    assert proposition.construction == :counted_object_action
    assert proposition.subject == "checker"
    assert proposition.predicate == "produced"
    assert proposition.object == "links"
    assert proposition.reported_count == 13
    assert proposition.reported_total == nil
    assert proposition.relation == :reported_action
    assert proposition.polarity == :positive
    assert proposition.verification == :author_claim_not_independently_verified
  end

  test "different counted subjects and predicates retain opposite evidence polarity" do
    opposite = [
      {contradicted(), :reported_refutation, 10, "claims"},
      {supported(), :reported_support, 3, "claims"}
    ]

    for {si, relation, count, object} <- opposite do
      result = DeclarativeProposition.interpret(si)
      assert result.status == :attributed_structural_interpretation
      assert result.construction == :counted_transitive
      assert result.relation == relation
      assert result.reported_count == count
      assert result.object == object
      assert result.polarity == :positive
    end
  end

  test "explicit negation remains an attributed negative action, not an affirmative fact" do
    result = DeclarativeProposition.interpret(negated())

    assert result.status == :attributed_structural_interpretation
    assert result.construction == :explicit_negated_transitive
    assert result.predicate == "verify"
    assert result.object == "claims"
    assert result.relation == :reported_action
    assert result.polarity == :negative
    assert result.reported_count == nil
  end

  test "missing syntactic evidence is not replaced with a plausible surface match" do
    si = produced()
    assert DeclarativeProposition.interpret(%{si | sentence_signatures: []}).reason ==
             :insufficient_grammatical_evidence
  end

  test "dictionary and ATL disagreement are independent reasons for abstention" do
    si = supported()
    cells = Enum.reject(si.active_cells, &(&1.token_index == 2))

    assert DeclarativeProposition.interpret(%{si | active_cells: cells}).reason ==
             :missing_attested_dictionary_roles

    altered =
      Enum.map(si.atl_slate.winners, fn winner ->
        if winner.token_index == 2,
          do: %{winner | id: "supported|noun|1"},
          else: winner
      end)

    assert DeclarativeProposition.interpret(%{si | atl_slate: %{winners: altered}}).reason ==
             :lifg_atl_role_disagreement
  end

  test "unsupported modal, mixed polarity, conjunction, and multiple sentences abstain" do
    for sentence <- [
      "Three links might have supported the claims.",
      "Three links supported and refuted the claims.",
      "Three links supported the claims. Those links were verified.",
      "The checker did not verify or support the claims.",
      "Ten links contradicted the original claims: the details are unclear."
    ] do
      assert %{status: :not_interpreted} =
               DeclarativeProposition.interpret(%{supported() | sentence: sentence})
    end

    assert DeclarativeProposition.interpret(%{produced() | source: :user}).status ==
             :not_interpreted
  end
end
