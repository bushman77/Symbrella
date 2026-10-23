defmodule Core.Social.DeclarativePropositionTest do
  use ExUnit.Case, async: true

  alias Core.Social.DeclarativeProposition

  defp fixture(sentence, opts) do
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

  defp verified do
    fixture("The checker verified those claims.",
      roles: [{1, "noun"}, {2, "verb"}, {4, "noun"}],
      noun_phrases: [{0, 1}, {3, 4}])
  end

  defp not_supported do
    fixture("Three links did not support the claims.",
      roles: [{1, "noun"}, {2, "verb"}, {3, "adverb"},
              {4, "verb"}, {6, "noun"}],
      noun_phrases: [{5, 6}])
  end

  @contrastive_cases [
    {"The checker verified those claims.", :affirmative_transitive,
     :reported_action, :positive, nil},
    {"The checker did not verify those claims.", :explicit_negated_transitive,
     :reported_action, :negative, nil},
    {"Three links supported the claims.", :counted_transitive,
     :reported_support, :positive, 3},
    {"Three links did not support the claims.", :counted_negated_transitive,
     :reported_support, :negative, 3}
  ]

  test "contrastive benchmark preserves independent predicate and polarity" do
    si_by_sentence = %{
      "The checker verified those claims." => verified(),
      "The checker did not verify those claims." => negated(),
      "Three links supported the claims." => supported(),
      "Three links did not support the claims." => not_supported()
    }

    for {sentence, construction, relation, polarity, count} <- @contrastive_cases do
      result = DeclarativeProposition.interpret(Map.fetch!(si_by_sentence, sentence))
      assert result.status == :attributed_structural_interpretation
      assert result.construction == construction
      assert result.relation == relation
      assert result.polarity == polarity
      assert result.reported_count == count
      assert result.verification == :author_claim_not_independently_verified
      assert result.trust == :untrusted_external
    end
  end

  defp finite_present_fixture(sentence, mode) do
    si =
      fixture(sentence,
        roles: [{1, "noun"}, {2, "verb"}, {4, "noun"}],
        noun_phrases:
          if(mode == :singular, do: [{0, 1}, {3, 4}], else: [{3, 4}])
      )

    tags = ["tag:form-of", "tag:indicative", "tag:present",
            "tag:singular", "tag:third-person"]

    cells =
      Enum.map(si.active_cells, fn cell ->
        case {mode, cell.token_index} do
          {:singular, 2} -> Map.put(cell, :semantic_atoms, tags)
          {:counted, 1} ->
            Map.put(cell, :semantic_atoms, ["tag:form-of", "tag:plural"])
          _ -> Map.put(cell, :semantic_atoms, [])
        end
      end)

    signature =
      case mode do
        :singular ->
          %{id: :finite_present_svo_candidate, source: :core_syntax,
            span: {0, 4},
            roles: %{subject_head: 1, predicate: 2, object_head: 4},
            evidence: [:contiguous_surface, :attested_nominal_subject,
                       :attested_nominal_object, :attested_finite_present_verb]}

        :counted ->
          %{id: :counted_finite_present_svo_candidate, source: :core_syntax,
            span: {0, 4},
            roles: %{count: 0, subject_head: 1, predicate: 2,
                     object_determiner: 3, object_head: 4},
            evidence: [:contiguous_surface, :attested_counted_plural_subject,
                       :attested_base_present_verb, :attested_nominal_object]}
      end

    %{si | active_cells: cells, sentence_signatures: [signature | si.sentence_signatures]}
  end

  test "COLONY-09 singular finite-present report requires real morphology and agreement" do
    si = finite_present_fixture("The reviewer verifies the report.", :singular)

    proposition = DeclarativeProposition.interpret(si)
    assert proposition.status == :attributed_structural_interpretation
    assert proposition.construction == :finite_present_transitive
    assert proposition.relation == :reported_action
    assert proposition.polarity == :positive
    assert proposition.subject == "reviewer"
    assert proposition.predicate == "verifies"
    assert proposition.object == "report"
    assert proposition.verification == :author_claim_not_independently_verified

    without_signature = %{si | sentence_signatures:
      Enum.reject(si.sentence_signatures, &(&1.id == :finite_present_svo_candidate))}
    assert DeclarativeProposition.interpret(without_signature).reason ==
             :insufficient_grammatical_evidence

    without_tags = %{si | active_cells:
      Enum.map(si.active_cells, fn cell ->
        if cell.token_index == 2, do: Map.put(cell, :semantic_atoms, []), else: cell
      end)}
    assert DeclarativeProposition.interpret(without_tags).reason ==
             :insufficient_grammatical_evidence

    plural_subject = %{si | active_cells:
      Enum.map(si.active_cells, fn cell ->
        if cell.token_index == 1,
          do: Map.put(cell, :semantic_atoms, ["tag:form-of", "tag:plural"]),
          else: cell
      end)}
    assert DeclarativeProposition.interpret(plural_subject).reason ==
             :insufficient_grammatical_evidence

    wrong_winner = %{si | atl_slate: %{winners:
      Enum.map(si.atl_slate.winners, fn
        %{token_index: 2} = winner -> %{winner | id: "verifies|noun|1"}
        winner -> winner
      end)}}
    assert DeclarativeProposition.interpret(wrong_winner).reason ==
             :lifg_atl_role_disagreement
  end

  test "COLONY-09 counted plural finite-present supports report only with plural witness" do
    si = finite_present_fixture("Four reports support the claims.", :counted)
    proposition = DeclarativeProposition.interpret(si)

    assert proposition.status == :attributed_structural_interpretation
    assert proposition.construction == :counted_present_transitive
    assert proposition.relation == :reported_support
    assert proposition.polarity == :positive
    assert proposition.subject == "reports"
    assert proposition.predicate == "support"
    assert proposition.object == "claims"
    assert proposition.reported_count == 4

    no_plural = %{si | active_cells:
      Enum.map(si.active_cells, fn cell ->
        if cell.token_index == 1, do: Map.put(cell, :semantic_atoms, []), else: cell
      end)}
    assert DeclarativeProposition.interpret(no_plural).reason ==
             :insufficient_grammatical_evidence

    no_signature = %{si | sentence_signatures:
      Enum.reject(si.sentence_signatures,
        &(&1.id == :counted_finite_present_svo_candidate))}
    assert DeclarativeProposition.interpret(no_signature).reason ==
             :insufficient_grammatical_evidence

    singular_count = finite_present_fixture(
      "One report support the claims.", :counted)
    assert DeclarativeProposition.interpret(singular_count).reason ==
             :unsupported_subject_verb_agreement

    wrong_sense = %{si | atl_slate: %{winners:
      Enum.map(si.atl_slate.winners, fn
        %{token_index: 2} = winner -> %{winner | id: "support|noun|1"}
        winner -> winner
      end)}}
    assert DeclarativeProposition.interpret(wrong_sense).reason ==
             :lifg_atl_role_disagreement
  end

  test "missing attested negative support predicate reports the exact unresolved role" do
    si = not_supported()
    without_predicate = %{
      si | active_cells: Enum.reject(si.active_cells, &(&1.token_index == 4))
    }

    result = DeclarativeProposition.interpret(without_predicate)

    assert result.reason == :missing_attested_dictionary_roles
    assert %{role: :predicate, token_index: 4, expected_pos: "verb",
             dictionary_pos_available?: false, agreed?: false} =
             Enum.find(result.role_audit, &(&1.role == :predicate))

    assert Enum.any?(result.role_audit, fn role ->
      role.role == :negator and role.agreed? == true
    end)
  end

  test "counted negative support accepts exact did-not lexical witnesses" do
    si = not_supported()

    winners = Enum.map(si.atl_slate.winners, fn
      %{token_index: 2} = winner -> %{winner | id: "did|auxiliary|0"}
      %{token_index: 3} = winner -> %{winner | id: "not|particle|0"}
      winner -> winner
    end)

    result = DeclarativeProposition.interpret(%{si | atl_slate: %{winners: winners}})
    assert result.status == :attributed_structural_interpretation
    assert result.reported_count == 3
    assert result.relation == :reported_support
    assert result.polarity == :negative
    refute result.relation == :reported_refutation

    assert DeclarativeProposition.interpret(
      %{si | active_cells: Enum.reject(si.active_cells, &(&1.token_index == 3)),
             atl_slate: %{winners: winners}}
    ).reason == :missing_attested_dictionary_roles
  end

  test "determined and counted modal statements abstain as uncertainty, not invalid number" do
    for modal <- ~w(might may could) do
      for {sentence, phrases} <- [
            {"The reviewer #{modal} verify those claims.", [{0, 1}, {4, 5}]},
            {"Three reports #{modal} support the claims.", [{4, 5}]}
          ] do
        si =
          fixture(sentence,
            roles: [{1, "noun"}, {3, "verb"}, {5, "noun"}],
            noun_phrases: phrases)

        result = DeclarativeProposition.interpret(si)
        assert result.status == :not_interpreted
        assert result.reason == :modal_uncertainty
        assert result.verification == :external_unverified
        refute Map.has_key?(result, :polarity)
      end
    end
  end

  test "modal support is explicitly unresolved, not affirmative evidence" do
    modal = fixture("Three links might support the claims.",
      roles: [{1, "noun"}, {2, "auxiliary"}, {3, "verb"}, {5, "noun"}],
      noun_phrases: [{4, 5}])

    result = DeclarativeProposition.interpret(modal)
    assert result.status == :not_interpreted
    assert result.reason == :modal_uncertainty
    assert result.verification == :external_unverified
    refute Map.has_key?(result, :polarity)
  end

  test "contrastive grammar still rejects missing noun phrases and mismatched ATL verbs" do
    affirmative = verified()
    unsupported = %{affirmative | sentence_signatures: []}
    assert DeclarativeProposition.interpret(unsupported).reason ==
             :insufficient_grammatical_evidence

    negative = not_supported()
    winners = Enum.map(negative.atl_slate.winners, fn
      %{token_index: 4} = winner -> %{winner | id: "support|noun|1"}
      winner -> winner
    end)
    failed = DeclarativeProposition.interpret(%{negative | atl_slate: %{winners: winners}})
    assert failed.reason == :lifg_atl_role_disagreement
    assert Enum.any?(failed.role_audit, &(&1.role == :predicate and &1.agreed? == false))
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

  test "did-not accepts exact attested closed-class auxiliary without inventing a dictionary sense" do
    si = negated()

    winners =
      Enum.map(si.atl_slate.winners, fn winner ->
        if winner.token_index == 2,
          do: %{winner | id: "did|auxiliary|0"},
          else: winner
      end)

    interpreted =
      DeclarativeProposition.interpret(%{si | atl_slate: %{winners: winners}})

    assert interpreted.status == :attributed_structural_interpretation
    assert interpreted.polarity == :negative

    no_dictionary =
      %{si | active_cells: Enum.reject(si.active_cells, &(&1.token_index == 2)),
             atl_slate: %{winners: winners}}

    assert DeclarativeProposition.interpret(no_dictionary).reason ==
             :missing_attested_dictionary_roles
  end

  test "a bounded regular-past verb remains grammatical when dictionary omits form-of tags" do
    for si <- [produced(), contradicted(), supported()] do
      # Missing finite-past morphology is not missing grammatical evidence
      # for these explicitly scoped, dictionary-attested regular-past forms.
      si = %{si | sentence_signatures:
        Enum.reject(si.sentence_signatures, &(&1.id == :finite_past_candidate))}
      proposition = DeclarativeProposition.interpret(si)

      assert proposition.status == :attributed_structural_interpretation
      assert proposition.verification == :author_claim_not_independently_verified
    end
  end

  test "an absent noun phrase is reported individually without guessing a parse" do
    si = contradicted()
    without_object = %{si | sentence_signatures: []}
    result = DeclarativeProposition.interpret(without_object)

    assert result.reason == :insufficient_grammatical_evidence
    assert [%{determiner: 3, head: 5, witnessed?: false}] =
             result.grammar_audit.noun_phrases
  end

  test "an unsupported predicate cannot bypass lexical selection via a surface shape" do
    si = %{produced() | sentence: "The checker appeared 13 links."}
    assert DeclarativeProposition.interpret(si).status == :not_interpreted
  end

  test "did-not recognizes an attested Stage1 not-particle without inventing a dictionary sense" do
    si = negated()
    winners =
      Enum.map(si.atl_slate.winners, fn winner ->
        case winner.token_index do
          2 -> %{winner | id: "did|auxiliary|0"}
          3 -> %{winner | id: "not|particle|0"}
          _ -> winner
        end
      end)

    result = DeclarativeProposition.interpret(%{si | atl_slate: %{winners: winners}})
    assert result.status == :attributed_structural_interpretation
    assert result.polarity == :negative
    assert result.verification == :author_claim_not_independently_verified

    no_negator =
      %{si | active_cells: Enum.reject(si.active_cells, &(&1.token_index == 3)),
             atl_slate: %{winners: winners}}

    assert DeclarativeProposition.interpret(no_negator).reason ==
             :missing_attested_dictionary_roles
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

  test "token stream must exactly correspond to the claimed source sentence" do
    si = produced()
    altered = Enum.map(si.tokens, fn
      %{index: 4} = token -> %{token | phrase: "claims"}
      token -> token
    end)

    assert DeclarativeProposition.interpret(%{si | tokens: altered}).reason ==
             :unsupported_surface
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
