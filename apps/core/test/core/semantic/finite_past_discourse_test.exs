defmodule Core.Semantic.FinitePastDiscourseTest do
  use ExUnit.Case, async: true

  alias Core.Discourse.Ledger
  alias Core.Semantic.FinitePastDiscourse
  alias Core.Semantic.Composition.FinitePastLocativeIntegration
  alias Core.SemanticInput
  alias Core.Syntax.Signatures
  alias Core.TokenFilters

  defp si(surface) when surface in ["sat", "slept"] do
    lemma = if surface == "sat", do: "sit", else: "sleep"
    words = ["the", "bird", surface, "on", "the", "fence"]
    form_id = "#{surface}|verb|1"
    selected_id = if surface == "sat", do: "sat|noun|1", else: form_id

    cells =
      words
      |> Enum.with_index()
      |> Enum.flat_map(fn {word, index} ->
        if index == 2 do
          verb = %{
            id: form_id, norm: word, pos: "verb", token_index: index,
            definition: "simple past and past participle of #{lemma}",
            semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]
          }

          if surface == "sat",
            do: [%{id: selected_id, norm: word, pos: "noun",
                   token_index: index, semantic_atoms: []}, verb],
            else: [verb]
        else
          pos = case index do
            n when n in [0, 4] -> "determiner"
            n when n in [1, 5] -> "noun"
            3 -> "preposition"
          end

          [%{id: "#{word}|#{pos}|1", norm: word, pos: pos,
             token_index: index, semantic_atoms: []}]
        end
      end)

    choices =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, index} ->
        if index == 2 do
          scores = if surface == "sat",
            do: %{"sat|noun|1" => 0.8, form_id => 0.2},
            else: %{form_id => 1.0}

          %{id: selected_id, token_index: index, candidate_probs: scores}
        else
          pos = case index do
            n when n in [0, 4] -> "determiner"
            n when n in [1, 5] -> "noun"
            3 -> "preposition"
          end
          id = "#{word}|#{pos}|1"
          %{id: id, token_index: index, candidate_probs: %{id => 1.0}}
        end
      end)

    %SemanticInput{
      sentence: "The bird #{surface} on the fence.",
      source: :user,
      session_id: "phase3-observation-only-#{System.unique_integer([:positive])}",
      active_cells: cells
    }
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Map.put(:lifg_choices, choices)
    |> Signatures.attach()
    |> FinitePastLocativeIntegration.attach(lookup: &lookup/1)
  end

  defp lookup("sit") do
    [
      %{id: "sit|verb|2", norm: "sit", pos: "verb",
        definition: "To serve on a jury."},
      %{id: "sit|verb|1", norm: "sit", pos: "verb",
        definition: "To be seated with the upper body is upright."}
    ]
  end

  defp lookup("sleep") do
    [
      %{id: "sleep|verb|2", norm: "sleep", pos: "verb",
        definition: "To have sexual intercourse."},
      %{id: "sleep|verb|1", norm: "sleep", pos: "verb",
        definition: "To rest in a state of reduced consciousness."}
    ]
  end

  test "both attested past verbs project only provisional same-session descriptions" do
    for {surface, expected, selected} <- [
          {"sat", :sit, "sat|noun|1"},
          {"slept", :sleep, "slept|verb|1"}
        ] do
      source = si(surface)
      before = source
      assert {:ok, projected} =
        FinitePastDiscourse.project(source, lookup: &lookup/1)

      assert projected.kind == :reported_finite_past_locative_candidate
      assert projected.status == :provisional
      assert projected.tense == :past
      assert projected.predicate == expected
      assert projected.actor_kind == :bird
      assert projected.relation == :on
      assert projected.landmark_kind == :fence
      assert projected.speaker == :user
      assert projected.speech_act == :declarative_candidate
      assert projected.session_scope == :current_session_only
      assert projected.verification == :conversation_context_only
      assert projected.provenance == :revalidated_current_turn_finite_past_composition
      assert projected.event_existence == :not_asserted
      refute projected.event_occurrence_verified?
      refute projected.referent_identity_verified?
      refute projected.semantic_compatibility_proven?
      refute projected.factual_authority?

      assert projected.typed_proof_sha256 ==
        (:crypto.hash(:sha256,
          :erlang.term_to_binary(source.finite_past_locative_derivation.result))
        |> Base.encode16(case: :lower))
      assert projected.source_turn_sha256 ==
        (:crypto.hash(:sha256, source.sentence) |> Base.encode16(case: :lower))
      assert map_size(projected) <= 20
      refute Map.has_key?(projected, :sentence)
      refute Map.has_key?(projected, :session_id)
      assert source == before
      assert Enum.at(source.lifg_choices, 2).id == selected
      assert :ok == SemanticInput.validate_contract(source)
    end
  end

  test "saved proof alone is insufficient: absent or invalid proof abstains" do
    source = si("slept")
    assert :not_applicable = FinitePastDiscourse.project(
      %{source | finite_past_locative_derivation: nil}, lookup: &lookup/1)
    assert :not_applicable = FinitePastDiscourse.project(
      %{source | finite_past_locative_derivation: 42}, lookup: &lookup/1)

    modified = put_in(source.finite_past_locative_derivation,
      [:event_semantics, :event_existence], :asserted)

    assert :not_applicable = FinitePastDiscourse.project(
      %{source | finite_past_locative_derivation: modified}, lookup: &lookup/1)
  end

  test "replayed proof must exactly match the current turn, not an earlier one" do
    older = si("sat")
    newer = si("slept")

    stale = %{newer |
      finite_past_locative_derivation: older.finite_past_locative_derivation}

    assert :not_applicable = FinitePastDiscourse.project(stale,
      lookup: &lookup/1)
    assert {:ok, _} = FinitePastDiscourse.project(newer, lookup: &lookup/1)
  end

  test "session and origin gates abstain before dictionary access" do
    source = si("sat")
    invalid = [
      %{source | session_id: nil},
      %{source | session_id: ""},
      %{source | session_id: String.duplicate("x", 129)},
      %{source | source: :assistant},
      %{source | source: :llm},
      %{source | source: nil}
    ]

    for modified <- invalid do
      assert :not_applicable = FinitePastDiscourse.project(modified,
        lookup: fn _ -> flunk("unscoped lookup attempted") end)
    end
  end

  test "interrogative, negated, quoted, reported, and modal sentences abstain" do
    source = si("slept")
    invalid = [
      "The bird slept on the fence?",
      "Did the bird sleep on the fence?",
      "The bird did not sleep on the fence.",
      "\"The bird slept on the fence.\"",
      "He said the bird slept on the fence.",
      "The bird might have slept on the fence.",
      "The bird slept on the fence! And flew away.",
      String.duplicate("a", 193)
    ]

    for sentence <- invalid do
      assert :not_applicable = FinitePastDiscourse.project(
        %{source | sentence: sentence},
        lookup: fn _ -> flunk("nondeclarative lookup attempted") end)
    end
  end

  test "scored morphology and independent lemma definitions remain mandatory" do
    source = si("slept")
    assert :not_applicable = FinitePastDiscourse.project(source,
      lookup: fn "sleep" -> [] end)
    assert :not_applicable = FinitePastDiscourse.project(source,
      lookup: fn "sleep" -> raise "database not available" end)

    altered = %{source | lifg_choices:
      Enum.map(source.lifg_choices, fn
        %{token_index: 2} = choice -> %{choice | candidate_probs: %{}}
        choice -> choice
      end)}
    assert :not_applicable = FinitePastDiscourse.project(altered,
      lookup: &lookup/1)
  end

  test "reordering unrelated lemma senses cannot change the candidate" do
    source = si("slept")
    assert {:ok, first} = FinitePastDiscourse.project(source,
      lookup: &lookup/1)
    assert {:ok, second} = FinitePastDiscourse.project(source,
      lookup: fn "sleep" -> Enum.reverse(lookup("sleep")) end)
    assert first == second
  end

  test "malformed input and non-keyword options abstain safely" do
    assert :not_applicable = FinitePastDiscourse.project(nil)
    assert :not_applicable = FinitePastDiscourse.project(%{}, [])
    assert :not_applicable = FinitePastDiscourse.project(si("sat"), :invalid)
    assert :not_applicable = FinitePastDiscourse.project(si("sat"), [42])
  end

  defp cleanup(session) do
    on_exit(fn -> Ledger.clear(session) end)
  end

  test "default production ledger semantics are unchanged until explicit Phase 3 opt-in" do
    source = si("sat")
    cleanup(source.session_id)

    recorded = Ledger.record(source,
      finite_past_lemma_lookup: fn _ -> flunk("disabled Phase 3 touched dictionary") end)

    assert recorded.lifg_choices == source.lifg_choices
    assert [%{kind: :uninterpreted, seq: 1, factual_authority?: false}] =
      Ledger.entries(source.session_id)
  end

  test "opt-in Ledger records a finite-past report, never a verified event or answer" do
    for {surface, predicate, selected} <- [
          {"sat", :sit, "sat|noun|1"},
          {"slept", :sleep, "slept|verb|1"}
        ] do
      source = si(surface)
      cleanup(source.session_id)
      assert is_map(source.finite_past_locative_derivation)

      result = Ledger.record(source, finite_past_discourse_ledger?: true,
        finite_past_lemma_lookup: &lookup/1)

      assert result.lifg_choices == source.lifg_choices
      assert result.active_cells == source.active_cells
      assert result.sentence == source.sentence
      assert Enum.at(result.lifg_choices, 2).id == selected

      assert [%{seq: 1, kind: :reported_finite_past_locative_candidate,
                status: :provisional, predicate: ^predicate,
                actor_kind: :bird, relation: :on, landmark_kind: :fence,
                tense: :past, event_existence: :not_asserted,
                event_occurrence_verified?: false, referent_identity_verified?: false,
                factual_authority?: false, verification: :conversation_context_only,
                speech_act: :declarative_candidate, speaker: :user} = entry] =
        Ledger.entries(source.session_id)

      assert entry.typed_proof_sha256 ==
        (:crypto.hash(:sha256,
          :erlang.term_to_binary(source.finite_past_locative_derivation.result))
          |> Base.encode16(case: :lower))
      assert entry.source_turn_sha256 ==
        (:crypto.hash(:sha256, source.sentence) |> Base.encode16(case: :lower))

      assert map_size(entry) <= 21
      refute Map.has_key?(entry, :sentence)
      refute Map.has_key?(entry, :session_id)
      refute Map.has_key?(entry, :lifg_choices)
      refute Map.has_key?(entry, :active_cells)

      # Gate 3 is not enabled: no automatic retrieval of this kind is legal.
      assert :not_found == Ledger.resolve(source.session_id, "Where was the bird?")
      assert :not_found == Ledger.resolve(source.session_id, "that bird")
      assert Enum.any?(result.trace, fn
        %{stage: :discourse_ledger, decision: :recorded,
          meta: %{sequence: 1, kind: :reported_finite_past_locative_candidate,
                  factual_authority?: false}} -> true
        _ -> false
      end)
    end
  end

  test "opt-in records never admit a tampered or stale attached proof" do
    source = si("slept")
    former = si("sat")
    cleanup(source.session_id)

    bad_event = put_in(source.finite_past_locative_derivation,
      [:event_semantics, :event_existence], :asserted)

    invalid = [
      %{source | finite_past_locative_derivation: nil},
      %{source | finite_past_locative_derivation: bad_event},
      %{source | finite_past_locative_derivation: former.finite_past_locative_derivation},
      %{source | sentence: "The bird slept on the fence?"},
      %{source | intent: :ask}
    ]

    for altered <- invalid do
      assert is_map(altered)
      Ledger.record(altered, finite_past_discourse_ledger?: true,
        finite_past_lemma_lookup: &lookup/1)
    end

    records = Ledger.entries(source.session_id)
    assert length(records) == length(invalid)
    assert Enum.all?(records, &(&1.kind == :uninterpreted))
    assert Enum.map(records, & &1.seq) == Enum.to_list(length(invalid)..1//-1)
  end

  test "opt-in Ledger rejects unsupported dictionary, morphology and origin evidence" do
    source = si("slept")
    cleanup(source.session_id)

    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: fn "sleep" -> [] end)

    without_scores = %{source | lifg_choices:
      Enum.map(source.lifg_choices, fn
        %{token_index: 2} = choice -> %{choice | candidate_probs: %{}}
        choice -> choice
      end)}

    Ledger.record(without_scores, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(%{source | source: :assistant}, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(%{source | session_id: nil}, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert Enum.map(Ledger.entries(source.session_id), & &1.kind) ==
      [:uninterpreted, :uninterpreted]
  end

  test "session boundaries and evidence-only records preserve bounded sequence" do
    first = si("sat")
    another = si("slept")
    cleanup(first.session_id)
    cleanup(another.session_id)

    Ledger.record(first, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(another, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(%{first | sentence: "Not a supported construction."})

    assert Enum.map(Ledger.entries(first.session_id), & &1.seq) == [2, 1]
    assert Enum.map(Ledger.entries(another.session_id), & &1.seq) == [1]
    assert Enum.map(Ledger.entries(first.session_id), & &1.kind) ==
      [:uninterpreted, :reported_finite_past_locative_candidate]
    assert Enum.map(Ledger.entries(another.session_id), & &1.kind) ==
      [:reported_finite_past_locative_candidate]

    assert :not_found = Ledger.resolve(first.session_id, "Where was the bird?")
    assert :not_found = Ledger.resolve(another.session_id, "Where was the bird?")
    assert :ok = Ledger.clear(first.session_id)
    assert Ledger.entries(first.session_id) == []
    assert length(Ledger.entries(another.session_id)) == 1
  end

  test "question-intent mismatch never becomes speaker-asserted discourse" do
    source = si("sat")
    assert :not_applicable = FinitePastDiscourse.project(%{source | intent: :ask},
      lookup: &lookup/1)
    assert :not_applicable = FinitePastDiscourse.project(%{source | intent: :command},
      lookup: &lookup/1)
  end

end
