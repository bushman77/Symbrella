defmodule Core.Semantic.FinitePastDiscourseTest do
  use ExUnit.Case, async: true

  alias Core.Discourse.Ledger
  alias Core.Semantic.FinitePastDiscourse
  alias Core.Semantic.FinitePastDiscourseQuestion
  alias Core.Response.FinitePastDiscourseReadout
  alias Core.Response.FinitePastDiscourseArbitration, as: Arbitration
  alias Core.Response.FinitePastResponseGate, as: ResponseGate
  alias Core.Response.Attach, as: ResponseAttach
  alias Core.Discourse.Ledger.Owner
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


  test "past question reads admitted report as an attributed role, never world truth" do
    for {surface, lemma, selected} <- [
          {"sat", :sit, "sat|noun|1"},
          {"slept", :sleep, "slept|verb|1"}
        ] do
      source = si(surface)
      cleanup(source.session_id)

      Ledger.record(source, finite_past_discourse_ledger?: true,
        finite_past_lemma_lookup: &lookup/1)
      before = Ledger.entries(source.session_id)

      assert {:ok, action} =
        FinitePastDiscourseQuestion.project(source.session_id,
          "What was the bird doing?")

      assert action.status == :speaker_description_candidate
      assert action.requested_role == :predicate
      assert action.actor_kind == :bird
      assert action.description == %{
        kind: :reported_predicate, lemma: lemma, tense: :past}
      assert action.source_turn_seq == 1
      assert action.source_turn_sha256 == hd(before).source_turn_sha256
      assert action.typed_proof_sha256 == hd(before).typed_proof_sha256
      assert action.evidence == :admitted_revalidated_finite_past_report
      assert action.verification == :conversation_context_only
      refute action.referent_identity_verified?
      refute action.event_occurrence_verified?
      refute action.factual_authority?

      assert {:ok, location} =
        FinitePastDiscourseQuestion.project(source.session_id,
          "Where was the bird?")

      assert location.description == %{
        kind: :reported_location, relation: :on,
        landmark_kind: :fence, tense: :past}
      assert location.source_turn_seq == action.source_turn_seq
      assert location.requested_role == :location

      # No writes, output text or selection changes occur on projection.
      assert Ledger.entries(source.session_id) == before
      assert Enum.at(source.lifg_choices, 2).id == selected
      refute Map.has_key?(action, :answer_text)
      refute Map.has_key?(action, :sentence)
      refute Map.has_key?(action, :session_id)
    end
  end

  test "repeated actor-kind reports produce ambiguity, never a recency winner" do
    earlier = si("sat")
    later = %{si("slept") | session_id: earlier.session_id}
    cleanup(earlier.session_id)

    Ledger.record(earlier, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(later, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    for question <- ["What was the bird doing?", "Where was the bird?"] do
      assert {:ambiguous, [newer, older]} =
        FinitePastDiscourseQuestion.project(earlier.session_id, question)
      assert newer.source_turn_seq == 2
      assert older.source_turn_seq == 1
      assert newer.actor_kind == :bird
      assert older.actor_kind == :bird
      refute newer.factual_authority?
      refute older.referent_identity_verified?
      refute Map.has_key?(newer, :predicate)
      refute Map.has_key?(newer, :sentence)
    end
  end

  test "unrelated or intervening turn prevents stale report revival" do
    source = si("sat")
    cleanup(source.session_id)

    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    assert {:ok, _} = FinitePastDiscourseQuestion.project(
      source.session_id, "Where was the bird?")

    Ledger.record(%{source | sentence: "An unrelated question?",
                            finite_past_locative_derivation: nil})

    assert :not_applicable = FinitePastDiscourseQuestion.project(
      source.session_id, "Where was the bird?")
    assert Enum.map(Ledger.entries(source.session_id), & &1.kind) ==
      [:uninterpreted, :reported_finite_past_locative_candidate]
  end

  test "other mentions of the same actor kind block unsupported identity joins" do
    source = si("slept")
    cleanup(source.session_id)

    # Another mention of a bird has no independent same-individual proof.
    assert {:ok, 1} = Owner.append(source.session_id,
      %{kind: :nominal_description, head: "bird",
        status: :provisional, factual_authority?: false})

    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert :not_applicable = FinitePastDiscourseQuestion.project(
      source.session_id, "What was the bird doing?")
  end

  test "Owner refuses forged or replayed finite-past records without source proof" do
    source = si("sat")
    cleanup(source.session_id)

    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    good = hd(Ledger.entries(source.session_id))
    assert good.seq == 1

    assert {:error, :verified_finite_past_required} =
      Owner.append(source.session_id, good)

    assert {:error, :verified_finite_past_required} =
      Owner.append(source.session_id, %{good | source_turn_sha256: "invalid"})

    # Calling the GenServer directly must not bypass the public API's guard.
    assert {:error, :verified_finite_past_required} =
      GenServer.call(Owner, {:append, source.session_id, good})

    assert Enum.map(Ledger.entries(source.session_id), & &1.seq) == [1]
    assert {:ok, %{description: %{lemma: :sit}}} =
      FinitePastDiscourseQuestion.project(source.session_id,
        "What was the bird doing?")

    # Other kinds remain appendable; an intervening unsupported turn
    # conservatively blocks the older source rather than reviving it.
    assert {:ok, 2} = Owner.append(source.session_id,
      %{kind: :uninterpreted, status: :no_verified_semantics,
        factual_authority?: false})

    assert :not_applicable = FinitePastDiscourseQuestion.project(
      source.session_id, "Where was the bird?")
  end

  test "owner admission independently rejects mismatched session, proof and descriptor" do
    source = si("slept")
    cleanup(source.session_id)

    assert {:ok, candidate} =
      FinitePastDiscourse.project(source, lookup: &lookup/1)

    altered = %{candidate | predicate: :sit}
    assert {:error, :invalid_finite_past_evidence} =
      Owner.append_verified_finite_past(source.session_id, source, altered, &lookup/1)

    assert {:error, :invalid_finite_past_evidence} =
      Owner.append_verified_finite_past("different-session", source, candidate, &lookup/1)

    stale = %{source | sentence: "The bird sat on the fence."}
    assert {:error, :invalid_finite_past_evidence} =
      Owner.append_verified_finite_past(source.session_id, stale, candidate, &lookup/1)

    assert {:error, :invalid_finite_past_evidence} =
      Owner.append_verified_finite_past(source.session_id, source, candidate,
        fn "sleep" -> [] end)

    assert Ledger.entries(source.session_id) == []

    assert {:ok, 1} = Owner.append_verified_finite_past(
      source.session_id, source, candidate, &lookup/1)

    assert [%{kind: :reported_finite_past_locative_candidate,
              predicate: :sleep, seq: 1, factual_authority?: false}] =
      Ledger.entries(source.session_id)
  end

  test "closed past-question grammar rejects present, pronouns, and malformed input" do
    source = si("slept")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    for question <- [
          "What is the bird doing?",
          "Where is the bird?",
          "Where was it?",
          "What was it doing?",
          "What was a bird doing?",
          "What was the bird doing",
          "What was the bird doing? And where?",
          "What was the tree doing?",
          "Did the bird sleep on the fence?",
          "\"Where was the bird?\"",
          String.duplicate("?", 129)
        ] do
      assert :not_applicable = FinitePastDiscourseQuestion.project(
        source.session_id, question)
    end

    assert {:ok, :bird, :predicate} =
      FinitePastDiscourseQuestion.parse_question("What was the bird doing?")
    assert {:ok, :bird, :location} =
      FinitePastDiscourseQuestion.parse_question("Where was the bird?")
    assert :not_applicable = FinitePastDiscourseQuestion.parse_question(nil)
  end

  test "sessions never cross and unsupported actor kinds cannot be inferred" do
    source = si("sat")
    other = si("slept")
    cleanup(source.session_id)
    cleanup(other.session_id)

    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert :not_applicable = FinitePastDiscourseQuestion.project(
      other.session_id, "What was the bird doing?")
    assert :not_applicable = FinitePastDiscourseQuestion.project(
      source.session_id, "Where was the cat?")
    assert :not_applicable = FinitePastDiscourseQuestion.project(
      nil, "Where was the bird?")
    assert :not_applicable = FinitePastDiscourseQuestion.project(
      "", "Where was the bird?")
    assert :not_applicable = FinitePastDiscourseQuestion.project(
      String.duplicate("s", 129), "Where was the bird?")
  end

  test "question projection remains separate from legacy Ledger.resolve and the planner" do
    source = si("sat")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert {:ok, %{description: %{lemma: :sit}}} =
      FinitePastDiscourseQuestion.project(source.session_id,
        "What was the bird doing?")

    assert :not_found == Ledger.resolve(source.session_id,
      "Where was the bird?")
    assert :not_found == Ledger.resolve(source.session_id,
      "What was the bird doing?")
  end


  test "read-only answer adapter describes the report without claiming an event" do
    for {surface, description_fragment} <- [
          {"sat", "a bird as sitting"},
          {"slept", "a bird as sleeping"}
        ] do
      source = si(surface)
      cleanup(source.session_id)
      Ledger.record(source, finite_past_discourse_ledger?: true,
        finite_past_lemma_lookup: &lookup/1)
      before = Ledger.entries(source.session_id)

      assert {:ok, action_text, action_meta} =
        FinitePastDiscourseReadout.read(source.session_id,
          "What was the bird doing?")
      assert String.contains?(action_text, description_fragment)
      assert String.contains?(action_text, "not an independently verified event")
      assert action_meta.status == :speaker_description_candidate
      assert action_meta.source_turn_seq == 1
      assert action_meta.response_source == :symbolic_finite_past_report_readout
      assert action_meta.verification == :conversation_context_only
      refute action_meta.event_occurrence_verified?
      refute action_meta.referent_identity_verified?
      refute action_meta.factual_authority?
      refute action_meta.llm_called?

      assert {:ok, location_text, location_meta} =
        FinitePastDiscourseReadout.read(source.session_id,
          "Where was the bird?")
      assert String.contains?(location_text, "a bird as being on a fence")
      assert String.contains?(location_text, "not an independently verified event")
      assert location_meta.source_turn_seq == 1
      assert Ledger.entries(source.session_id) == before
      assert Ledger.resolve(source.session_id, "Where was the bird?") == :not_found
    end
  end

  test "read-only answer adapter preserves ambiguity instead of recency" do
    first = si("sat")
    second = %{si("slept") | session_id: first.session_id}
    cleanup(first.session_id)
    Ledger.record(first, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(second, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    before = Ledger.entries(first.session_id)

    assert {:ambiguous, text, meta} = FinitePastDiscourseReadout.read(
      first.session_id, "What was the bird doing?")
    assert String.contains?(text, "multiple descriptions of a bird")
    assert String.contains?(text, "cannot establish which one")
    assert meta.source_turn_sequences == [2, 1]
    assert meta.status == :referent_ambiguous
    refute meta.factual_authority?
    refute meta.referent_identity_verified?
    refute meta.llm_called?
    assert Ledger.entries(first.session_id) == before
  end

  test "read-only answer adapter never falls back past stale or irrelevant turns" do
    source = si("slept")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert :not_applicable == FinitePastDiscourseReadout.read(
      source.session_id, "Where is the bird?")
    assert :not_applicable == FinitePastDiscourseReadout.read(
      source.session_id, "What was it doing?")
    assert :not_applicable == FinitePastDiscourseReadout.read(
      "other-session", "Where was the bird?")
    assert :not_applicable == FinitePastDiscourseReadout.read(
      nil, "Where was the bird?")
    assert :not_applicable == FinitePastDiscourseReadout.read(
      source.session_id, "Where was the dog?")

    Ledger.record(%{source | sentence: "Unrelated conversation.",
                            finite_past_locative_derivation: nil})
    assert :not_applicable == FinitePastDiscourseReadout.read(
      source.session_id, "Where was the bird?")
  end



  test "arbitration is default off even when a valid report is admitted" do
    source = si("sat")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert :pass = Arbitration.decide(source.session_id, "Where was the bird?")
    assert :pass = Arbitration.decide(source.session_id, "Where was the bird?",
      finite_past_response_arbitration?: false)
    assert {:fallback, :legacy_reply} = Arbitration.route(
      source.session_id, "Where was the bird?", fn -> :legacy_reply end)

    assert Enum.at(source.lifg_choices, 2).id == "sat|noun|1"
  end

  test "opt-in arbitration returns qualified report and suppresses fallback" do
    source = si("slept")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    parent = self()
    dangerous_fallback = fn ->
      send(parent, :wrong_episodic_answer)
      {"An unrelated historical response.", %{}}
    end

    assert {:answer, text, meta} = Arbitration.route(
      source.session_id, "What was the bird doing?", dangerous_fallback,
      finite_past_response_arbitration?: true)

    assert String.contains?(text, "bird as sleeping")
    assert String.contains?(text, "not an independently verified event")
    assert meta.arbitration == :finite_past_report
    assert meta.status == :speaker_description_candidate
    assert meta.response_source == :symbolic_finite_past_report_readout
    refute meta.event_occurrence_verified?
    refute meta.referent_identity_verified?
    refute meta.factual_authority?
    refute meta.llm_called?
    refute_received :wrong_episodic_answer
    assert Ledger.entries(source.session_id) |> length() == 1
  end

  test "multiple actor-kind reports require clarification and never hit fallback" do
    first = si("sat")
    second = %{si("slept") | session_id: first.session_id}
    cleanup(first.session_id)
    Ledger.record(first, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(second, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    assert {:clarify, text, meta} = Arbitration.route(
      first.session_id, "Where was the bird?",
      fn -> flunk("ambiguous report fell through to old episodic memory") end,
      finite_past_response_arbitration?: true)

    assert String.contains?(text, "multiple descriptions")
    assert meta.status == :referent_ambiguous
    assert meta.source_turn_sequences == [2, 1]
    assert meta.arbitration == :finite_past_report
    refute meta.factual_authority?
  end

  test "intervening unrelated turn blocks fallback instead of reviving stale episode" do
    source = si("slept")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    Ledger.record(%{source | sentence: "Unrelated conversation.",
                            finite_past_locative_derivation: nil})

    assert {:abstain, text, meta} = Arbitration.route(
      source.session_id, "Where was the bird?",
      fn -> flunk("stale report fell through to older memory") end,
      finite_past_response_arbitration?: true)

    assert String.contains?(text, "cannot safely connect")
    assert meta.status == :continuity_unresolved
    assert meta.response_source == :symbolic_finite_past_report_arbitration
    assert meta.arbitration == :finite_past_report
    refute meta.referent_identity_verified?
    refute meta.factual_authority?
    refute meta.llm_called?
  end

  test "different actor or unsupported question preserves ordinary memory behavior" do
    source = si("sat")
    other = si("slept")
    cleanup(source.session_id)
    cleanup(other.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    for {session, question} <- [
          {source.session_id, "Where was the cat?"},
          {source.session_id, "Where is the bird?"},
          {source.session_id, "What was it doing?"},
          {source.session_id, "What was the bird doing"},
          {other.session_id, "Where was the bird?"}
        ] do
      assert {:fallback, :ordinary} = Arbitration.route(session, question,
        fn -> :ordinary end, finite_past_response_arbitration?: true)
    end
  end

  test "no ledger reports preserves fallback and never fabricates an answer" do
    source = si("sat")
    cleanup(source.session_id)
    assert {:fallback, :none_found} = Arbitration.route(
      source.session_id, "Where was the bird?",
      fn -> :none_found end, finite_past_response_arbitration?: true)
    assert :pass == Arbitration.decide(nil, "Where was the bird?",
      finite_past_response_arbitration?: true)
    assert :pass == Arbitration.decide(source.session_id, nil,
      finite_past_response_arbitration?: true)
    assert :pass == Arbitration.decide(source.session_id, "Where was the bird?",
      [42])
  end



  test "opt-in response attachment uses same-session qualified finite-past answer" do
    source = si("slept")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    before = Ledger.entries(source.session_id)
    question = %SemanticInput{
      session_id: source.session_id, source: :user, intent: :question,
      confidence: 0.8, sentence: "What was the bird doing?"
    }

    result = ResponseAttach.maybe_build_response_plan(question,
      finite_past_response_arbitration?: true)

    assert String.contains?(result.response_text, "bird as sleeping")
    assert String.contains?(result.response_text, "not an independently verified event")
    assert result.response_meta.arbitration == :finite_past_report
    assert result.response_meta.response_source == :symbolic_finite_past_report
    assert result.response_meta.memory_source == :same_session_finite_past_report
    assert result.response_meta.status == :speaker_description_candidate
    assert result.response_meta.source_turn_seq == 1
    refute result.response_meta.factual_authority?
    refute result.response_meta.event_occurrence_verified?
    refute result.response_meta.referent_identity_verified?
    refute result.response_meta.llm_called?
    assert Ledger.entries(source.session_id) == before
    assert Enum.at(source.lifg_choices, 2).id == "slept|verb|1"
  end

  test "opt-in response attachment clarifies repeated actors without guessing" do
    first = si("sat")
    second = %{si("slept") | session_id: first.session_id}
    cleanup(first.session_id)
    Ledger.record(first, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(second, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    question = %SemanticInput{
      source: :user, session_id: first.session_id,
      intent: :question, sentence: "Where was the bird?"
    }

    result = ResponseAttach.maybe_build_response_plan(question,
      finite_past_response_arbitration?: true)

    assert String.contains?(result.response_text, "multiple descriptions")
    assert result.response_meta.arbitration == :finite_past_report
    assert result.response_meta.status == :referent_ambiguous
    assert result.response_meta.source_turn_sequences == [2, 1]
    refute result.response_meta.retrieval_found?
    refute result.response_meta.referent_identity_verified?
    refute result.response_meta.factual_authority?
    assert Enum.map(Ledger.entries(first.session_id), & &1.seq) == [2, 1]
  end

  test "opt-in response attachment blocks stale episodic fallback" do
    source = si("sat")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)
    Ledger.record(%{source | sentence: "An unrelated comment.",
                            finite_past_locative_derivation: nil})

    result = ResponseAttach.maybe_build_response_plan(%SemanticInput{
      source: :user, session_id: source.session_id,
      intent: :question, sentence: "Where was the bird?"
    }, finite_past_response_arbitration?: true)

    assert String.contains?(result.response_text, "cannot safely connect")
    assert result.response_meta.status == :continuity_unresolved
    assert result.response_meta.arbitration == :finite_past_report
    assert result.response_meta.response_source == :symbolic_finite_past_report
    refute result.response_meta.factual_authority?
    refute result.response_meta.retrieval_found?
    refute result.response_meta.llm_called?
  end

  test "response gate is opt-in and defers to the original planner once" do
    source = si("sat")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    si_like = %{source: :user, session_id: source.session_id,
      sentence: "Where was the bird?", confidence: 0.0}

    parent = self()
    fallback = fn ->
      send(parent, :old_planner)
      {:warm, "Original planner.", %{response_source: :legacy}}
    end

    assert {:warm, "Original planner.", _} =
      ResponseGate.route(si_like, [], fallback)
    assert_receive :old_planner
    refute_received :old_planner

    assert {:warm, "Original planner.", _} =
      ResponseGate.route(si_like,
        [finite_past_response_arbitration?: false], fallback)
    assert_receive :old_planner
    refute_received :old_planner

    assert {:warm, answer, %{status: :speaker_description_candidate}} =
      ResponseGate.route(si_like, [finite_past_response_arbitration?: true],
        fn -> flunk("qualified answer fell through to legacy planner") end)
    assert String.contains?(answer, "bird as being on a fence")
  end

  test "response gate does not infer permission from a stored report or other session" do
    source = si("slept")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    fallback = fn -> {:warm, "Original planner.", %{}} end

    for si <- [
          %{source: :assistant, session_id: source.session_id,
            sentence: "Where was the bird?"},
          %{source: :test, session_id: source.session_id,
            sentence: "Where was the bird?"},
          %{source: :user, session_id: "different",
            sentence: "Where was the bird?"},
          %{source: :user, session_id: source.session_id,
            sentence: "Where is the bird?"},
          %{source: :user, session_id: nil,
            sentence: "Where was the bird?"}
        ] do
      assert {:warm, "Original planner.", _} =
        ResponseGate.route(si, [finite_past_response_arbitration?: true],
          fallback)
    end
  end

  test "response attachment explicit off disables all reply paths even if opted in" do
    source = si("sat")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    question = %SemanticInput{source: :user, session_id: source.session_id,
      sentence: "Where was the bird?"}

    assert ResponseAttach.maybe_build_response_plan(question,
      response: :off, finite_past_response_arbitration?: true) == question
  end


  test "LiveView fast path skips only opted-in terminal finite-past arbitration" do
    source = si("sat")
    cleanup(source.session_id)
    Ledger.record(source, finite_past_discourse_ledger?: true,
      finite_past_lemma_lookup: &lookup/1)

    refute ResponseGate.skip_legacy_fast_path?(
      source.session_id, "Where was the bird?", false)
    assert ResponseGate.skip_legacy_fast_path?(
      source.session_id, "Where was the bird?", true)
    refute ResponseGate.skip_legacy_fast_path?(
      source.session_id, "Where is the bird?", true)
    refute ResponseGate.skip_legacy_fast_path?(
      "different-session", "Where was the bird?", true)
    refute ResponseGate.skip_legacy_fast_path?(
      nil, "Where was the bird?", true)

    Ledger.record(%{source | sentence: "An unrelated turn.",
                            finite_past_locative_derivation: nil})
    assert ResponseGate.skip_legacy_fast_path?(
      source.session_id, "Where was the bird?", true)
  end

end
