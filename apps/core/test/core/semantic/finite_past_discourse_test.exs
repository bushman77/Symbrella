defmodule Core.Semantic.FinitePastDiscourseTest do
  use ExUnit.Case, async: true

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
      session_id: "phase3-observation-only",
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
end
