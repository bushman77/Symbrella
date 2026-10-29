defmodule Core.Semantic.ClarifiedEventTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Discourse.Ledger.Owner
  alias Core.Semantic.ClarifiedEvent

  setup do
    if not is_pid(Process.whereis(Owner)), do: start_supervised!(Owner)
    sid = "clarified-event-#{System.unique_integer([:positive])}"
    on_exit(fn -> Ledger.clear(sid) end)
    %{sid: sid}
  end

  defp records(sentence \\ "The bird is flarping on the fence.") do
    words = sentence |> String.trim_trailing(".") |> String.split(" ")

    request = %{
      kind: :fuzzy_clarification_request,
      status: :provisional,
      actor: "bird",
      original: "flarping",
      proposed: "flapping",
      source_tokens: words,
      source_turn_sha256: sha(sentence),
      verification: :conversation_context_only,
      factual_authority?: false
    }

    resolution = %{
      kind: :fuzzy_clarification_resolution,
      status: :speaker_confirmed_hypothesis,
      actor: "bird",
      original: "flarping",
      proposed: "flapping",
      source_turn_sha256: sha(sentence),
      verification: :conversation_context_only,
      factual_authority?: false
    }

    {request, resolution}
  end

  defp sha(sentence) do
    :crypto.hash(:sha256, sentence) |> Base.encode16(case: :lower)
  end

  defp record_pair(sid, request, resolution) do
    {:ok, seq} = Owner.append(sid, request)
    {:ok, _} = Owner.append(sid, Map.put(resolution, :request_seq, seq))
    :ok
  end

  test "recomposes an attributed activity frame and answers its requested role", %{sid: sid} do
    {request, resolution} = records()
    record_pair(sid, request, resolution)

    assert {:ok, frame} = ClarifiedEvent.build(sid, "What is the bird doing?")
    assert frame.actor.surface == "bird"
    assert frame.predicate.surface == "flapping"
    assert frame.predicate.original_surface == "flarping"
    assert frame.spatial == %{
      relation: "on", location: "the fence", event_location_verified?: false
    }
    assert frame.evidence.source_clause_replayed? == true
    assert frame.evidence.question_role == :predicate
    assert frame.factual_authority? == false
    assert frame.predicate.exact_dictionary_sense? == false

    assert {:ok, "In your description, the bird is flapping.", meta} =
      ClarifiedEvent.answer(sid, "What is the bird doing?")
    assert meta.semantic_frame == frame
    assert meta.llm_called? == false

    assert :not_applicable == ClarifiedEvent.build(sid, "Where is the bird?")
    assert :not_applicable == ClarifiedEvent.build(sid, "What is the cat doing?")
    assert :not_applicable == ClarifiedEvent.build(sid <> "-other", "What is the bird doing?")
  end

  test "replayed pre-LIFG syntax gap enriches provenance but not factual authority", %{sid: sid} do
    alias Core.SemanticInput
    alias Core.Syntax.Signatures
    alias Core.TokenFilters

    sentence = "The fox is flarping on the fence."
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "fox", pos: "noun"},
      %{norm: "is", pos: "verb"},
      %{norm: "on", pos: "preposition"},
      %{norm: "fence", pos: "noun"}
    ]

    si = %SemanticInput{sentence: sentence, active_cells: cells}
      |> TokenFilters.rebuild_word_ngrams(3)

    assert [gap] =
      Enum.filter(Signatures.detect(si),
        &(&1.id == :progressive_lexical_gap_candidate))

    original = %{
      kind: :fuzzy_clarification_request, status: :provisional,
      actor: "fox", original: "flarping", proposed: "flapping",
      source_tokens: ~w(The fox is flarping on the fence),
      source_turn_sha256: sha(sentence),
      source_syntax: gap,
      verification: :conversation_context_only,
      factual_authority?: false
    }
    resolved = %{
      kind: :fuzzy_clarification_resolution,
      status: :speaker_confirmed_hypothesis, actor: "fox",
      original: "flarping", proposed: "flapping",
      source_turn_sha256: sha(sentence),
      verification: :conversation_context_only,
      factual_authority?: false
    }

    record_pair(sid, original, resolved)
    assert {:ok, frame} =
      ClarifiedEvent.build(sid, "What is the fox doing?")

    assert frame.actor.surface == "fox"
    assert frame.predicate.surface == "flapping"
    assert frame.evidence.role_binding == :attested_core_syntax_gap
    assert frame.evidence.pre_lifg_gap_observed? == true
    assert frame.evidence.event_occurrence_verified? == false
    assert frame.predicate.exact_dictionary_sense? == false
    assert frame.factual_authority? == false

    assert {:ok, "In your description, the fox is flapping.", _} =
      ClarifiedEvent.answer(sid, "What is the fox doing?")
  end

  test "original provisional semantics binds every role before speaker correction", %{sid: sid} do
    alias Core.SemanticInput
    alias Core.TokenFilters
    alias Core.Syntax.Signatures

    sentence = "The bird is flarping on the fence."
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "bird", pos: "noun"},
      %{norm: "is", pos: "verb"},
      %{norm: "on", pos: "preposition"},
      %{norm: "fence", pos: "noun"}
    ]

    si =
      %SemanticInput{sentence: sentence, active_cells: cells}
      |> TokenFilters.rebuild_word_ngrams(3)

    assert [syntax] =
      Enum.filter(Signatures.detect(si),
        &(&1.id == :progressive_lexical_gap_candidate))

    witness = %{
      kind: :unresolved_progressive_predicate,
      operation: :clarify_unresolved_predicate,
      source_span_sha256:
        (:crypto.hash(:sha256, "the bird is flarping on the fence")
         |> Base.encode16(case: :lower)),
      actor: "bird", predicate: "flarping",
      position: "on", location: "the fence",
      speaker_clarification_confirmed?: false,
      event_occurrence_verified?: false,
      factual_authority?: false
    }

    {request, resolution} = records()
    request = Map.merge(request, %{
      source_syntax: syntax, source_provisional: witness
    })
    record_pair(sid, request, resolution)

    assert {:ok, frame} = ClarifiedEvent.build(sid, "What is the bird doing?")
    assert frame.predicate.surface == "flapping"
    assert frame.original_interpretation.kind == :unresolved_progressive_predicate
    assert frame.original_interpretation.status == :provisional
    assert frame.evidence.role_binding == :validated_original_provisional_frame
    assert frame.evidence.original_semantic_frame_replayed? == true
    assert frame.evidence.event_occurrence_verified? == false
    assert frame.factual_authority? == false

    assert {:ok, "In your description, the bird is flapping.", _} =
      ClarifiedEvent.answer(sid, "What is the bird doing?")

    for corrupted <- [
      %{witness | actor: "cat"},
      %{witness | predicate: "dancing"},
      %{witness | position: "under"},
      %{witness | location: "the branch"},
      %{witness | source_span_sha256: String.duplicate("0", 64)},
      %{witness | event_occurrence_verified?: true}
    ] do
      isolated = sid <> "-" <> Integer.to_string(:erlang.phash2(corrupted))
      record_pair(isolated, %{request | source_provisional: corrupted}, resolution)
      assert :not_applicable ==
        Ledger.confirmed_fuzzy_activity(isolated, "What is the bird doing?")
      assert :not_applicable ==
        ClarifiedEvent.build(isolated, "What is the bird doing?")
      Ledger.clear(isolated)
    end

    Ledger.clear(sid)
    altered_signature = %{syntax | source_span_sha256: String.duplicate("0", 64)}
    record_pair(sid, %{request | source_syntax: altered_signature}, resolution)
    assert :not_applicable ==
      ClarifiedEvent.build(sid, "What is the bird doing?")
  end

  test "rejects altered source tokens despite matching actor and correction", %{sid: sid} do
    {request, resolution} = records()
    tampered = %{request | source_tokens: ~w(The bird is flarping under the fence)}
    record_pair(sid, tampered, resolution)
    assert :not_applicable == ClarifiedEvent.build(sid, "What is the bird doing?")
  end

  test "rejects an otherwise plausible request with a mismatched fingerprint", %{sid: sid} do
    {request, resolution} = records()
    record_pair(sid, %{request | source_turn_sha256: sha("other")},
      %{resolution | source_turn_sha256: sha("other")})
    assert :not_applicable == ClarifiedEvent.build(sid, "What is the bird doing?")
  end

  test "rejects a broken request-resolution link", %{sid: sid} do
    {request, resolution} = records()
    {:ok, _} = Owner.append(sid, request)
    {:ok, _} = Owner.append(sid, Map.put(resolution, :request_seq, 999_999))
    assert :not_applicable == ClarifiedEvent.build(sid, "What is the bird doing?")
  end
end
