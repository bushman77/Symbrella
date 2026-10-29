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
