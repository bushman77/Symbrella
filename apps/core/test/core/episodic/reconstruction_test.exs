defmodule Core.Episodic.ReconstructionTest do
  use ExUnit.Case, async: true
  alias Core.Episodic.Reconstruction

  test "legacy row produces an attributed review candidate but cannot auto-write" do
    row = %{id: 887, session_id: "s-legacy", sentence: "The project deadline is Friday.",
      role: nil, source: nil, inserted_at: ~N[2026-10-17 18:04:31], meta: %{}}

    preview = Reconstruction.preview_row(row)
    assert preview.attribution == :unattributed_legacy
    assert [%{entity_key: "project", relation: "deadline", value: "Friday"}] =
      preview.candidates
    assert preview.episode_id == 887
    assert preview.write_authorized? == false
    refute Reconstruction.production_write_allowed?(preview)
  end

  test "verified user-role row can be audited but never promoted automatically" do
    row = %{id: 12, session_id: "s-user", sentence: "Rowan lives in Victoria.",
      role: "user", source: "user", meta: %{}}
    assert %{attribution: :attributed_user, candidate_count: 1,
      write_authorized?: false} = Reconstruction.preview_row(row)
  end

  test "pairs legacy ordinary traces with already-structured events, without automatic promotion" do
    earlier = ~N[2026-10-17 19:19:56.347625]
    later = ~N[2026-10-17 19:19:56.367071]

    original = %{id: 891, role: "user", source: "user",
      sentence: nil, inserted_at: earlier, session_id: "s-a",
      meta: %{"kind" => "user_reported_event", "subject" => "user",
        "entity_key" => "project", "relation" => "deadline",
        "status" => "asserted", "value" => "Friday", "session_id" => "s-a"}}
    trace = %{id: 892, role: nil, source: nil, inserted_at: later,
      session_id: "s-a", sentence: "The project deadline is Friday.", meta: %{}}

    assert [_, preview] = Reconstruction.preview_batch([original, trace])
    assert preview.already_indexed?
    assert preview.matching_structured_episode_id == 891
    refute preview.write_authorized?

    # A different statement in the same session is not a duplicate.
    conflicting = %{trace | sentence: "The project deadline is Tuesday."}
    assert [_, unrelated] = Reconstruction.preview_batch([original, conflicting])
    refute unrelated.already_indexed?

    # The textual source check applies once the typed row includes its sentence.
    typed_new = %{original | sentence: "The project deadline is Friday."}
    assert [_, wrong_text] =
      Reconstruction.preview_batch([typed_new, %{trace | sentence: "Actually, the project deadline is Friday."}])
    refute wrong_text.already_indexed?
  end

  test "assistant, self-facts, questions and empty rows cannot become candidates" do
    for row <- [
      %{id: 3, role: "assistant", sentence: "Rowan lives in Victoria."},
      %{id: 4, role: "user", sentence: "What city does Rowan live in?"},
      %{id: 5, role: "user", sentence: "Rowan lives in Victoria.",
        meta: %{"kind" => "user_reported_event"}},
      %{id: 6, role: nil, sentence: ""}
    ] do
      assert %{candidate_count: 0, write_authorized?: false} =
        Reconstruction.preview_row(row)
    end
  end
end
