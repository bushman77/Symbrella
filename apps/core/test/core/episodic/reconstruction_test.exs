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
