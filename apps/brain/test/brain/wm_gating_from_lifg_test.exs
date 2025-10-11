# apps/brain/test/brain/wm_gating_from_lifg_test.exs
defmodule Brain.WMGatingFromLIFGTest do
  use ExUnit.Case, async: false

  @moduledoc """
  Ensures Stage-1 winners (with sufficient score) are gated into WM
  and that no char-gram/boundary guards fire for normal inputs.
  """

  setup do
    # Telemetry listeners used in assertions
    :telemetry.attach_many("wm-gating-test-guards",
      [[:brain, :lifg, :chargram_violation], [:brain, :lifg, :boundary_drop]],
      fn e, m, meta, pid -> send(pid, {:guard, e, m, meta}) end,
      self()
    )

    :telemetry.attach("wm-gating-test-update",
      [:brain, :wm, :update],
      fn _e, m, meta, pid -> send(pid, {:wm, m, meta}) end,
      self()
    )

    :telemetry.attach("wm-gating-test-gate",
      [:brain, :gate, :decision],
      fn _e, m, meta, pid -> send(pid, {:gate, m, meta}) end,
      self()
    )

    on_exit(fn ->
      :telemetry.detach("wm-gating-test-update")
      :telemetry.detach("wm-gating-test-gate")
      :telemetry.detach("wm-gating-test-guards")
    end)

    :ok
  end

  test "stage-1 winners above threshold are admitted to WM and guards stay quiet" do
    # Minimal SI with sense candidates; mirrors your IEx repro
    si = %{
      sentence: "Hello there",
      tokens: [
        %{word: "Hello", start: 0, stop: 5,  kind: :word, pos: "intj"},
        %{word: "there", start: 6, stop: 11, kind: :word, pos: "adv"}
      ],
      sense_candidates: %{
        0 => [%{id: "Hello there|phrase|fallback", lemma: "Hello there", pos: "phrase", mw: true,  score: 1.0}],
        1 => [%{id: "hello|interjection|2",        lemma: "hello",       pos: "intj",             score: 0.50}],
        2 => [%{id: "there|noun|0",                lemma: "there",       pos: "noun",             score: 0.08}]
      }
    }

    {:ok, out} =
      Brain.lifg_stage1(si, [],
        scores: :all,
        gate_into_wm: true,
        lifg_min_score: 0.6
      )

    # Winners include phrase + token sense
    assert [%{chosen_id: id0}, %{chosen_id: id1}] = out.choices
    assert id0 == "Hello there|phrase|fallback"
    assert id1 == "hello|interjection|2"

    # Telemetry: at least 1 gate decision and a WM update with added > 0
    assert_receive {:gate, %{score: s1}, %{decision: dec1, source: :lifg}} when is_number(s1) and dec1 in [:allow, :boost]
    assert_receive {:wm, %{added: added, size: size}, %{reason: :gate_from_lifg}} when added > 0 and size >= added

    # No guard violations for this clean input
    refute_received {:guard, [:brain, :lifg, :chargram_violation], _m, _meta}
    refute_received {:guard, [:brain, :lifg, :boundary_drop], _m, _meta}

    # WM contains the winners
    %{wm: wm} = Brain.snapshot_wm()
    ids_in_wm = Enum.map(wm, & &1.id) |> MapSet.new()
    assert "Hello there|phrase|fallback" in ids_in_wm
    assert "hello|interjection|2" in ids_in_wm
  end
end

