# apps/brain/test/brain/lifg_guard_chargram_test.exs
defmodule Brain.LIFG.GuardChargramTest do
  use ExUnit.Case, async: false

  alias Brain.LIFG.Guard

  @moduledoc """
  Invariants for LIFG Guard around char-grams and boundary substrings.

  Goals:

    • Explicit char-grams (kind/source/chargram?) are dropped before LIFG.
    • Cross-word substrings (non-MWE) are treated as char-grams and dropped.
    • In-word non-boundary substrings are dropped as boundary violations.
    • Telemetry events are emitted for both char-gram and boundary drops.
  """

  # ───────────────────────────────────────────────────────────────────────────
  # Helpers
  # ───────────────────────────────────────────────────────────────────────────

  defp attach_telemetry(id, event, tag) do
    :telemetry.attach(
      id,
      event,
      fn ev, meas, meta, cfg_pid ->
        send(cfg_pid, {tag, ev, meas, meta})
      end,
      self()
    )

    on_exit(fn ->
      try do
        :telemetry.detach(id)
      rescue
        _ -> :ok
      end
    end)
  end

  # ───────────────────────────────────────────────────────────────────────────
  # Tests
  # ───────────────────────────────────────────────────────────────────────────

  test "drops explicit char-grams without sentence and emits chargram_violation" do
    handler_id = {:lifg_guard_chargram_explicit, make_ref()}

    attach_telemetry(
      handler_id,
      [:brain, :lifg, :chargram_violation],
      :chargram_violation
    )

    tokens = [
      %{phrase: "Ki", kind: :chargram},
      %{phrase: "Kick"}
    ]

    sanitized = Guard.sanitize(tokens)

    phrases = Enum.map(sanitized, & &1.phrase)
    assert phrases == ["Kick"]

    # We expect at least one chargram_violation for the explicit char-gram.
    assert_receive {:chargram_violation, [:brain, :lifg, :chargram_violation], _meas, meta},
                   100

    assert meta.reason == :chargram
    assert meta.mw == false
    assert meta.count == 1
  end

  test "drops cross-word char-grams but keeps MWE (mw: true) and emits violations" do
    sentence = "hello there"

    tokens = [
      # Legit MWE that spans the space
      %{phrase: "hello there", span: {0, 11}, mw: true},
      # Legit unigrams
      %{phrase: "hello", span: {0, 5}},
      %{phrase: "there", span: {6, 5}},
      # Cross-word substring (non-MWE); should be treated as char-gram
      %{phrase: "lo ther", span: {3, 7}}
    ]

    h1 = {:lifg_guard_chargram_crossword_prod, make_ref()}
    h2 = {:lifg_guard_chargram_crossword_test, make_ref()}

    attach_telemetry(
      h1,
      [:brain, :lifg, :chargram_violation],
      :chargram_violation
    )

    attach_telemetry(
      h2,
      [:test, :lifg, :chargram_violation_tripwire],
      :chargram_tripwire
    )

    slate = %{tokens: tokens, sentence: sentence}
    %{tokens: sanitized} = Guard.sanitize(slate)

    phrases = Enum.map(sanitized, & &1.phrase) |> Enum.sort()
    # Cross-word "lo ther" should be gone; others remain.
    assert phrases == Enum.sort(["hello there", "hello", "there"])

    # Char-gram violation (prod event)
    assert_receive {:chargram_violation, [:brain, :lifg, :chargram_violation], _meas, meta1},
                   100

    assert meta1.reason == :chargram

    # Tripwire event for tests
    assert_receive {:chargram_tripwire,
                    [:test, :lifg, :chargram_violation_tripwire],
                    _meas,
                    meta2},
                   100

    assert meta2.reason == :chargram
  end

  test "drops in-word non-boundary substrings as boundary_drop when sentence is present" do
    sentence = "what"

    tokens = [
      %{phrase: "what", span: {0, 4}},
      # In-word substring "hat" starting at index 1 (non-boundary)
      %{phrase: "hat", span: {1, 3}}
    ]

    handler_id = {:lifg_guard_boundary_drop, make_ref()}

    attach_telemetry(
      handler_id,
      [:brain, :lifg, :boundary_drop],
      :boundary_drop
    )

    slate = %{tokens: tokens, sentence: sentence}
    %{tokens: sanitized} = Guard.sanitize(slate)

    phrases = Enum.map(sanitized, & &1.phrase)
    # Only the full word should remain; the in-word substring is dropped.
    assert phrases == ["what"]

    assert_receive {:boundary_drop, [:brain, :lifg, :boundary_drop], _meas, meta}, 100

    # We at least know this was not marked as an MWE, so mw must be false.
    refute meta.mw
  end
end

