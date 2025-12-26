# apps/brain/test/brain/wm_gating_from_lifg_test.exs
defmodule Brain.WMGatingFromLIFGTest do
  use ExUnit.Case, async: false

  @moduledoc """
  Ensures Stage-1 winners (with sufficient score) are gated into WM
  and that no char-gram/boundary guards fire for normal inputs.
  """

  setup do
    # Telemetry listeners used in assertions (use remote captures to avoid perf warnings)
    :telemetry.attach_many(
      "wm-gating-test-guards",
      [
        # legacy listeners
        [:brain, :lifg, :chargram_violation],
        [:brain, :lifg, :boundary_drop],
        # stage1-namespaced listeners (newer)
        [:brain, :lifg, :stage1, :chargram_violation],
        [:brain, :lifg, :stage1, :boundary_drop]
      ],
      &__MODULE__.handle_guard/4,
      self()
    )

    :telemetry.attach(
      "wm-gating-test-update",
      [:brain, :wm, :update],
      &__MODULE__.handle_wm_update/4,
      self()
    )

    :telemetry.attach(
      "wm-gating-test-gate",
      [:brain, :gate, :decision],
      &__MODULE__.handle_gate/4,
      self()
    )

    on_exit(fn ->
      :telemetry.detach("wm-gating-test-update")
      :telemetry.detach("wm-gating-test-gate")
      :telemetry.detach("wm-gating-test-guards")
    end)

    :ok
  end

  # ───────────────────────── Telemetry handlers ─────────────────────────

  def handle_guard(event, measurements, meta, pid) do
    send(pid, {:guard, event, measurements, meta})
    :ok
  end

  def handle_wm_update(_event, measurements, meta, pid) do
    send(pid, {:wm, measurements, meta})
    :ok
  end

  def handle_gate(_event, measurements, meta, pid) do
    send(pid, {:gate, measurements, meta})
    :ok
  end

  # ───────────────────────── Tests ─────────────────────────

  test "stage-1 winners above threshold are admitted to WM and guards stay quiet" do
    # Ensure deterministic WM (avoid leftovers from other tests / prior runs)
    _ = Brain.defocus(fn _ -> true end)
    drain_mailbox()

    min_score = 0.6

    # Minimal SI with sense candidates; mirrors your IEx repro
    si = %{
      sentence: "Hello there",
      tokens: [
        %{word: "Hello", start: 0, stop: 5, kind: :word, pos: "intj"},
        %{word: "there", start: 6, stop: 11, kind: :word, pos: "adv"}
      ],
      sense_candidates: %{
        0 => [
          %{
            id: "Hello there|phrase|fallback",
            lemma: "Hello there",
            pos: "phrase",
            mw: true,
            score: 1.0
          }
        ],
        1 => [%{id: "hello|interjection|2", lemma: "hello", pos: "intj", score: 0.50}],
        2 => [%{id: "there|noun|0", lemma: "there", pos: "noun", score: 0.08}]
      }
    }

    {:ok, out} =
      Brain.lifg_stage1(si, [],
        scores: :all,
        gate_into_wm: true,
        lifg_min_score: min_score
      )

    # Stage1 still returns both winners (phrase + token sense)
    assert [%{chosen_id: id0}, %{chosen_id: id1}] = out.choices
    assert id0 == "Hello there|phrase|fallback"
    assert id1 == "hello|interjection|2"

    # Telemetry: at least 1 gate decision and a WM update with added > 0
    assert_receive {:gate, %{score: s1}, %{decision: dec1, source: :lifg}}
                   when is_number(s1) and dec1 in [:allow, :boost]

    assert_receive {:wm, %{added: added, size: size}, %{reason: :gate_from_lifg}}
                   when added > 0 and size >= added

    # No guard violations for this clean input (both legacy + namespaced)
    refute_received {:guard, [:brain, :lifg, :chargram_violation], _m, _meta}
    refute_received {:guard, [:brain, :lifg, :boundary_drop], _m, _meta}
    refute_received {:guard, [:brain, :lifg, :stage1, :chargram_violation], _m, _meta}
    refute_received {:guard, [:brain, :lifg, :stage1, :boundary_drop], _m, _meta}

    # WM contains every Stage1 winner that actually clears the gating threshold.
    expected_ids =
      out.choices
      |> List.wrap()
      |> Enum.filter(fn ch -> gate_score(ch) >= min_score end)
      |> Enum.map(&winner_id/1)

    assert expected_ids != []

    %{wm: wm} = Brain.snapshot_wm()
    ids_in_wm = Enum.map(wm, & &1.id) |> MapSet.new()

    Enum.each(expected_ids, fn id ->
      assert id in ids_in_wm
    end)
  end

  test "Stage2 enabled can reject low-confidence winners (synthetic Stage1 trace)" do
    _ = Brain.defocus(fn _ -> true end)
    drain_mailbox()

    # Construct an SI with a Stage1-like trace event where the phrase winner is low-confidence
    si = %{
      sentence: "Hello there",
      tokens: [
        %{index: 0, phrase: "Hello"},
        %{index: 1, phrase: "there"}
      ],
      trace: [
        %{
          stage: :lifg_stage1,
          choices: [
            %{
              token_index: 0,
              chosen_id: "Hello there|phrase|fallback",
              winner_id: "Hello there|phrase|fallback",
              # omit :score intentionally so Stage2 uses p_top1
              p_top1: 0.12,
              margin: 0.02,
              probs: %{
                "Hello there|phrase|fallback" => 0.12,
                "hello|interjection|2" => 0.11
              }
            },
            %{
              token_index: 1,
              chosen_id: "hello|interjection|2",
              winner_id: "hello|interjection|2",
              p_top1: 1.0,
              margin: 1.0,
              probs: %{"hello|interjection|2" => 1.0}
            }
          ]
        }
      ]
    }

    :ok =
      Brain.gate_from_lifg(si,
        lifg_stage2_enabled: true,
        lifg_min_score: 0.6
      )

    # Stage2-driven gate emits WM update reason :lifg_stage2
    assert_receive {:wm, %{added: added, size: size}, %{reason: :lifg_stage2}}
                   when is_number(added) and added >= 1 and is_number(size) and size >= added

    # Gate decision event should reflect the admitted commit(s)
    assert_receive {:gate, %{score: s}, %{id: "hello|interjection|2", decision: d, source: :lifg}}
                   when is_number(s) and d in [:allow, :boost]

    %{wm: wm} = Brain.snapshot_wm()
    ids_in_wm = Enum.map(wm, & &1.id) |> MapSet.new()

    assert "hello|interjection|2" in ids_in_wm
    refute "Hello there|phrase|fallback" in ids_in_wm
  end

  # ───────────────────────── Helpers ─────────────────────────

  defp drain_mailbox do
    receive do
      _msg -> drain_mailbox()
    after
      0 -> :ok
    end
  end

  defp winner_id(%{} = ch) do
    raw =
      Map.get(ch, :chosen_id) || Map.get(ch, "chosen_id") ||
        Map.get(ch, :winner_id) || Map.get(ch, "winner_id") ||
        Map.get(ch, :id) || Map.get(ch, "id")

    case raw do
      v when is_binary(v) -> v
      v when is_atom(v) -> Atom.to_string(v)
      _ -> ""
    end
  end

  # Approximate the gating score used by the pipeline/Stage2:
  # prefer p_top1/prob if present; otherwise fall back to score; clamp to 0..1.
  defp gate_score(%{} = ch) do
    s = to_float01(Map.get(ch, :score) || Map.get(ch, "score"))

    p =
      to_float01(
        Map.get(ch, :p_top1) || Map.get(ch, "p_top1") || Map.get(ch, :prob) || Map.get(ch, "prob")
      )

    max(s, p)
  end

  defp gate_score(_), do: 0.0

  defp to_float01(v) when is_float(v), do: v |> max(0.0) |> min(1.0)
  defp to_float01(v) when is_integer(v), do: (v * 1.0) |> max(0.0) |> min(1.0)

  defp to_float01(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {f, _} -> f |> max(0.0) |> min(1.0)
      _ -> 0.0
    end
  end

  defp to_float01(_), do: 0.0
end
