defmodule Core.Brain.ActivationProbeTest do
  use ExUnit.Case, async: true

  alias Core.Brain.Activation
  alias Core.Brain.ActivationProbe, as: Probe

  @bank "bank|noun|1"
  @river "bank|noun|10"

  defp candidate(id), do: %{id: id, token_index: 0, lemma: "bank", score: 0.5}

  test "isolated prior overrides a live snapshot only when explicitly requested" do
    si = %{
      active_cells: [candidate(@bank), candidate(@river)],
      neuron_activation: %{@bank => 0.9},
      trace: []
    }

    # The test-only counterfactual input does not stimulate runtime neurons.
    out = Activation.prime(si,
      notify_brain?: false,
      neuron_probe_snapshot: %{@river => 0.8}
    )

    assert out.neuron_activation == %{@river => 0.8}
    [bank, river] = out.active_cells
    refute Map.has_key?(bank, :activation)
    assert_in_delta river.activation, 0.82, 1.0e-6

    # Normal operation still uses STM's actual read-only snapshot.
    normal = Activation.prime(si, notify_brain?: false)
    [bank2, river2] = normal.active_cells
    assert_in_delta bank2.activation, 0.86, 1.0e-6
    refute Map.has_key?(river2, :activation)
  end

  test "synthetic recruitment and unresolved feedback obey the real half-life" do
    first = %{
      active_cells: [candidate(@bank), candidate(@river)],
      semantic_choices: [
        %{id: @bank, candidate_probs: %{@bank => 0.5, @river => 0.5}}
      ]
    }

    report = Probe.carryover(first, 0)
    assert_in_delta report[@bank], 0.025, 1.0e-9
    assert_in_delta report[@river], 0.01, 1.0e-9

    report_half = Probe.carryover(first, Brain.Cell.Activation.half_life_ms())
    assert_in_delta report_half[@bank], 0.0125, 1.0e-9
    assert_in_delta report_half[@river], 0.005, 1.0e-9
    assert Probe.carryover(first, 120_000) == %{}
  end

  test "comparing selected senses reports differences without scoring correctness" do
    baseline = %{
      senses: [
        %{token_index: 0, id: @bank},
        %{token_index: 1, id: "draw|verb|1"}
      ],
      comprehension_degraded?: true,
      projected_event_count: 0
    }

    neural = %{
      senses: [
        %{token_index: 0, id: @river},
        %{token_index: 1, id: "draw|verb|1"}
      ],
      comprehension_degraded?: false,
      projected_event_count: 1
    }

    assert %{
      changed_sense_selections: [
        %{token_index: 0, baseline_id: @bank, neural_id: @river}
      ],
      degraded_changed?: true,
      projected_event_count_delta: 1
    } = Probe.compare(baseline, neural)
  end

  test "an arbitrary answer must not be reconstructed from unsupported events" do
    question = "what did the artist draw?"

    # No replay from global owner-scoped memories or invented episodic frames.
    assert Probe.event_answer(%{episodic_candidates: []}, question) == nil

    # An injected event without independently verified source structure must
    # not be accepted merely because it contains familiar words.
    injected = %{
      source: :prod,
      sentence: "The artist draws a picture.",
      episodic_candidates: [
        %{entity: "the artist", entity_key: "artist", relation: "verb_draw",
          value: "a picture", status: :asserted, source_frame: :finite_present_svo}
      ]
    }

    assert Probe.event_answer(injected, question) == nil
  end

  test "summary retains uncertainty and exact winner IDs as evidence levels" do
    summary =
      Probe.summarize(%{
        intent: :unknown,
        confidence: 0.4,
        active_cells: [%{id: @river, runtime_primed?: true}],
        semantic_choices: [
          %{token_index: 0, id: @river, score: 0.15, pos: :noun, source: :active_cells}
        ],
        comprehension: %{
          degraded?: true,
          reasons: [:weak_decision_rate_high],
          lexical_meanings: [],
          uncertain: ["bank"]
        },
        sentence_meaning: %{status: :not_yet_interpreted},
        episodic_candidates: []
      })

    assert summary.active_prior_candidates == 1
    assert [%{id: @river, score: 0.15}] = summary.senses
    assert summary.comprehension_degraded?
    assert summary.verified_lexical_meanings == []
    assert summary.uncertain == ["bank"]
    assert summary.projected_event_count == 0
  end

  test "hippocampal diagnostic opt-out never encodes an episode" do
    si = %{atl_slate: %{winners: [%{id: @river}]}, sentence: "bank"}

    assert Core.Brain.Hippocampus.encode(si, hippocampus_encode?: false) == si
  end

  test "built-in scenarios include artist and bank-context switch" do
    assert [
      {"artist", "The artist draws a picture.", "what did the artist draw?"},
      {"bank_switch", _, _}
    ] = Probe.scenarios()
  end
end
