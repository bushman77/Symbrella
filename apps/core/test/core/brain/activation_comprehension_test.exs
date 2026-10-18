defmodule Core.Brain.ActivationComprehensionTest do
  use ExUnit.Case, async: true

  alias Core.Brain.Activation
  alias Core.Brain.STM

  @bank "bank|noun|1"
  @river "bank|noun|10"

  defp row(id, token_index \\ 0) do
    %{id: id, token_index: token_index, lemma: "bank", norm: "bank",
      pos: "noun", score: 0.5, source: :ltm}
  end

  test "STM keeps DB candidates separate from the firing neuron map" do
    input = %{sentence: "bank", active_cells: [row(@bank)], neuron_activation: %{}}
    brain_return = %{input | active_cells: %{@river => 0.3}}

    out = STM.attach_neuron_snapshot(input, brain_return)

    assert out.active_cells == [row(@bank)]
    assert out.neuron_activation == %{@river => 0.3}
    assert out.sentence == "bank"
  end

  test "live activation becomes a bounded prior only on its exact sense" do
    si = %{
      active_cells: [row(@bank), row(@river)],
      neuron_activation: %{@river => 0.5, "unrelated|noun|1" => 0.9},
      trace: []
    }

    result = Activation.prime(si, notify_brain?: false)

    [financial, riverbank] = result.active_cells
    assert financial.score == 0.5
    refute Map.has_key?(financial, :activation)
    assert riverbank.score == 0.5
    assert_in_delta riverbank.activation, 0.7, 0.0001
    assert riverbank.prior_neuron_activation == 0.5
    assert riverbank.runtime_primed?
    assert result.neuron_activation == si.neuron_activation

    assert Enum.any?(result.trace, fn
      %{stage: :neuron_priming, meta: %{previously_active: 1, recruited: 2}} -> true
      _ -> false
    end)
  end

  test "prebuilt sense slates receive the same exact-ID prior as LTM cells" do
    si = %{
      neuron_activation: %{@river => 0.7},
      sense_candidates: %{0 => [row(@bank), row(@river)]},
      candidates_by_token: %{0 => [row(@bank), row(@river)]}
    }

    result = Activation.overlay_candidate_slates(si)

    for key <- [:sense_candidates, :candidates_by_token] do
      [financial, riverbank] = get_in(result, [key, 0])
      refute Map.has_key?(financial, :activation)
      assert_in_delta riverbank.activation, 0.78, 0.0001
      assert riverbank.prior_neuron_activation == 0.7
    end
  end

  test "zero activation and candidate-owned activation are not overwritten" do
    cells = [row(@bank), Map.put(row(@river), :activation, 0.8)]

    result =
      Activation.prime(%{
        active_cells: cells,
        neuron_activation: %{@bank => 0.0, @river => 0.9},
        trace: []
      }, notify_brain?: false)

    [bank, river] = result.active_cells
    refute Map.has_key?(bank, :activation)
    assert river.activation == 0.8
  end

  test "initial recruitment is bounded and does not choose arbitrary meanings in large slates" do
    large_slate =
      for i <- 1..12 do
        row("draw|verb|#{i}", 0)
      end

    result = Activation.prime(%{active_cells: large_slate, trace: []},
      notify_brain?: false)

    assert length(result.active_cells) == 12
    assert hd(result.trace).meta.recruited == 0
    assert hd(result.trace).meta.skipped_ambiguous_slates?
  end

  test "a tied lexical winner is weakly activated without claiming verification" do
    si = %{
      active_cells: [row(@bank), row(@river)],
      semantic_choices: [
        %{id: @bank, candidate_probs: %{@bank => 0.5, @river => 0.5}}
      ]
    }

    assert [%{id: @bank, delta: 0.015, evidence: :unresolved_sense_selection}] =
             Activation.feedback_plan(si)

    # No artificial verification event is added by the activation stage.
    out = Activation.notify(Map.put(si, :trace, []), notify_brain?: false)
    assert [{:activated, %{verified: 0, unresolved: 1}}] = out.trace
  end

  test "distinguishable but unverified scores remain explicitly unverified" do
    si = %{
      active_cells: [row(@bank), row(@river)],
      semantic_choices: [
        %{id: @river, candidate_probs: %{@bank => 0.4, @river => 0.6}}
      ]
    }

    assert [%{id: @river, delta: 0.05, evidence: :ranked_not_verified}] =
             Activation.feedback_plan(si)
  end

  test "only independently verified lexical constructions earn strongest feedback" do
    si = %{
      active_cells: [row(@bank), row(@river)],
      semantic_choices: [%{id: @bank, candidate_probs: %{@bank => 0.5, @river => 0.5}}],
      comprehension: %{
        lexical_meanings: [%{id: @river, phrase: "river bank", definition: "edge of river"}]
      }
    }

    assert [
      %{id: @river, delta: 0.12, evidence: :verified_lexical_construction},
      %{id: @bank, delta: 0.015, evidence: :unresolved_sense_selection}
    ] = Activation.feedback_plan(si)
  end

  test "fallbacks, nonexistent candidate selections, and large-slate unknown margins are safe" do
    si = %{
      active_cells: [row(@bank)],
      semantic_choices: [
        %{id: "bank|noun|fallback", candidate_probs: nil},
        %{id: "unknown|noun|1", candidate_probs: nil},
        %{id: @bank, candidate_probs: nil}
      ],
      comprehension: %{lexical_meanings: []}
    }

    assert [%{id: @bank, delta: 0.015, evidence: :unresolved_sense_selection}] =
             Activation.feedback_plan(si)
  end
end
