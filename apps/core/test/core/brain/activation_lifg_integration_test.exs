defmodule Core.Brain.ActivationLifgIntegrationTest do
  use ExUnit.Case, async: false

  alias Core.Brain.Activation

  test "a prior neuron report for one exact sense reaches Stage1 and can break a tie" do
    candidates = [
      %{
        id: "bank|noun|1",
        token_index: 0,
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        definition: "A financial institution.",
        score: 0.5,
        source: :ltm
      },
      %{
        id: "bank|noun|10",
        token_index: 0,
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        definition: "The edge of a river.",
        score: 0.5,
        source: :ltm
      }
    ]

    si =
      %{
        sentence: "bank",
        tokens: [%{index: 0, phrase: "bank", n: 1, mw: false, span: {0, 4}}],
        active_cells: candidates,
        neuron_activation: %{"bank|noun|10" => 0.8},
        sense_candidates: %{},
        trace: []
      }
      |> Activation.prime(notify_brain?: false)

    assert {:ok, %{choices: [%{scores: scores} = choice]}} =
             Brain.LIFG.Stage1.run(si,
               preserve_sense_candidates: true,
               scores: :all,
               weights: %{lex_fit: 0.0, context_fit: 0.0, rel_prior: 0.0,
                          activation: 1.0, intent_bias: 0.0}
             )

    assert scores["bank|noun|10"] > scores["bank|noun|1"]
    assert choice.chosen_id == "bank|noun|10"

    # A neural prior affecting an LIFG decision is still not an independently
    # verified meaning, so a later comprehension is free to revise it.
    assert si.comprehension == nil
  end
end
