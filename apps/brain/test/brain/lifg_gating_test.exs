defmodule Brain.LIFGGatingTest do
  use ExUnit.Case, async: false

  @strong_id "this|noun|0"

  setup do
    original =
      Application.get_env(:brain, :lifg_stage1_weights) ||
        %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}

    # For this test, make Stage-1 almost entirely activation-driven so that
    # a high :score winner clearly crosses lifg_min_score when normalized.
    Application.put_env(:brain, :lifg_stage1_weights, %{
      lex_fit: 0.0,
      rel_prior: 0.0,
      activation: 1.0,
      intent_bias: 0.0
    })

    on_exit(fn ->
      Application.put_env(:brain, :lifg_stage1_weights, original)
    end)

    :ok
  end

  test "lifg choices cross gate into WM with min_score" do
    si = %{
      tokens: [
        %{index: 0, phrase: "this"}
      ],
      sense_candidates: %{
        0 => [
          # Strong winner: high score
          %{
            id: @strong_id,
            norm: "this",
            pos: "noun",
            score: 0.9
          },
          # Weak alternative: low score
          %{
            id: "this|noun|1",
            norm: "this",
            pos: "noun",
            score: 0.1
          }
        ]
      }
    }

    # Keep the same call site; normalize with softmax so gate uses probabilities.
    {:ok, _} =
      Brain.lifg_stage1(
        si,
        # ctx arg (legacy/ignored by Stage1)
        [0.0],
        gate_into_wm: true,
        lifg_min_score: 0.6,
        normalize: :softmax,
        scores: :all
      )

    %{wm: wm} = Brain.snapshot_wm()

    assert Enum.any?(wm, &(&1.id == @strong_id and &1.source == :lifg))
  end
end

