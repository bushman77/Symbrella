# apps/brain/test/brain/lifg/lifg_gating_test.exs
defmodule Brain.LIFGGatingTest do
  use ExUnit.Case, async: false
  import Brain.TestHelpers

  @strong_id "this|noun|0"

  setup do
    original_state = Brain.snapshot()
    {:ok, clean_state} = Brain.init(:ok)
    :sys.replace_state(Process.whereis(Brain), fn _state -> clean_state end)

    original =
      Application.get_env(:brain, :lifg_stage1_weights) ||
        %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}

    # Make Stage-1 activation-driven to isolate score-based gating
    Application.put_env(:brain, :lifg_stage1_weights, %{
      lex_fit: 0.0,
      rel_prior: 0.0,
      activation: 1.0,
      intent_bias: 0.0
    })

    :ok = Brain.configure_wm(Map.to_list(Brain.Config.wm_defaults()))
    _ = Brain.defocus(fn _ -> true end)
    reset_stage1_mood()

    on_exit(fn ->
      Application.put_env(:brain, :lifg_stage1_weights, original)
      :sys.replace_state(Process.whereis(Brain), fn _state -> original_state end)
    end)

    :ok
  end

  defp reset_stage1_mood do
    case Process.whereis(Brain.LIFG.Stage1) do
      pid when is_pid(pid) ->
        :sys.replace_state(pid, fn state ->
          state
          |> Map.put(:mood, nil)
          |> Map.put(:mood_last_ms, nil)
        end)

      nil ->
        :ok
    end
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
    {:ok, out} =
      Brain.lifg_stage1(
        si,
        # ctx arg (legacy/ignored by Stage1)
        [0.0],
        gate_into_wm: true,
        lifg_min_score: 0.6,
        normalize: :softmax,
        scores: :all
      )

    assert [%{chosen_id: @strong_id, score: score}] = out.choices
    assert score >= 0.6
    assert [%{id: @strong_id, source: :lifg, score: lifg_score}] = out.si.lifg_choices
    assert lifg_score >= 0.6

    # 🧠 Wait for async WM write instead of snapshotting immediately
    item = assert_wm_item_exists(@strong_id, 150)

    # Verify full item shape
    assert item.source == :lifg
    assert item.score >= 0.6
    assert item.id == @strong_id

    # Optional: assert activation is reasonable
    assert item.activation >= 0.6
  end
end
