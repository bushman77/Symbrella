defmodule BrainLIFGIntegrationTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG
  alias Core.SemanticInput

  # NEW: run LIFG via the SI-based API and return the trace event
  defp lifg_event(cands, ctx, lifg_opts \\ []) do
    token_idxs =
      cands
      |> Enum.map(& &1.token_index)
      |> Enum.uniq()
      |> Enum.sort()

    tokens = Enum.map(token_idxs, fn i -> %{index: i, phrase: "t#{i}"} end)

    si =
      struct(SemanticInput, %{
        tokens: tokens,
        active_cells: cands,
        context_vec: ctx,
        lifg_opts: lifg_opts
      })

    si2 = LIFG.disambiguate_stage1(si)
    # LIFG pushes an audit/event onto si.trace (head-first)
    case si2.trace do
      [ev | _] -> ev
      _ -> %{}
    end
  end

  # ... your existing tests ...

  test "winners stabilize in Top-K; losers drift with decay + inhibitions" do
    # (your existing cands/ctx setup stays the same)
    cands = [
      # ...
    ]

    ctx = [1.0, 0.0, 0.0]

    # OLD (remove this):
    # {:ok, out} =
    #   LIFG.disambiguate_stage1(cands, ctx,
    #     margin_threshold: 0.05, normalize: :softmax, scores: :none
    #   )

    # NEW:
    out =
      lifg_event(cands, ctx,
        margin_threshold: 0.05,
        normalize: :softmax,
        scores: :none
      )

    # out now has :choices, :boosts, :inhibitions — same fields you used before
    # ...rest of your assertions & loop logic unchanged...
  end
end
