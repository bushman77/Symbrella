defmodule BrainLIFGIntegrationTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG
  alias Core.SemanticInput

  # NEW: run LIFG via the SI-based API and return the trace event
  defp lifg_event(cands, ctx, lifg_opts) do
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

    si2 = LIFG.disambiguate_stage1(si, lifg_opts)
    # LIFG pushes an audit/event onto si.trace (head-first)
    case si2.trace do
      [ev | _] -> ev
      _ -> %{}
    end
  end

  # ... your existing tests ...
# --- local helpers for this spec ---

defp choice_for(out, ti),
  do: Enum.find(out.choices, &(&1.token_index == ti)) || %{}

defp apply_feedback(cands, ev) do
  boosts_by = ev.boosts |> Enum.group_by(fn b -> {b.token_index, b.id} end, & &1.amount)
  inhib_by  = ev.inhibitions |> Enum.group_by(fn i -> {i.token_index, i.id} end, & &1.amount)

  Enum.map(cands, fn c ->
    key = {c.token_index, c.id}
    bsum = boosts_by |> Map.get(key, []) |> Enum.sum()
    isum = inhib_by  |> Map.get(key, []) |> Enum.sum()

    f = Map.get(c, :features, %{})
    act0 = (f[:activation] || f["activation"] || 0.0) * 1.0
    act1 = act0 + bsum - isum
    act1 = max(0.0, min(1.0, act1))

    Map.put(c, :features, Map.put(f, :activation, act1))
  end)
end

test "winners stabilize in Top-K; losers drift with decay + inhibitions" do
  # Token 0: clear winner (margin > threshold) -> expect BOOST
  # Token 1: tight race (margin < threshold)   -> expect INHIBITIONS
  cands = [
    # token 0 — clear top
    %{token_index: 0, id: "a0", features: %{lex_fit: 0.95, rel_prior: 0.60, activation: 0.20, intent_bias: 0.0}},
    %{token_index: 0, id: "b0", features: %{lex_fit: 0.80, rel_prior: 0.60, activation: 0.20, intent_bias: 0.0}},
    %{token_index: 0, id: "c0", features: %{lex_fit: 0.70, rel_prior: 0.60, activation: 0.20, intent_bias: 0.0}},
    # token 1 — very close top vs runner (will trigger inhibitions)
    %{token_index: 1, id: "a1", features: %{lex_fit: 0.86, rel_prior: 0.55, activation: 0.20, intent_bias: 0.0}},
    %{token_index: 1, id: "b1", features: %{lex_fit: 0.84, rel_prior: 0.55, activation: 0.20, intent_bias: 0.0}},
    %{token_index: 1, id: "c1", features: %{lex_fit: 0.60, rel_prior: 0.55, activation: 0.20, intent_bias: 0.0}}
  ]

  ctx  = [1.0, 0.0, 0.0]
  opts = [margin_threshold: 0.05, normalize: :softmax, scores: :none]

  # Round 1
  out1 = lifg_event(cands, ctx, opts)
  ch1_0 = choice_for(out1, 0)
  ch1_1 = choice_for(out1, 1)

  assert ch1_0.chosen_id == "a0"
  assert ch1_1.chosen_id == "a1"

  # Expect a BOOST on token 0 (clear margin) and some INHIBITIONS on token 1 (tight margin)
  assert Enum.any?(out1.boosts,       &(&1.token_index == 0 and &1.id == "a0"))
  assert Enum.any?(out1.inhibitions,  &(&1.token_index == 1))

  # Apply feedback (boosts/inhibitions) to candidate activations
  cands2 = apply_feedback(cands, out1)

  # Round 2
  out2 = lifg_event(cands2, ctx, opts)
  ch2_0 = choice_for(out2, 0)
  ch2_1 = choice_for(out2, 1)

  # Winners stabilize — chosen ids remain the same
  assert ch2_0.chosen_id == ch1_0.chosen_id
  assert ch2_1.chosen_id == ch1_1.chosen_id

  # Losers drift — margin on the tight token increases after inhibitions
  assert ch2_1.margin > ch1_1.margin

  # Apply feedback again to reinforce the trend
  cands3 = apply_feedback(cands2, out2)
  out3   = lifg_event(cands3, ctx, opts)
  ch3_1  = choice_for(out3, 1)

  # Margin should keep widening or at least not shrink (stabilization/drift)
  assert ch3_1.margin >= ch2_1.margin
end

end
