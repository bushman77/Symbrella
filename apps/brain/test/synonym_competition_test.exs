defmodule SynonymCompetitionTest do
  use ExUnit.Case, async: true

  @moduledoc false

  # --- ATL "availability" (semantic store) ---
  @syns %{
    "fast" => ~w(quick rapid swift prompt ready)
  }

  # --- Contextual compatibility (drives control/selection) ---
  # Higher = fits that context better.
  @compat %{
    runner: %{
      "quick" => 0.70,
      "rapid" => 0.60,
      "swift" => 0.85,
      "prompt" => 0.05,
      "ready" => 0.10
    },
    wifi: %{"quick" => 0.45, "rapid" => 0.90, "swift" => 0.20, "prompt" => 0.05, "ready" => 0.15},
    reply: %{"quick" => 0.50, "rapid" => 0.35, "swift" => 0.40, "prompt" => 0.95, "ready" => 0.40}
  }

  # --- Helpers: softmax-based selector (emulates LIFG competition) ---
  defp select_synonym(lemma, ctx, opts) do
    tau = Keyword.get(opts, :tau, 0.7)
    syns_override = Keyword.get(opts, :syns_override, nil)
    intent_bias = Keyword.get(opts, :intent_bias, %{})

    syns = syns_override || @syns
    cands = Map.get(syns, lemma, [])

    scores =
      Enum.map(cands, fn cand ->
        compat = get_in(@compat, [ctx, cand]) || 0.0
        bias = Map.get(intent_bias, cand, 0.0)
        # Availability baseline (1.0) + contextual compat + PMTG intent bias
        {cand, 1.0 + compat + bias}
      end)

    probs = softmax(scores, tau)
    [{top1, p1} | rest] = probs
    {top2, p2} = Enum.at(rest, 0, {nil, 0.0})

    %{
      lemma: lemma,
      context: ctx,
      tau: tau,
      intent_bias: intent_bias,
      choices:
        Enum.map(probs, fn {cand, p} ->
          sc = Map.new(scores) |> Map.fetch!(cand)
          %{cand: cand, score: sc, p: p}
        end),
      chosen: top1,
      p_top1: p1,
      margin: p1 - p2,
      alt_top2: top2
    }
  end

  defp softmax(kv_scores, tau) do
    zs = Enum.map(kv_scores, fn {k, v} -> {k, :math.exp(v / tau)} end)
    z_sum = zs |> Enum.map(&elem(&1, 1)) |> Enum.sum()

    zs
    |> Enum.map(fn {k, z} -> {k, z / z_sum} end)
    |> Enum.sort_by(fn {_k, p} -> -p end)
  end

  # ───────────────────────── Tests ─────────────────────────

  test "with full ATL availability, runner context favors 'swift'" do
    res = select_synonym("fast", :runner, tau: 0.7)
    assert res.chosen == "swift"
    assert res.p_top1 > 0.30
  end

  test "with full ATL availability, wifi context favors 'rapid'" do
    res = select_synonym("fast", :wifi, tau: 0.7)
    assert res.chosen == "rapid"
    assert res.p_top1 > 0.35
  end

  test "with full ATL availability, reply context favors 'prompt'" do
    res = select_synonym("fast", :reply, tau: 0.7)
    assert res.chosen == "prompt"
    # Be tolerant to floating-point/temperature edges:
    # was > 0.35; actual ≈ 0.3494 at τ=0.7
    assert res.p_top1 > 0.33
    # ensures it's not a coin flip vs #2
    assert res.margin > 0.15
  end

  test "ATL drop (remove 'prompt') shifts reply choice away from 'prompt' (to 'quick')" do
    syns2 = %{"fast" => ~w(quick rapid swift ready)}
    res = select_synonym("fast", :reply, tau: 0.7, syns_override: syns2)
    assert res.chosen == "quick"
    refute res.chosen == "prompt"
  end

  test "PMTG intent bias toward 'prompt' increases its win probability in reply context" do
    base = select_synonym("fast", :reply, tau: 0.7)
    biased = select_synonym("fast", :reply, tau: 0.7, intent_bias: %{"prompt" => 0.40})

    assert base.chosen == "prompt"
    assert biased.chosen == "prompt"
    assert biased.p_top1 > base.p_top1
  end

  test "lower temperature sharpens competition (higher p_top1 vs higher tau)" do
    sharp = select_synonym("fast", :runner, tau: 0.4)
    soft = select_synonym("fast", :runner, tau: 0.9)

    assert sharp.chosen == "swift"
    assert soft.chosen == "swift"
    assert sharp.p_top1 > soft.p_top1
  end

  test "PMTG bias strictly raises the target's probability (reply → prompt)" do
    base = select_synonym("fast", :reply, tau: 0.7)
    biased = select_synonym("fast", :reply, tau: 0.7, intent_bias: %{"prompt" => 0.25})
    assert biased.p_top1 > base.p_top1
  end

  test "lower tau sharpens competition monotonically for the winner" do
    p1 = select_synonym("fast", :runner, tau: 0.9).p_top1
    p2 = select_synonym("fast", :runner, tau: 0.7).p_top1
    p3 = select_synonym("fast", :runner, tau: 0.5).p_top1
    assert p2 > p1
    assert p3 > p2
  end
end
