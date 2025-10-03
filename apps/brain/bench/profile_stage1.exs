# MIX_ENV=dev mix run apps/brain/bench/hotspots_stage1_bench.exs
#Mix.install([{:benchee, "~> 1.4"}])

defmodule Hot do
  def randf, do: :rand.uniform()

  def cands(k) do
    for s <- 1..k do
      %{
        id: "s#{s}",
        features: %{
          lex_fit: randf(), rel_prior: randf(),
          activation: randf(), intent_bias: randf()
        }
      }
    end
  end

  def weights, do: %{lex_fit: 0.40, rel_prior: 0.30, activation: 0.20, intent_bias: 0.10}

  def score(%{features: f}, w \\ weights()) do
    w.lex_fit * (f.lex_fit || 0.0) +
    w.rel_prior * (f.rel_prior || 0.0) +
    w.activation * (f.activation || 0.0) +
    w.intent_bias * (f.intent_bias || 0.0)
  end

  # Baseline: sort to get top-2
  def top2_sort(cands) do
    cands
    |> Enum.map(fn c -> %{id: c.id, score: score(c)} end)
    |> Enum.sort_by(&(-&1.score))
    |> Enum.take(2)
  end

  # Optimized: single pass to get top-2 (no sort)
  def top2_scan(cands) do
    {b1, b2} =
      Enum.reduce(cands, {nil, nil}, fn c, {best, second} ->
        s = score(c)
        cond do
          best == nil or s > best.score -> {%{id: c.id, score: s}, best}
          second == nil or s > second.score -> {best, %{id: c.id, score: s}}
          true -> {best, second}
        end
      end)

    [b1, b2]
  end
end

sizes = [3, 4, 6, 8, 12]
inputs = for k <- sizes, into: %{}, do: {:"senses_#{k}", Hot.cands(k)}

Benchee.run(
  %{
    "top2_sort_by"  => fn c -> Hot.top2_sort(c) end,
    "top2_one_pass" => fn c -> Hot.top2_scan(c) end
  },
  inputs: inputs,
  warmup: 1,
  time: 2,
  memory_time: 1
)

