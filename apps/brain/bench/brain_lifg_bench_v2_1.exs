
# bench/brain_lifg_bench_v2_1.exs
unless Code.ensure_loaded?(Benchee) do
  IO.puts(:stderr, "Benchee missing. Add {:benchee, \"~> 1.3\", only: :dev} and run mix deps.get")
  System.halt(1)
end

:rand.seed(:exsplus, {101, 202, 303})

defmodule BenchUtil do
  def randf, do: :rand.uniform()
  def vec(dim), do: for _ <- 1..dim, do: (:rand.uniform() - 0.5)
  def candidates(num_tokens, senses_per_token, dim) do
    for t <- 0..(num_tokens - 1),
        s <- 1..senses_per_token do
      %{
        id: "t#{t}|s#{s}",
        token_index: t,
        lemma: "lemma_#{t}",
        pos: "noun",
        embedding: vec(dim),
        lex_fit: randf(),
        rel_prior: randf(),
        intent_bias: randf(),
        activation: randf()
      }
    end
  end
  def context(dim), do: vec(dim)
end

scenarios = [
  {:g8_s3_d64, 8, 3, 64},
  {:g32_s6_d256, 32, 6, 256}
]

inputs =
  for {name, g, s, d} <- scenarios, into: %{} do
    {name, %{cands: BenchUtil.candidates(g, s, d), ctx: BenchUtil.context(d)}}
  end

Benchee.run(
  %{
    "v2.1 serial maxnorm (top2)" => fn %{cands: cands, ctx: ctx} ->
      {:ok, _} = Brain.LIFG.disambiguate_stage1(cands, ctx, scores: :top2, normalize: :maxnorm, parallel: false)
    end,
    "v2.1 auto maxnorm (top2)" => fn %{cands: cands, ctx: ctx} ->
      {:ok, _} = Brain.LIFG.disambiguate_stage1(cands, ctx, scores: :top2, normalize: :maxnorm, parallel: :auto)
    end
  },
  inputs: inputs,
  time: 3.0,
  memory_time: 1.0,
  warmup: 1.0
)
