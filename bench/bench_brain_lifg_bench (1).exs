
# How to run:
#   MIX_ENV=dev mix run bench/brain_lifg_bench.exs
#
# Add to deps (apps/brain/mix.exs or umbrella root mix.exs):
#   {:benchee, "~> 1.3", only: :dev}
# Then: mix deps.get

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
  {:g4_s3_d64,  4,  3,  64},
  {:g8_s3_d64,  8,  3,  64},
  {:g8_s6_d64,  8,  6,  64},
  {:g16_s4_d64, 16,  4,  64},
  {:g8_s3_d128, 8,  3, 128}
]

inputs =
  for {name, g, s, d} <- scenarios, into: %{} do
    {name, %{cands: BenchUtil.candidates(g, s, d), ctx: BenchUtil.context(d)}}
  end

Benchee.run(
  %{
    "LIFG.disambiguate_stage1 (softmax)" => fn %{cands: cands, ctx: ctx} ->
      {:ok, _} = Brain.LIFG.disambiguate_stage1(cands, ctx)
    end,
    "LIFG.disambiguate_stage1 (maxnorm)" => fn %{cands: cands, ctx: ctx} ->
      {:ok, _} = Brain.LIFG.disambiguate_stage1(cands, ctx, normalize: :maxnorm)
    end
  },
  inputs: inputs,
  time: 2.0,
  memory_time: 1.0
)
