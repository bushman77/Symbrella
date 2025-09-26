
# bench/brain_lifg_bench.exs
# Run with:
#   MIX_ENV=dev mix run bench/brain_lifg_bench.exs
#
# Ensure benchee is added in apps/brain/mix.exs (or umbrella root app):
#   {:benchee, "~> 1.3", only: :dev}
#
# This benchmark measures Stage‑1 disambiguation throughput across different
# token counts (groups), senses per token, and embedding dimensions.
#
# NOTE: Keep sizes modest so it runs quickly on low‑power devices.

Mix.install([{:benchee, "~> 1.3"}])

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
  memory_time: 1.0,
  format: :markdown
)
