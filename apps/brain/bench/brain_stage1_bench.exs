# Run with:
#   MIX_ENV=dev mix run apps/brain/bench/brain_stage1_bench.exs

defmodule BenchGen do
  def randf, do: :rand.uniform()

  def tokens(n) do
    for i <- 0..(n - 1) do
      %{index: i, phrase: "t#{i}", n: 1}
    end
  end

  # Build si.sense_candidates as %{token_index => [%{id, features: %{...}}]}
  def slate(num_tokens, senses_per_token) do
    Enum.into(0..(num_tokens - 1), %{}, fn t ->
      senses =
        for s <- 1..senses_per_token do
          %{
            id: "t#{t}|s#{s}",
            features: %{
              lex_fit: randf(),
              rel_prior: randf(),
              activation: randf() * 0.1, # tiny activation
              intent_bias: 0.0
            }
          }
        end

      {t, senses}
    end)
  end
end

# Make runs reproducible
:rand.seed(:exsplus, {101, 102, 103})

scenarios = [
  {:g4_s3, 4, 3},
  {:g8_s3, 8, 3},
  {:g8_s6, 8, 6},
  {:g16_s4, 16, 4}
]

inputs =
  Enum.into(scenarios, %{}, fn {name, g, s} ->
    si = %{tokens: BenchGen.tokens(g), sense_candidates: BenchGen.slate(g, s)}
    {name, si}
  end)

# Define your benchmark jobs (use the current Stage1 API)
jobs = %{
  "LIFG.Stage1.run" => fn si ->
    {:ok, _} = Brain.LIFG.Stage1.run(si)
  end
}

# Use Console; add Markdown if benchee_markdown is installed
formatters =
  if Code.ensure_loaded?(Benchee.Formatters.Markdown) do
    [
      {Benchee.Formatters.Console, []},
      {Benchee.Formatters.Markdown, file: "brain_stage1_bench.md"}
    ]
  else
    [{Benchee.Formatters.Console, []}]
  end

Benchee.run(
  jobs,
  inputs: inputs,
  time: 2.0,
  memory_time: 1.0,
  formatters: formatters
)

