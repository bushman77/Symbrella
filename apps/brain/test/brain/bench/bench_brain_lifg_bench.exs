# Run with:
#   MIX_ENV=dev mix run bench/brain_lifg_bench.exs
#
# Ensure benchee is added:
#   {:benchee, "~> 1.3", only: :dev}

Mix.install([{:benchee, "~> 1.3"}])

alias Brain.LIFG
alias Brain.LIFG.Stage1

defmodule BenchUtil do
  def randf, do: :rand.uniform()
  def vec(dim), do: for(_ <- 1..dim, do: :rand.uniform() - 0.5)

  # Build a full SI (current API): tokens + sense_candidates + context_vec
  def si(num_tokens, senses_per_token, dim) do
    tokens =
      for t <- 0..(num_tokens - 1) do
        phrase = "lemma_#{t}"
        %{index: t, phrase: phrase, n: 1, span: {t * 3, String.length(phrase)}}
      end

    sense_candidates =
      for t <- 0..(num_tokens - 1), into: %{} do
        cands =
          for s <- 1..senses_per_token do
            %{
              id: "t#{t}|s#{s}",
              # feeds :rel_prior through candidates_from_slate/… → score mix
              prior: randf(),
              features: %{
                pos: "noun",
                lex_fit: randf(),
                activation: randf(),
                embedding: vec(dim)
              }
            }
          end

        {t, cands}
      end

    %{
      sentence: Enum.map(tokens, & &1.phrase) |> Enum.join(" "),
      tokens: tokens,
      sense_candidates: sense_candidates,
      context_vec: vec(dim)
    }
  end
end

scenarios = [
  {:g4_s3_d64, 4, 3, 64},
  {:g8_s3_d64, 8, 3, 64},
  {:g8_s6_d64, 8, 6, 64},
  {:g16_s4_d64, 16, 4, 64},
  {:g8_s3_d128, 8, 3, 128}
]

inputs =
  for {name, g, s, d} <- scenarios, into: %{} do
    {name, BenchUtil.si(g, s, d)}
  end

Benchee.run(
  %{
    # Stage1 (guarded, per-token scoring without context sim)
    "LIFG.Stage1.run/2" => fn si ->
      {:ok, _} = Stage1.run(si, scores: :none)
    end,

    # disambiguate_stage1 (full scoring incl. sim if embeddings/context present)
    "LIFG.disambiguate_stage1(scores: :none)" => fn si ->
      # compatibility: second arg is ignored; we pass si-only opts
      _si2 = LIFG.disambiguate_stage1(si, scores: :none)
    end,
    "LIFG.disambiguate_stage1(scores: :all)" => fn si ->
      _si2 = LIFG.disambiguate_stage1(si, scores: :all)
    end
  },
  inputs: inputs,
  time: 2.0,
  memory_time: 1.0,
  format: :markdown
)
