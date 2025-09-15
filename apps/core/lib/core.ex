defmodule Core do
  @moduledoc "Sentence → SemanticInput (minimal, Brain-agnostic)."

  alias Core.{SemanticInput, Token, TokenFilters}
  alias Core.Recall.{Gate, Execute}

  @spec resolve_input(String.t()) :: SemanticInput.t()
  def resolve_input(sentence) when is_binary(sentence) do
    sentence
    |> Token.tokenize()                 # builds %SemanticInput{tokens: [...]}
    |> TokenFilters.prune(min_n: 1)     # keep singletons while debugging
    |> Core.Lexicon.Stage.run()         # your DB/lexicon enrichment
    |> gate_and_optionally_execute()
  end

  defp gate_and_optionally_execute(si) do
    case Core.Recall.Gate.gate(si, confidence: 0.5, requires_knowledge?: false, oov_terms: []) do
      {:skip, si2}       -> si2
      {:plan, plan, si2} -> Execute.execute(si2, plan)
    end
  end
end

