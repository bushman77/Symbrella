defmodule Core do
  alias Core.{SemanticInput, Token}

  @spec resolve_input(String.t(), atom()) :: SemanticInput.t()
  def resolve_input(sentence, source \\ :console) when is_binary(sentence) do
    {:ok, tokens} = Token.tokenize(sentence, assume_all: true, max_n: 4)

    %SemanticInput{
      original_sentence: sentence,
      sentence: sentence,
      source: source,
      tokens: tokens
    }
  end
end
