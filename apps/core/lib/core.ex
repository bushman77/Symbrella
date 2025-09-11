defmodule Core do
  alias Core.{SemanticInput, Tokenizer, POSEngine, IntentClassifier, ResponsePlanner}
  alias MoodCore
  alias Core.{
    Token
  }

  @spec resolve_input(String.t()) :: SemanticInput.t()
  def resolve_input(sentence) when is_binary(sentence) do
    sentence
    |> Token.tokenize()
    #|> POSEngine.tag()
    #|> Brain.link_cells()
    #|> IntentClassifier.classify_tokens()
    #|> MoodCore.attach_mood()
    #|> ResponsePlanner.analyze()
  end
end

