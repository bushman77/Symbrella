defmodule Core do
  alias Core.{SemanticInput, Token}

  def resolve_input(phrase) do
    phrase
    |> Token.tokenize()
    
  end
end
