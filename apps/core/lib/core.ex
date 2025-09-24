defmodule Core do
  alias Core.{Brain, Lexicon, SemanticInput, Token}

  def resolve_input(phrase) do
    phrase
    |> Token.tokenize()
    |> Brain.stm()
    |> Db.ltm()
    |> Lexicon.all()
  end
end
