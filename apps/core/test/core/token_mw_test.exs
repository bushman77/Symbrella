defmodule Core.TokenMWTest do
  use ExUnit.Case, async: true

  defmodule PhraseRepoFake do
    def exists?(phrase) do
      norm =
        phrase
        |> String.downcase()
        |> String.trim()

      norm in ["kick the bucket", "kick the"]
      # or: (String.trim(String.downcase(phrase))) in [...]
    end
  end

  test "tokenizer builds MW tokens when repo knows the phrase" do
    si = Core.Token.tokenize("Kick the bucket today")

    assert Enum.any?(si.tokens, fn t ->
             String.downcase(t.phrase) == "kick the bucket" and t.mw == true
           end),
           "Tokenizer did not build 'kick the bucket' MW token. Got: #{inspect(si.tokens)}"
  end
end
