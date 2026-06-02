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

  test "injector builds confirmed MW tokens when repo knows the phrase" do
    si = Core.Token.tokenize("Kick the bucket today")
    tokens = Core.MWE.Injector.inject(si.tokens, exists?: &PhraseRepoFake.exists?/1)

    assert Enum.any?(tokens, fn t ->
             String.downcase(t.phrase) == "kick the bucket" and t.mw == true and
               Map.get(t, :confirmed?) == true
           end),
           "Injector did not build confirmed 'kick the bucket' MW token. Got: #{inspect(tokens)}"
  end
end
