defmodule Core.MWE.LIFGBridgeTest do
  use ExUnit.Case, async: true

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.MWE.{Lexicon, LIFGBridge}

  test "known verb MWE is an extra LIFG candidate, not a replacement for base tokens" do
    si =
      %SemanticInput{sentence: "I need to turn off the lights.", tokens: [], trace: []}
      |> TokenFilters.rebuild_word_ngrams(3)
      |> Lexicon.attach(
        mwe_lookup: fn _phrases ->
          [
            %{
              id: "turn off|verb|2",
              norm: "turn off",
              pos: "verb",
              definition: "To deactivate an appliance",
              gram_function: ["transitive"]
            }
          ]
        end
      )
      |> TokenResolution.resolve()

    {input, virtual} = LIFGBridge.prepare(si)

    assert length(si.tokens) == 7
    assert length(input.tokens) == 8
    assert Enum.map(Enum.take(input.tokens, 7), & &1.phrase) ==
             Enum.map(si.tokens, & &1.phrase)

    assert %{7 => %{phrase: "turn off", token_span: {3, 4}}} = virtual
    assert Enum.at(input.tokens, 7).mw == true
    assert [%{id: "turn off|verb|2", pos: "verb"}] = input.sense_candidates[7]
    assert input.sense_candidates[7] |> hd() |> Map.get(:source) == :mwe_lexicon

    choices = [
      %{token_index: 3, id: "turn|noun|15"},
      %{token_index: 7, id: "turn off|verb|2", pos: "verb"}
    ]

    assert {[%{id: "turn|noun|15"}], [%{id: "turn off|verb|2", token_span: {3, 4}}]} =
             LIFGBridge.split_choices(choices, virtual)
  end

  test "DB misses cannot invent virtual lexical sense candidates" do
    si = %SemanticInput{sentence: "turn off", tokens: [], trace: []}
         |> TokenFilters.rebuild_word_ngrams(3)

    assert {^si, %{}} = LIFGBridge.prepare(si)
    assert {^si, %{}} = LIFGBridge.prepare(si, mwe_lifg: :off)
  end
end
