# apps/core/test/core/mwe_injector_span_test.exs
defmodule Core.MWE.InjectorSpanTest do
  use ExUnit.Case, async: true

  alias Core.MWE.Injector

  test "inject derives MWE span from underlying token spans when available (char spans)" do
    tokens = [
      %{phrase: "good", span: {0, 4}, n: 1, mw: false},
      %{phrase: "morning", span: {5, 12}, n: 1, mw: false},
      %{phrase: "symbrella", span: {13, 22}, n: 1, mw: false}
    ]

    exists? = fn phrase ->
      phrase in ["good morning", "morning symbrella", "good morning symbrella"]
    end

    out = Injector.inject(tokens, max_n: 3, exists?: exists?)

    # ordering: words first, then MWEs; MWEs at same start: shorter before longer
    assert Enum.map(out, &Map.get(&1, :phrase)) == [
             "good",
             "morning",
             "symbrella",
             "good morning",
             "good morning symbrella",
             "morning symbrella"
           ]

    mwes = Enum.filter(out, &(&1[:mw] == true))
    by_phrase = Map.new(mwes, fn t -> {t.phrase, t} end)

    assert by_phrase["good morning"].span == {0, 12}
    assert by_phrase["good morning symbrella"].span == {0, 22}
    assert by_phrase["morning symbrella"].span == {5, 22}
  end

  test "inject falls back to {i, i+n} when spans are missing/unusable" do
    tokens = [
      %{phrase: "kick", span: nil, n: 1, mw: false},
      %{phrase: "the", span: nil, n: 1, mw: false},
      %{phrase: "bucket", span: nil, n: 1, mw: false}
    ]

    exists? = fn phrase -> phrase in ["kick the", "kick the bucket"] end

    out = Injector.inject(tokens, max_n: 3, exists?: exists?)

    mwes = Enum.filter(out, &(&1[:mw] == true))
    by_phrase = Map.new(mwes, fn t -> {t.phrase, t} end)

    assert by_phrase["kick the"].span == {0, 2}
    assert by_phrase["kick the bucket"].span == {0, 3}
  end
end
