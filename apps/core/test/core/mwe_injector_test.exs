# apps/core/test/core/mwe_injector_test.exs
defmodule Core.MWE.InjectorTest do
  use ExUnit.Case, async: true
  alias Core.MWE.Injector

  defp exists_from_set(set), do: fn phrase -> MapSet.member?(set, phrase) end

  test "injects a 3-word MWE and keeps originals" do
    # tokens for: "kick the bucket"
    tokens = [
      %{phrase: "kick", span: {0, 1}},
      %{phrase: "the", span: {1, 2}},
      %{phrase: "bucket", span: {2, 3}}
    ]

    repo = MapSet.new(["kick the bucket"])
    out = Injector.inject(tokens, exists?: exists_from_set(repo), max_n: 4)

    phrases = Enum.map(out, & &1.phrase)
    assert phrases == ["kick", "the", "bucket", "kick the bucket"]

    mwe =
      Enum.find(out, fn t ->
        Map.get(t, :mw, false) == true and Map.get(t, :phrase) == "kick the bucket"
      end)

    refute is_nil(mwe)
    assert mwe.span == {0, 3}
    assert mwe.n == 3
  end

  test "injects overlapping MWEs when present in repo" do
    # tokens: "new york city mayor"
    tokens = [
      %{phrase: "new", span: {0, 1}},
      %{phrase: "york", span: {1, 2}},
      %{phrase: "city", span: {2, 3}},
      %{phrase: "mayor", span: {3, 4}}
    ]

    repo = MapSet.new(["new york", "new york city"])
    out = Injector.inject(tokens, exists?: exists_from_set(repo), max_n: 4)

    phrases = Enum.map(out, & &1.phrase)
    assert phrases == ["new", "york", "city", "mayor", "new york", "new york city"]

    ny = Enum.find(out, fn t -> Map.get(t, :mw, false) and t.phrase == "new york" end)
    nyc = Enum.find(out, fn t -> Map.get(t, :mw, false) and t.phrase == "new york city" end)

    assert ny.span == {0, 2}
    assert ny.n == 2
    assert nyc.span == {0, 3}
    assert nyc.n == 3
  end

  test "does not duplicate an existing MWE token" do
    tokens = [
      %{phrase: "kick", span: {0, 1}},
      %{phrase: "the", span: {1, 2}},
      %{phrase: "bucket", span: {2, 3}},
      %{phrase: "kick the bucket", mw: true, span: {0, 3}, n: 3}
    ]

    repo = MapSet.new(["kick the bucket"])
    out = Injector.inject(tokens, exists?: exists_from_set(repo), max_n: 4)

    just_mwes =
      out
      |> Enum.filter(&Map.get(&1, :mw, false))
      |> Enum.map(&{&1.phrase, &1.span})

    assert just_mwes == [{"kick the bucket", {0, 3}}]
  end

  test "returns original tokens when repo never confirms any MWE" do
    tokens = [
      %{phrase: "alpha", span: {0, 1}},
      %{phrase: "beta", span: {1, 2}},
      %{phrase: "gamma", span: {2, 3}}
    ]

    repo = MapSet.new([])
    out = Injector.inject(tokens, exists?: exists_from_set(repo), max_n: 4)

    # No injected MWEs
    refute Enum.any?(out, fn t -> Map.get(t, :mw, false) or Map.get(t, "mw", false) end)

    # Same number of tokens and same phrases in order
    assert length(out) == length(tokens)
    assert Enum.map(out, & &1.phrase) == Enum.map(tokens, & &1.phrase)
  end

  test "does not inject MWEs from tokens that already look like char-grams" do
    tokens = [
      %{phrase: "Ki", span: {0, 1}},
      # char-gram-ish: includes a space and not a clean word
      %{phrase: "ck t", span: {1, 2}},
      %{phrase: "the", span: {2, 3}}
    ]

    # Even if the repo would recognise suspicious phrases, Injector should
    # only build MWEs from single-word strings.
    repo = MapSet.new(["Ki ck t", "ck t the"])
    out = Injector.inject(tokens, exists?: exists_from_set(repo), max_n: 3)

    mwes =
      out
      |> Enum.filter(fn t -> Map.get(t, :mw, false) or Map.get(t, "mw", false) end)

    # No MWEs should be injected at all from "ck t" style junk.
    assert mwes == []

    # Base tokens are preserved
    assert Enum.map(out, & &1.phrase) == Enum.map(tokens, & &1.phrase)
  end
end

