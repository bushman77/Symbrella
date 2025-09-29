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
end
