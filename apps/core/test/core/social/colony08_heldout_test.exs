defmodule Core.Social.Colony08HeldoutTest do
  use ExUnit.Case, async: true

  @path Path.expand("../../../../../priv/colony08_heldout_cases.json", __DIR__)
  @payload Jason.decode!(File.read!(@path))
  @cases Map.fetch!(@payload, "cases")

  @colony07 [
    "The checker produced 13 links.",
    "Ten links contradicted the original claims.",
    "Three links supported the claims.",
    "The checker did not verify those claims.",
    "The checker verified those claims.",
    "Three links did not support the claims.",
    "Three links might support the claims.",
    "Three links supported and contradicted the claims."
  ]

  test "held-out targets are frozen, unique, and disjoint from COLONY-07" do
    assert @payload["version"] == 1
    assert length(@cases) == 37

    ids = Enum.map(@cases, & &1["id"])
    sentences = Enum.map(@cases, & &1["sentence"])
    assert Enum.uniq(ids) == ids
    assert Enum.uniq(sentences) == sentences
    assert Enum.empty?(Enum.filter(sentences, &(&1 in @colony07)))
    assert Enum.all?(sentences, &(is_binary(&1) and String.length(&1) <= 420))

    assert Enum.frequencies(Enum.map(@cases, & &1["group"])) == %{
             "transfer" => 18, "frontier" => 7, "safety" => 12
           }
  end

  test "transfer and frontier require full structural labels, not keyword success" do
    for item <- @cases, item["group"] in ["transfer", "frontier"] do
      expected = item["expected"]
      assert expected["outcome"] == "interpret"
      assert expected["relation"] in
               ["reported_action", "reported_support", "reported_refutation"]
      assert expected["polarity"] in ["positive", "negative"]
      assert Enum.all?(["subject", "predicate", "object"], fn field ->
               is_binary(expected[field]) and expected[field] != ""
             end)
      assert Map.has_key?(expected, "count")
      assert is_nil(expected["count"]) or
               (is_integer(expected["count"]) and expected["count"] > 0)
    end
  end

  test "safety negatives explicitly demand abstention instead of a plausible answer" do
    safety = Enum.filter(@cases, &(&1["group"] == "safety"))

    assert Enum.all?(safety, &(&1["expected"]["outcome"] == "abstain"))
    assert Enum.count(safety, &(&1["expected"]["reason"] == "modal_uncertainty")) == 3

    assert Enum.any?(safety, &String.contains?(&1["sentence"], ";"))
    assert Enum.any?(safety, &String.contains?(&1["sentence"], "?"))
    assert Enum.any?(safety, &String.contains?(&1["sentence"], " and "))
  end

  test "matched affirmative/negative pairs differ only in polarity and verb form" do
    pairs =
      @cases
      |> Enum.filter(&Map.has_key?(&1, "pair"))
      |> Enum.group_by(& &1["pair"])

    assert map_size(pairs) == 5
    assert Enum.all?(pairs, fn {_pair, cases} -> length(cases) == 2 end)

    for {_pair, [first, second]} <- pairs do
      a = first["expected"]
      b = second["expected"]

      assert first["group"] == "transfer"
      assert second["group"] == "transfer"
      assert Enum.sort([a["polarity"], b["polarity"]]) == ["negative", "positive"]
      assert a["relation"] == b["relation"]
      assert a["subject"] == b["subject"]
      assert a["object"] == b["object"]
      assert a["count"] == b["count"]
      refute a["predicate"] == b["predicate"]
    end
  end
end
