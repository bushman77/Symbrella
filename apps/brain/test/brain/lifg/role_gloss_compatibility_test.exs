defmodule Brain.LIFG.RoleGlossCompatibilityTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.RoleGlossCompatibility, as: Compat

  test "one local role grammar applies to different verbs and paraphrased glosses" do
    for {object, source, sentence_marker, gloss, expected_pattern} <- [
          {"water", "well", "from", "To take water out of a well.", "out of"},
          {"water", "well", "out of", "To draw water from a well.", "from"},
          {"water", "well", "from", "To take up (water) from a well.", "from"},
          {"sword", "sheath", "from", "To pull a sword out of its sheath.", "out of"},
          {"sword", "sheath", "out of", "To remove a sword from the sheath.", "from"},
          {"money", "bank", "from", "To withdraw money from a bank.", "from"},
          {"money", "bank", "from", "To take money from a large commercial bank.", "from"},
          {"water", "well", "from", "To get water from an old stone well.", "from"}
        ] do
      assert {:ok, trace} =
               Compat.match(gloss, %{object: object, source: source,
                                     preposition: sentence_marker})
      assert trace.pattern == expected_pattern
      assert trace.modifier_count in 0..2
    end
  end

  test "never substitutes unrelated nouns or reversed semantic relations" do
    for gloss <- [
          "A well that holds water and supports fish.",
          "A well receives water.",
          "Money is deposited into a bank.",
          "A bank sends money to customers.",
          "To draw a well containing water.",
          "The sword was thrust into a sheath."
        ] do
      for {object, source} <- [{"water", "well"}, {"money", "bank"}, {"sword", "sheath"}] do
        assert :no_match == Compat.match(gloss, %{
          object: object, source: source, preposition: "from"
        })
      end
    end
  end

  test "does not match across punctuation or clause boundaries in a gloss" do
    for gloss <- [
          "To collect water. From a well.",
          "To collect water; from a well.",
          "To collect water, from a well.",
          "To collect water (from a well).",
          "To collect water and from a well.",
          "To collect water from a. Well.",
          "To collect water from an and well."
        ] do
      assert Compat.match(gloss, %{object: "water", source: "well",
                                   preposition: "from"}) == :no_match
    end
  end

  test "a shared generic word does not equal an object-source relation" do
    assert Compat.match("A vessel draws water as its depth increases.",
             %{object: "water", source: "well", preposition: "from"}) == :no_match

    assert Compat.match("The bank receives money from a client.",
             %{object: "money", source: "bank", preposition: "from"}) == :no_match
  end

  test "rejects incomplete and unsupported inputs without writing state" do
    for relation <- [
          %{object: "water", source: "well", preposition: "with"},
          %{object: "", source: "well", preposition: "from"},
          %{object: "water", source: "", preposition: "from"}
        ] do
      assert Compat.match("To draw water from a well.", relation) == :no_match
    end

    assert Compat.match(nil,
             %{object: "water", source: "well", preposition: "from"}) == :no_match
  end
end
