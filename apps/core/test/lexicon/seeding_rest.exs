# apps/core/test/lexicon/seeding_rest.exs
defmodule Core.Lexicon.SeedingTest do
  use ExUnit.Case, async: true
  alias Core.Lexicon

  test "unigram does not seed fallback" do
    :ok = Lexicon.ensure_cells_from_tokens([%{phrase: "Hello", pos: "interjection"}])
    rows = Db.Lexicon.fetch_by_norms(["hello"])
    refute Enum.any?(rows, &String.ends_with?(&1.id, "|fallback"))
  end

  test "phrase with head POS seeds pos fallback" do
    :ok =
      Lexicon.ensure_cells_from_tokens([
        %{phrase: "Hello there", pos: nil},
        %{phrase: "there", pos: "pron"}
      ])

    rows = Db.Lexicon.fetch_by_norms(["hello there"])
    assert Enum.any?(rows, &(&1.id == "hello there|pronoun|fallback"))
  end

  test "phrase without POS seeds phrase|fallback once" do
    phrase = "glurble snarfblat"        # improbable head word; lookup likely returns no POS
    :ok = Lexicon.ensure_cells_from_tokens([%{phrase: phrase}])

    rows = Db.Lexicon.fetch_by_norms([phrase])

    # exactly one conservative phrase fallback
    assert Enum.count(rows, &(&1.id == "#{phrase}|phrase|fallback")) == 1

    # and no POS-attached fallbacks for this phrase
    refute Enum.any?(rows, fn r ->
             String.starts_with?(r.id, "#{phrase}|") and
               String.ends_with?(r.id, "|fallback") and
               r.id != "#{phrase}|phrase|fallback"
           end)
  end
end

