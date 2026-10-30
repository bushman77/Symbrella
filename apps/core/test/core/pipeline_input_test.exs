defmodule Core.PipelineInputTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Input
  alias Core.TokenFilters

  defp source_slice(text, %{span: {start, stop}}) do
    binary_part(text, start, stop - start)
  end

  test "keeps original UTF-8 text and exact word/punctuation spans despite whitespace" do
    text = "  Café,  can't  fly! 👋\n"
    si = Input.prepare(text, max_wordgram_n: 3)

    assert si.original_text == text
    assert si.sentence == text
    assert si.tokenization.span_unit == :utf8_bytes
    assert si.tokenization.valid? == true
    assert Enum.map(si.tokens, & &1.phrase) == ["Café", "can't", "fly"]
    assert Enum.map(si.tokens, & &1.norm) == ["café", "can't", "fly"]
    assert Enum.map(si.tokens, & &1.index) == [0, 1, 2]

    assert Enum.all?(si.tokens, &(source_slice(text, &1) == &1.phrase))
    assert Enum.all?(si.punctuation_tokens, &(source_slice(text, &1) == &1.phrase))
    assert Enum.any?(si.punctuation_tokens, &(&1.phrase == ","))
    assert Enum.any?(si.punctuation_tokens, &(&1.phrase == "!"))
    assert Enum.any?(si.punctuation_tokens, &(&1.phrase == "👋"))
    assert Enum.find(si.phrase_candidates, &(&1.phrase == "can't fly")).surface ==
             "can't  fly"
    refute Enum.any?(si.phrase_candidates, &String.contains?(&1.phrase, "Café can't"))

    # The exact UTF-8 offsets, not character indices, reach the next stage.
    kept = TokenFilters.keep_only_word_boundary_tokens(si)
    assert kept.sentence == text
    assert kept.tokens == si.tokens
  end

  test "does not build lexical windows across sentence-final punctuation" do
    si = Input.prepare("The bird flies. The cat sleeps.", max_wordgram_n: 3)

    assert Enum.map(si.tokens, & &1.phrase) ==
             ["The", "bird", "flies", "The", "cat", "sleeps"]

    assert Enum.any?(si.phrase_candidates, &(&1.phrase == "The bird flies"))
    assert Enum.any?(si.phrase_candidates, &(&1.phrase == "The cat sleeps"))
    refute Enum.any?(si.phrase_candidates, &String.contains?(&1.phrase, "flies The"))
    assert Enum.count(si.punctuation_tokens, &(&1.subpos == :sentence_final)) == 2
  end

  test "line breaks are source-aligned and block phrase candidates" do
    si = Input.prepare("The bird.\nThe cat", max_wordgram_n: 3)

    assert Enum.any?(si.punctuation_tokens, &(&1.subpos == :line_break))
    assert Enum.any?(si.phrase_candidates, &(&1.phrase == "The bird"))
    assert Enum.any?(si.phrase_candidates, &(&1.phrase == "The cat"))
    refute Enum.any?(si.phrase_candidates, &String.contains?(&1.phrase, "bird The"))
  end

  test "preserves dotted initials and numbers without confusing final punctuation" do
    si = Input.prepare("U.S. GDP was 3.14.", max_wordgram_n: 2)

    assert Enum.map(si.tokens, & &1.phrase) == ["U.S", "GDP", "was", "3.14"]
    assert Enum.map(si.punctuation_tokens, & &1.phrase) == [".", "."]
    assert Enum.all?(si.tokens, &(source_slice(si.original_text, &1) == &1.phrase))
  end

  test "repeated lexical occurrences retain independent stable word indices" do
    si = Input.prepare("bank, bank.", max_wordgram_n: 2)

    assert Enum.map(si.tokens, & &1.index) == [0, 1]
    assert Enum.map(si.tokens, & &1.phrase) == ["bank", "bank"]
    assert Enum.map(si.tokens, & &1.span) == [{0, 4}, {6, 10}]
  end

  test "resolve_input test mode uses Phase 1 rather than rebuilding tokens" do
    si = Core.resolve_input("The  bird flies.", mode: :test, fuzzy: false)

    assert si.original_text == "The  bird flies."
    assert si.sentence == si.original_text
    assert si.tokenization.valid? == true
    assert Enum.map(si.tokens, & &1.phrase) == ["The", "bird", "flies"]
    assert Enum.any?(si.punctuation_tokens, &(&1.phrase == "."))
    assert Enum.all?(si.tokens, &(source_slice(si.original_text, &1) == &1.phrase))
  end

  test "fuzzy interpretation cannot rewrite canonical source tokens" do
    si = Core.resolve_input("teh  bird.", mode: :test, fuzzy: true)

    assert si.original_text == "teh  bird."
    assert si.sentence == si.original_text
    assert hd(si.tokens).phrase == "teh"
    assert si.tokenization.corrections_applied? == false
  end
end
