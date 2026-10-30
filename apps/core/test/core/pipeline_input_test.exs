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

  test "optional grammatical projection exposes possessive stem without changing source tokens" do
    text = "10 of my checker's 13 links were lines that refute the claim"
    canonical = Input.prepare(text, max_wordgram_n: 1)
    grammatical = Input.grammatical_projection(canonical)

    assert Enum.any?(canonical.tokens, &(&1.phrase == "checker's"))
    refute Enum.any?(canonical.tokens, &(&1.phrase == "checker"))
    assert Enum.any?(grammatical.tokens, &(&1.phrase == "checker"))
    assert Enum.any?(grammatical.tokens, &(&1.phrase == "'s"))
    assert Enum.map(grammatical.tokens, & &1.index) ==
             Enum.to_list(0..(length(grammatical.tokens) - 1))
    assert Enum.all?(grammatical.tokens,
             &(source_slice(text, &1) == &1.phrase))

    assert grammatical.original_text == text
    assert canonical.tokenization.valid? == true
    assert Enum.map(canonical.tokens, & &1.phrase) ==
             Enum.map(Input.prepare(text, max_wordgram_n: 1).tokens, & &1.phrase)
  end

  test "non-possessive contractions do not get broken by grammatical projection" do
    text = "It's true that he can't read it."
    canonical = Input.prepare(text, max_wordgram_n: 1)
    grammatical = Input.grammatical_projection(canonical)

    assert Enum.map(grammatical.tokens, & &1.phrase) ==
             Enum.map(canonical.tokens, & &1.phrase)
  end

  test "Brain's legacy span normalizer uses UTF-8 byte coordinates" do
    sentence = "Café is here"
    tokens = [%{index: 1, phrase: "is", span: {6, 8}}]

    assert [%{index: 1, phrase: "is", span: {6, 8}}] =
             Brain.Utils.Tokens.normalize_tokens(tokens, sentence)
  end

  test "Brain STM preserves canonical Phase 1 token identity" do
    si = Input.prepare("Café is here.", max_wordgram_n: 2)
    original_tokens = si.tokens

    if is_pid(Process.whereis(Brain)) do
      after_stm = Core.Brain.STM.run(Map.from_struct(si))
      assert after_stm.tokens == original_tokens
      assert after_stm.tokenization == si.tokenization
      assert Enum.all?(after_stm.tokens,
               &(source_slice(si.original_text, &1) == &1.phrase))
    end
  end

  test "Phase 2 reuses the exact Phase 1 fuzzy evidence instead of recalculating it" do
    original = "helo bird"
    si = Core.resolve_input(original, mode: :test, fuzzy: true)

    assert si.original_text == original
    assert hd(si.tokens).phrase == "helo"
    assert si.fuzzy_text.original == original
    assert si.fuzzy_text.text =~ "hello"
    assert is_list(si.fuzzy_text.evidence)
    assert si.keyword in ["helo", "hello"]
    assert is_list(si.intent_evidence)
  end

  test "fuzzy false prevents intent selection from rerunning spelling correction" do
    si = Core.resolve_input("helo", mode: :test, fuzzy: false)

    assert si.sentence == "helo"
    assert hd(si.tokens).phrase == "helo"
    assert si.fuzzy_text == nil
    assert si.keyword == "helo"
  end

  test "reference arbitration retains independent conflicting antecedents" do
    one = %{
      source: :wm_comprehension, antecedents: ["Kamloops BC", "Maple Ridge BC"],
      verification: :conversation_context_only
    }

    other = %{
      source: :recent_user_turn, antecedent: "Vancouver BC",
      verification: :conversation_context_only
    }

    assert {:ambiguous, audit} = Core.Discourse.References.arbitrate([one, other])
    assert audit.status == :ambiguous
    assert length(audit.candidates) == 2
    assert audit.factual_authority? == false
  end

  test "reference arbitration corroborates the same referent and retains source provenance" do
    one = %{source: :wm_nominal_focus, antecedent: "The Black Bird"}
    two = %{source: :discourse_ledger, antecedent: "the black bird"}

    assert {:unique, ^one, audit} = Core.Discourse.References.arbitrate([one, two])
    assert audit.status == :corroborated
    assert Enum.map(audit.candidates, & &1.source) ==
             [:wm_nominal_focus, :discourse_ledger]
  end

  test "fuzzy interpretation cannot rewrite canonical source tokens" do
    si = Core.resolve_input("teh  bird.", mode: :test, fuzzy: true)

    assert si.original_text == "teh  bird."
    assert si.sentence == si.original_text
    assert hd(si.tokens).phrase == "teh"
    assert si.tokenization.corrections_applied? == false
  end
end
