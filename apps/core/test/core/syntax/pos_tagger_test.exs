defmodule Core.Syntax.PosTaggerTest do
  @moduledoc """
  P-036 (shadow): whole-sentence POS tagger. Expected tags were reproduced
  from Bradley's live experiment (COLLAB §29) using the same attested POS
  sets; the "draw" result is a documented bigram limitation, not a target.
  """
  use ExUnit.Case, async: true

  alias Core.Syntax.{PosTagger, Signatures}

  # Attested POS families per word, as observed from the live lexicon.
  @attested %{
    "hey" => ~w(interjection noun proper_noun verb), "symbrella" => [],
    "what" => ~w(determiner particle pronoun), "are" => ~w(determiner noun proper_noun verb),
    "you" => ~w(determiner noun pronoun proper_noun verb),
    "up" => ~w(adjective adverb noun preposition proper_noun verb),
    "to" => ~w(adverb conjunction noun particle preposition proper_noun verb),
    "how" => ~w(adverb conjunction interjection noun proper_noun),
    "about" => ~w(adjective adverb preposition verb), "them" => ~w(determiner noun pronoun),
    "canucks" => ~w(noun), "did" => ~w(noun verb), "the" => ~w(adverb determiner preposition pronoun),
    "artist" => ~w(adjective noun proper_noun), "draw" => ~w(interjection noun verb),
    "i" => ~w(interjection noun numeral pronoun), "gave" => ~w(verb), "plan" => ~w(noun proper_noun verb),
    "is" => ~w(verb), "she" => ~w(determiner noun pronoun proper_noun verb),
    "doing" => ~w(interjection noun verb), "look" => ~w(interjection noun proper_noun verb),
    "word" => ~w(interjection noun proper_noun verb), "they" => ~w(determiner noun pronoun verb),
    "eat" => ~w(noun verb), "apples" => ~w(adjective noun verb)
  }

  defp tags(sentence) do
    sentence
    |> String.split()
    |> Enum.with_index()
    |> Enum.map(fn {w, i} -> %{index: i, word: w, candidates: PosTagger.candidates(w, Map.fetch!(@attested, w))} end)
    |> PosTagger.tag()
    |> Enum.map(& &1.tag)
  end

  test "tags the experiment sentences as observed live" do
    for {sentence, expected} <- [
          {"hey symbrella what are you up to", ~w(interjection proper_noun pronoun auxiliary pronoun adverb particle)},
          {"how about them canucks", ~w(adverb preposition pronoun noun)},
          {"i gave up the plan", ~w(pronoun verb particle determiner noun)},
          {"what is she doing", ~w(pronoun auxiliary pronoun verb)},
          {"you look up the word", ~w(pronoun verb particle determiner noun)},
          {"they eat apples", ~w(pronoun verb noun)}
        ] do
      assert tags(sentence) == expected, sentence
    end
  end

  test "known bigram limitation: long-range do-support is not visible" do
    # "did ... draw" needs a verb; a bigram tagger tags "draw" as a noun.
    # Core.Syntax.Signatures (:wh_aux_object_question) covers this case.
    assert tags("what did the artist draw") == ~w(pronoun auxiliary determiner noun noun)
  end

  test "candidates add the closed auxiliary/particle/name augment" do
    assert PosTagger.candidates("are", ["verb"]) == ~w(auxiliary verb)
    assert PosTagger.candidates("up", ["adverb"]) == ~w(adverb particle)
    assert PosTagger.candidates("symbrella", []) == ~w(proper_noun)
    assert PosTagger.candidates("apples", ~w(noun verb)) == ~w(noun verb)
  end

  test "deterministic, bounded and total" do
    assert tags("they eat apples") == tags("they eat apples")
    assert PosTagger.tag([]) == []

    long = for i <- 0..64, do: %{index: i, word: "the", candidates: ["determiner"]}
    assert PosTagger.tag(long) == []

    assert [%{tag: "noun", candidates: ["noun"]}] =
             PosTagger.tag([%{index: 0, word: "zzqx", candidates: []}])
  end

  describe "attach/1 is shadow-only" do
    defp si do
      words = ~w(you look up the word)

      %{
        sentence: "you look up the word",
        source: :test,
        tokens: words |> Enum.with_index() |> Enum.map(fn {w, i} -> %{index: i, phrase: w, n: 1} end),
        active_cells:
          for(w <- words, pos <- Map.fetch!(@attested, w), do: %{id: "#{w}|#{pos}|1", norm: w, pos: pos}),
        trace: []
      }
    end

    test "adds :pos_tags built from tokens and active_cells" do
      out = PosTagger.attach(si())

      assert Enum.map(out.pos_tags, &{&1.token_index, &1.word, &1.tag}) == [
               {0, "you", "pronoun"}, {1, "look", "verb"}, {2, "up", "particle"},
               {3, "the", "determiner"}, {4, "word", "noun"}
             ]
    end

    test "changes nothing else, and signatures are identical with or without it" do
      base = si()
      out = PosTagger.attach(base)

      assert Map.delete(out, :pos_tags) == base
      assert Signatures.detect(out) == Signatures.detect(base)
    end
  end
end
