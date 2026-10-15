defmodule Core.Syntax.VerbRoleShadowTest do
  use ExUnit.Case, async: true

  alias Core.Syntax.VerbRoleShadow

  # These are dictionary POS entries, not activated cognitive state.
  @lexicon [
    %{norm: "draw", pos: "verb", status: "inactive"},
    %{norm: "water", pos: "noun", status: "inactive"},
    %{norm: "well", pos: "noun", status: "inactive"},
    %{norm: "picture", pos: "noun", status: "inactive"},
    %{norm: "sword", pos: "noun", status: "inactive"},
    %{norm: "artist", pos: "noun", status: "inactive"},
    %{norm: "pencil", pos: "noun", status: "inactive"}
  ]

  # Unlike a plain word-only scanner, this keeps punctuation as tokens so
  # sentence boundaries cannot silently collapse during shadow observation.
  defp tokenize(sentence) do
    Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[.!?;,:]/u, sentence)
    |> Enum.map(&hd/1)
    |> Enum.with_index()
    |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)
  end

  defp observe(sentence, index, cells \\ @lexicon) do
    VerbRoleShadow.observe(tokenize(sentence), index, cells)
  end

  test "preserves object, source and preposition indices without source-window expansion" do
    report = observe("They draw water from a well.", 1)

    assert report.status == :candidate
    assert report.frame_id == :verb_object_source_hypothesis
    assert report.roles == %{
      verb: 1, object_head: 2, source_preposition: 3,
      source_head: 5, source_determiner: 4
    }
    assert report.words == %{object: "water", preposition: "from", source: "well"}
    refute report.verified?
    refute report.source_relation_verified?
    refute report.rejects_intransitive?
  end

  test "strict source proximity with a determiner-led object" do
    report = observe("They draw a picture from a well.", 1)
    assert report.status == :candidate
    assert report.roles.object_head == 3
    assert report.roles.source_preposition == 4
    assert report.roles.source_head == 6
  end

  test "does not infer source role from unrelated prepositions or unverified heads" do
    for {sentence, index, reason} <- [
          {"They draw water with a well.", 1, :no_adjacent_from_preposition},
          {"They draw water because from a well.", 1, :no_adjacent_from_preposition},
          {"They draw water from a mysteryword.", 1, :source_head_not_lexically_attested},
          {"They draw water from a.", 1, :source_head_missing},
          {"They draw water from.", 1, :source_head_missing},
          {"They draw water.", 1, :no_adjacent_from_preposition},
          {"They draw from a well.", 1, :direct_object_unresolved},
          {"The artist will draw with a pencil.", 3, :direct_object_unresolved}
        ] do
      result = observe(sentence, index)
      assert result.status == :abstain, inspect({sentence, result})
      assert result.reason == reason, inspect({sentence, result})
      refute result.rejects_intransitive?
    end
  end

  test "never joins sentences or punctuation-separated clauses to find an object/source" do
    for {sentence, index} <- [
          {"They draw water. From a well.", 1},
          {"They draw water; from a well.", 1},
          {"They draw water, from a well.", 1},
          {"They draw a. Picture from a well.", 1},
          {"They draw a picture. From a well.", 1},
          {"They draw a picture; from a well.", 1}
        ] do
      assert observe(sentence, index).status == :abstain, sentence
    end
  end

  test "no noun elsewhere in the clause can substitute for an immediate direct object" do
    assert observe("A sword draws.", 2).status == :abstain
    assert observe("They draw with a picture from a well.", 1).status == :abstain
    assert observe("Draw water from a well.", 0).status == :abstain
  end

  test "lifecycles never determine lexical visibility or mutate the corpus" do
    changed = Enum.map(@lexicon, &Map.put(&1, :status, "active"))
    baseline = observe("They draw water from a well.", 1, @lexicon)
    activated = observe("They draw water from a well.", 1, changed)
    assert baseline == activated
    assert Enum.all?(@lexicon, &(&1.status == "inactive"))
  end
end
