defmodule Core.Syntax.VerbArgumentShadowTest do
  use ExUnit.Case, async: true

  alias Core.Syntax.VerbArgumentShadow

  @lexicon [
    %{norm: "they", pos: "pronoun", status: "inactive"},
    %{norm: "artist", pos: "noun", status: "inactive"},
    %{norm: "draw", pos: "verb", status: "inactive"},
    %{norm: "card", pos: "noun", status: "inactive"},
    %{norm: "playing", pos: "adjective", status: "inactive"},
    %{norm: "playing", pos: "noun", status: "inactive"},
    %{norm: "sword", pos: "noun", status: "inactive"},
    %{norm: "sheath", pos: "noun", status: "inactive"},
    %{norm: "water", pos: "noun", status: "inactive"},
    %{norm: "well", pos: "noun", status: "inactive"},
    %{norm: "curtains", pos: "noun", status: "inactive"},
    %{norm: "pencil", pos: "noun", status: "inactive"},
    %{norm: "picture", pos: "noun", status: "inactive"}
  ]

  defp tokens(sentence) do
    Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, sentence)
    |> Enum.map(&hd/1)
    |> Enum.with_index()
    |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)
  end

  defp observe(sentence, index, lexicon \\ @lexicon) do
    VerbArgumentShadow.observe(tokens(sentence), index, lexicon)
  end

  test "surface determiner and attested noun head yield a bounded direct-object candidate" do
    for {sentence, verb_index, head, determiner} <- [
          {"They draw a playing card.", 1, 4, 2},
          {"They draw a sword from its sheath.", 1, 3, 2},
          {"They draw the curtains closed.", 1, 3, 2},
          {"They draw a picture.", 1, 3, 2},
          {"The artist will draw a picture.", 3, 5, 4}
        ] do
      report = observe(sentence, verb_index)
      assert report.status == :candidate
      assert report.frame_id == :verb_direct_object_hypothesis
      assert report.strength == :strong
      assert report.supports == :transitive
      assert report.roles.verb == verb_index
      assert report.roles.object_head == head
      assert report.roles.object_determiner == determiner
      assert report.rejects_intransitive? == false
      assert report.verified? == false
    end
  end

  test "bare-noun complement is weaker than a determiner-led object" do
    report = observe("They draw water from a well.", 1)
    assert report.status == :candidate
    assert report.strength == :weak
    assert report.roles.object_head == 2
    assert report.roles.object_determiner == nil
    assert report.supports == :transitive
    refute report.rejects_intransitive?
  end

  test "abstains for PP adjuncts, bare intransitive constructions, and incomplete NPs" do
    for {sentence, index, reason} <- [
          {"The artist will draw with a pencil.", 3,
           :following_prepositional_phrase_not_direct_object},
          {"They draw from a well.", 1,
           :following_prepositional_phrase_not_direct_object},
          {"They draw.", 1, :no_following_complement},
          {"They draw a.", 1, :determiner_without_attested_nominal_head},
          {"They draw a mysteryword.", 1, :determiner_without_attested_nominal_head},
          {"Draw a picture.", 0, :subject_frame_unresolved}
        ] do
      report = observe(sentence, index)
      assert report.status == :abstain
      assert report.reason == reason
      assert report.supports == nil
      refute report.rejects_intransitive?
      refute report.verified?
    end
  end

  test "does not cross unsupported noun modifiers or PP boundaries to invent an object" do
    assert observe("They draw a very old picture.", 1).reason ==
             :nominal_modifier_not_attested

    assert observe("They draw with a picture.", 1).status == :abstain
  end

  test "lifecycle status never removes lexical POS alternatives or mutates cells" do
    before = @lexicon
    all_active = Enum.map(before, &Map.put(&1, :status, "active"))

    baseline = observe("They draw a picture.", 1, before)
    promoted = observe("They draw a picture.", 1, all_active)

    assert baseline == promoted
    assert baseline.status == :candidate
    assert before == @lexicon
    assert Enum.all?(before, &(&1.status == "inactive"))
  end

  test "missing or unverified verb lexicon evidence forces abstention" do
    no_draw = Enum.reject(@lexicon, &(&1.norm == "draw"))
    assert observe("They draw a picture.", 1, no_draw).reason == :verb_pos_not_attested
    assert observe("They draw a picture.", 99).reason == :verb_token_missing
    assert VerbArgumentShadow.observe(nil, 1, []) == %{
      status: :abstain, reason: :invalid_input,
      supports: nil, rejects_intransitive?: false, verified?: false
    }
  end
end
