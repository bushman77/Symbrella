defmodule Core.Syntax.VerbRoleShadowGeneralizationTest do
  use ExUnit.Case, async: true

  alias Core.Syntax.VerbRoleShadow

  # Lexical POS evidence remains available irrespective of lifecycle status.
  @cells Enum.map(
    [
      {"draw", "verb"}, {"pull", "verb"}, {"remove", "verb"},
      {"withdraw", "verb"}, {"sword", "noun"}, {"sheath", "noun"},
      {"water", "noun"}, {"well", "noun"}, {"money", "noun"},
      {"bank", "noun"}, {"picture", "noun"}
    ],
    fn {norm, pos} -> %{norm: norm, pos: pos, status: "inactive"} end
  )

  defp tokens(sentence) do
    Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[\p{P}\p{S}]/u, sentence)
    |> Enum.map(&hd/1)
    |> Enum.with_index()
    |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end)
  end

  defp observe(sentence, verb_index, cells \\ @cells) do
    VerbRoleShadow.observe(tokens(sentence), verb_index, cells)
  end

  test "captures roles across multiple verbs and from/out-of source markers" do
    for {sentence, object, source, marker} <- [
          {"They draw water from a well.", "water", "well", "from"},
          {"They pull a sword out of its sheath.", "sword", "sheath", "out of"},
          {"They remove a sword from its sheath.", "sword", "sheath", "from"},
          {"They withdraw money from a bank.", "money", "bank", "from"},
          {"They draw water out of a well.", "water", "well", "out of"}
        ] do
      report = observe(sentence, 1)
      assert report.status == :candidate, inspect({sentence, report})
      assert report.words == %{object: object, preposition: marker, source: source}
      assert report.roles.verb == 1
      refute report.source_relation_verified?
      refute report.rejects_intransitive?
    end
  end

  test "out of preserves both preposition token indices" do
    report = observe("They pull a sword out of its sheath.", 1)
    assert report.roles.object_head == 3
    assert report.roles.source_preposition == 4
    assert report.roles.source_marker_end == 5
    assert report.roles.source_determiner == 6
    assert report.roles.source_head == 7
    assert :explicit_out_of_source_marker in report.evidence
  end

  test "rejects unrelated phrases, missing heads and interrupted source markers" do
    for {sentence, reason} <- [
          {"They withdraw money into a bank.", :no_adjacent_from_preposition},
          {"They pull a sword out. Of its sheath.", :no_adjacent_from_preposition},
          {"They pull a sword out, of its sheath.", :no_adjacent_from_preposition},
          {"They pull a sword out of.", :source_head_missing},
          {"They pull a sword out of mysteryword.", :source_head_not_lexically_attested},
          {"They pull from its sheath.", :direct_object_unresolved},
          {"They pull a sword — out of its sheath.", :no_adjacent_from_preposition},
          {"They pull a sword from. Its sheath.", :source_head_missing}
        ] do
      report = observe(sentence, 1)
      assert report.status == :abstain, inspect({sentence, report})
      assert report.reason == reason, inspect({sentence, report})
    end
  end

  test "no direct-object inference from an unrelated preceding noun" do
    assert observe("Pull a sword out of its sheath.", 0).reason == :direct_object_unresolved

    result = observe("They pull with a sword out of its sheath.", 1)
    assert result.status == :abstain
    assert result.reason == :direct_object_unresolved
  end

  test "lifecycle state does not affect grammatical role availability" do
    active = Enum.map(@cells, &Map.put(&1, :status, "active"))
    before = observe("They withdraw money from a bank.", 1, @cells)
    after_activation = observe("They withdraw money from a bank.", 1, active)
    assert before == after_activation
    assert Enum.all?(@cells, &(&1.status == "inactive"))
  end
end
