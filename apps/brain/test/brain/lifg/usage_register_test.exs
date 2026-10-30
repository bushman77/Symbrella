defmodule Brain.LIFG.UsageRegisterTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.{Stage1, UsageRegister}

  @on_modern "on|preposition|1"
  @on_obsolete "on|preposition|46"

  @ordinary %{
    id: @on_modern, norm: "on", pos: "preposition",
    definition: "Indicating position or location. — Positioned at the upper surface of, touching from above.",
    example: "A vase of flowers stood on the table."
  }

  @obsolete %{
    id: @on_obsolete, norm: "on", pos: "preposition",
    definition: "(dialectal, obsolete, regional) Of.",
    example: "I never seen 'im, and that's the truth on it."
  }

  test "detects only leading historical labels, not mere words in descriptions" do
    assert UsageRegister.historical_labels(@obsolete.definition) == ["obsolete"]
    assert UsageRegister.historical_labels("(archaic, dialectal) Word.") == ["archaic"]
    assert UsageRegister.historical_labels("[dated] Expression.") == ["dated"]
    assert UsageRegister.historical_labels("(dialectal, regional) Expression.") == []
    assert UsageRegister.historical_labels("A theory about obsolete machinery.") == []
    assert UsageRegister.historical_labels("An archaic language studied in history.") == []
    assert UsageRegister.historical_labels(nil) == []
  end

  test "ordinary context slightly disfavors an explicitly obsolete same-POS sense" do
    scores = %{@on_modern => 0.70, @on_obsolete => 0.70}

    {adjusted, report} =
      UsageRegister.adjust(scores, [@ordinary, @obsolete],
        "The fisherman sat on the bank of the river.")

    assert report.applied?
    assert report.reason == :explicit_historical_register
    assert [%{id: @on_obsolete, labels: ["obsolete"], delta: -0.06}] =
             report.adjustments
    assert adjusted[@on_modern] == scores[@on_modern]
    assert_in_delta adjusted[@on_obsolete], 0.64, 1.0e-9
    assert Enum.sort(Map.keys(adjusted)) == Enum.sort(Map.keys(scores))
  end

  test "historical-language questions preserve all scores" do
    scores = %{@on_modern => 0.7, @on_obsolete => 0.7}
    assert {^scores, %{applied?: false, reason: :historical_context}} =
             UsageRegister.adjust(scores, [@ordinary, @obsolete],
               "Explain the obsolete use of on in historical dialects.")
  end

  test "no same-POS ordinary witness means abstention" do
    scores = %{@on_obsolete => 0.7}
    assert {^scores, %{applied?: false}} =
             UsageRegister.adjust(scores, [@obsolete], "on")

    other_pos = %{@on_obsolete => 0.7, "on|noun|1" => 0.7}
    noun = %{id: "on|noun|1", norm: "on", pos: "noun",
      definition: "An ordinary noun."}
    assert {^other_pos, %{applied?: false}} =
             UsageRegister.adjust(other_pos, [@obsolete, noun], "on")
  end

  test "regional or dialectal alone are not treated as archaic" do
    regional = %{@obsolete | definition: "(regional, dialectal) In this region."}
    scores = %{@on_modern => 0.5, @on_obsolete => 0.5}
    assert {^scores, %{applied?: false}} =
             UsageRegister.adjust(scores, [@ordinary, regional], "on")
  end

  test "explicitly disabled prior leaves logits unchanged" do
    scores = %{@on_modern => 0.7, @on_obsolete => 0.7}
    assert {^scores, %{applied?: false, reason: :disabled}} =
             UsageRegister.adjust(scores, [@ordinary, @obsolete], "on",
               enabled?: false)
  end

  test "Stage1 continues full candidate competition but avoids obsolete tie winner" do
    si = %{
      sentence: "The fisherman sat on the bank of the river.",
      tokens: [%{index: 0, phrase: "on", n: 1, mw: false, span: {18, 20}}],
      sense_candidates: %{
        0 => [
          Map.merge(@ordinary, %{lemma: "on", activation: 0.5}),
          Map.merge(@obsolete, %{lemma: "on", activation: 0.5})
        ]
      }
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               semantic_control?: false, usage_register?: true)

    assert choice.chosen_id == @on_modern
    assert Map.has_key?(choice.scores, @on_modern)
    assert Map.has_key?(choice.scores, @on_obsolete)
    assert choice.usage_register.applied?
    assert [%{id: @on_obsolete}] = choice.usage_register.adjustments

    assert {:ok, %{choices: [baseline]}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               semantic_control?: false, usage_register?: false)

    assert baseline.usage_register.reason == :disabled
    assert baseline.usage_register.adjustments == []
    assert Map.keys(baseline.scores) |> Enum.sort() ==
             Map.keys(choice.scores) |> Enum.sort()
  end
end
