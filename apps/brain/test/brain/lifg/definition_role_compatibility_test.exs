defmodule Brain.LIFG.DefinitionRoleCompatibilityTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.DefinitionRoleCompatibility, as: DefinitionRole
  alias Brain.LIFG.ContextFit
  alias Brain.LIFG.Stage1

  @finance %{
    id: "bank|noun|1", lemma: "bank", pos: "noun",
    definition: "(countable) An institution where one can place and borrow money and take care of financial affairs.",
    activation: 0.5
  }
  @gambling %{
    id: "bank|noun|5", lemma: "bank", pos: "noun",
    definition: "(countable) The sum of money which the dealer has as a fund from which to draw stakes.",
    activation: 0.5
  }
  @money %{
    id: "bank|noun|6", lemma: "bank", pos: "noun",
    definition: "(slang, uncountable) Money; profit.",
    activation: 0.5
  }
  @river %{
    id: "bank|noun|10", lemma: "bank", pos: "noun",
    definition: "An edge of river, lake, or other watercourse.",
    activation: 0.5
  }

  test "definition role signal actively separates same-word money senses" do
    sentence = "I deposited money at the bank."

    assert %{applied?: true, reason: :locative_venue_definition, bonus: 0.18} =
             DefinitionRole.evaluate(sentence, "bank", @finance)
    refute DefinitionRole.evaluate(sentence, "bank", @gambling).applied?
    refute DefinitionRole.evaluate(sentence, "bank", @money).applied?

    finance = ContextFit.explain(sentence, "bank", @finance)
    gambling = ContextFit.explain(sentence, "bank", @gambling)
    money = ContextFit.explain(sentence, "bank", @money)

    assert_in_delta finance.score, 0.47, 1.0e-6
    assert_in_delta gambling.score, 0.29, 1.0e-6
    assert_in_delta money.score, 0.29, 1.0e-6
    assert finance.definition_role.reason == :locative_venue_definition
  end

  test "production Stage1 and SemanticControl use the stronger definition evidence" do
    sentence = "I deposited money at the bank."

    si = %{
      sentence: sentence,
      tokens: [%{index: 0, phrase: "bank", n: 1, mw: false, span: {24, 28}}],
      sense_candidates: %{0 => [@finance, @gambling, @money, @river]}
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(si, scores: :all, preserve_sense_candidates: true,
               semantic_control?: true, cerebellum_learn?: false)

    assert choice.chosen_id == @finance.id
    assert choice.scores[@finance.id] > choice.scores[@gambling.id]
    assert choice.scores[@finance.id] > choice.scores[@money.id]
    assert choice.semantic_control.applied?
    assert Enum.any?(choice.definition_context_evidence, fn row ->
             row.id == @finance.id and row.definition_role.applied?
           end)
  end

  test "direct spoken salutation ranks exact ordinary greeting sense above telephone and sarcasm" do
    greeting = %{id: "hello|interjection|1", lemma: "hello", pos: "interjection",
      definition: "A greeting (salutation) said when meeting someone."}
    telephone = %{id: "hello|interjection|2", lemma: "hello", pos: "interjection",
      definition: "A greeting used when answering the telephone."}
    sarcasm = %{id: "hello|interjection|4", lemma: "hello", pos: "interjection",
      definition: "Used sarcastically to imply someone missed something."}

    assert ContextFit.score("Hello Symbrella.", "hello", greeting) > 0.0
    assert ContextFit.score("Hello Symbrella.", "hello", telephone) == 0.0
    assert ContextFit.score("Hello Symbrella.", "hello", sarcasm) == 0.0
    assert ContextFit.score("Hello?", "hello", greeting) == 0.0
  end

  test "abstains when grammar or candidate definition lacks enough evidence" do
    assert DefinitionRole.evaluate("I visited the bank.", "bank", @finance).bonus == 0.0
    assert DefinitionRole.evaluate("I deposited money on the bank.", "bank", @finance).bonus == 0.0
    assert DefinitionRole.evaluate("I deposited money at the bank.", "bank", %{definition: nil}).bonus == 0.0
    assert DefinitionRole.evaluate("The bank at the bank keeps money.", "bank", @finance).bonus == 0.0
    assert DefinitionRole.evaluate("The water flowed at the bank.", "bank", @river).bonus == 0.0
  end
end
