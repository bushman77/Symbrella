defmodule Brain.LIFG.Stage1.PosBiasWhPronounTest do
  @moduledoc """
  P-029: a wh-word directly followed by an auxiliary/BE/HAVE form favours the
  pronoun reading over determiner/particle readings. Other contexts are
  left neutral.
  """
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1
  alias Brain.LIFG.Stage1.PosBias

  defp bias(words, index, pos) do
    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, i} -> %{index: i, phrase: word} end)

    PosBias.window(%{si: %{tokens: tokens}}, Enum.at(tokens, index), pos)
  end

  # Difference against a control sentence whose next token is not an
  # auxiliary isolates the P-029 rule from the module's other local rules.
  defp delta(words, control, pos), do: bias(words, 0, pos) - bias(control, 0, pos)

  test "wh-word + auxiliary/BE/HAVE favours pronoun and disfavours determiner/particle" do
    for {words, control} <- [
          {~w(what are you up to), ~w(what time is it)},
          {~w(who is there), ~w(who knows)},
          {~w(which can fly), ~w(which book)},
          {~w(what did you say), ~w(what time)},
          {~w(whose has gone), ~w(whose book)}
        ] do
      assert_in_delta delta(words, control, "pronoun"), 0.34, 1.0e-9
      assert_in_delta delta(words, control, "determiner"), -0.34, 1.0e-9
      assert_in_delta delta(words, control, "particle"), -0.34, 1.0e-9
      assert_in_delta delta(words, control, "noun"), 0.0, 1.0e-9
    end
  end

  test "non-wh words and wh-words without an auxiliary follower are unaffected" do
    assert_in_delta delta(~w(that are fine), ~w(that time), "pronoun"), 0.0, 1.0e-9
    assert bias(~w(what), 0, "pronoun") == bias(~w(what time), 0, "pronoun")
    assert bias(~w(how are you), 0, "pronoun") == bias(~w(how come), 0, "pronoun")
  end

  defp what_si(next_word) do
    sentence = "what #{next_word}"

    candidates =
      (for n <- 1..4, do: %{id: "what|pronoun|#{n}", lemma: "what", pos: "pronoun", activation: 0.5}) ++
        (for n <- 1..3, do: %{id: "what|determiner|#{n}", lemma: "what", pos: "determiner", activation: 0.5}) ++
        (for n <- 1..2, do: %{id: "what|particle|#{n}", lemma: "what", pos: "particle", activation: 0.5})

    %{
      sentence: sentence,
      tokens: [
        %{index: 0, phrase: "what", n: 1, mw: false, span: {0, 4}},
        %{index: 1, phrase: next_word, n: 1, mw: false, span: {5, 5 + byte_size(next_word)}}
      ],
      sense_candidates: %{0 => candidates}
    }
  end

  defp base_logit(next_word, id) do
    {:ok, %{choices: choices}} =
      Stage1.run(what_si(next_word), scores: :all, preserve_sense_candidates: true,
        cerebellum_learn?: false)

    choice = Enum.find(choices, &(&1.token_index == 0))
    row = Enum.find(List.wrap(choice.definition_context_evidence), &(&1.id == id))
    {choice, row.base_logit}
  end

  # Posterior *mass* is bounded by Stage1's clamp01 + temperature-1 softmax
  # (COLLAB §24.1), so assert the rule's own effect on the base logit.
  test "in Stage1, 'what are' raises the pronoun base logit by the wh-pronoun bias" do
    {with_aux, aux_logit} = base_logit("are", "what|pronoun|1")
    {_control, control_logit} = base_logit("time", "what|pronoun|1")

    assert with_aux.chosen_id =~ "|pronoun|"
    assert_in_delta aux_logit - control_logit, 0.34, 1.0e-6
  end
end
