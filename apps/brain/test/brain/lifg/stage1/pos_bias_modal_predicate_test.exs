defmodule Brain.LIFG.Stage1.PosBiasModalPredicateTest do
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

  test "auxiliary inversion promotes a predicate, not an arbitrary noun" do
    for words <- [
          ~w(can you write me a basic GenServer),
          ~w(could you write a program),
          ~w(will she book a room),
          ~w(does he draw a picture),
          ~w(why can you write a program)
        ] do
      index =
        words
        |> Enum.find_index(&(&1 in ~w(write book draw)))

      assert bias(words, index, "verb") == 0.34
      assert bias(words, index, "noun") == -0.14
      assert bias(words, index, "proper_noun") == -0.14
    end
  end

  test "determined subjects and polite or negative interruptions retain the predicate slot" do
    for {words, index} <- [
          {~w(can the programmer write a module), 3},
          {~w(can you please write a program), 3},
          {~w(could you not write a file), 3},
          {~w(can you please not write a file), 4}
        ] do
      assert bias(words, index, "verb") == 0.34
      assert bias(words, index, "noun") == -0.14
    end
  end

  test "subject plus modal supports a following existing verb sense" do
    for {words, index} <- [
          {~w(i can write a report), 2},
          {~w(the programmer can write a module), 3},
          {~w(the worker will file a report), 3}
        ] do
      assert bias(words, index, "verb") == 0.34
      assert bias(words, index, "noun") == -0.14
    end
  end

  test "direct imperatives have a predicate slot without a hard-coded action list" do
    assert bias(~w(write me a note), 0, "verb") == 0.30
    assert bias(~w(please book a room), 1, "verb") == 0.30
    assert bias(~w(please book a room), 1, "noun") == -0.12
  end

  test "no speculative verb boost for non-predicates or missing structure" do
    assert bias(~w(the book cover), 1, "verb") == 0.0
    assert bias(~w(i went to the bank), 4, "verb") == 0.0
    assert bias(~w(can you a write), 3, "verb") == 0.0
    assert bias(~w(the worker and nurse), 3, "verb") == 0.0
    assert bias(~w(write), 0, "verb") == 0.0
  end

  test "Stage1 favors dictionary verb entries over a noun for a real modal request" do
    words = ~w(Can you write me a basic GenServer in MyExample?)
    sentence = Enum.join(words, " ")

    {tokens, _} =
      words
      |> Enum.with_index()
      |> Enum.map_reduce(0, fn {word, index}, start ->
        stop = start + byte_size(word)
        {%{index: index, phrase: word, n: 1, mw: false, span: {start, stop}},
         stop + 1}
      end)

    shared = %{norm: "write", lemma: "write", activation: 0.5}
    noun = Map.merge(shared, %{id: "write|noun|1", pos: "noun"})
    verb1 = Map.merge(shared, %{id: "write|verb|1", pos: "verb"})
    verb2 = Map.merge(shared, %{id: "write|verb|2", pos: "verb"})

    si = %{
      sentence: sentence,
      tokens: tokens,
      active_cells: [noun, verb1, verb2],
      sense_candidates: %{2 => [noun, verb1, verb2]},
      sentence_signatures: []
    }

    assert {:ok, %{choices: choices}} =
             Stage1.run(si,
               scores: :all,
               db_backfill?: false,
               preserve_sense_candidates: true,
               semantic_control?: false,
               weights: [
                 lex_fit: 0.0, context_fit: 0.0, rel_prior: 0.0,
                 activation: 1.0, intent_bias: 0.0
               ]
             )

    selected = Enum.find(choices, &(&1.token_index == 2))
    assert selected
    assert String.contains?(selected.chosen_id, "|verb|")
    assert selected.scores["write|verb|1"] > selected.scores["write|noun|1"]
    assert selected.scores["write|verb|2"] > selected.scores["write|noun|1"]
    # Contextual POS evidence must not fabricate an answer to verb-sense ambiguity.
    assert selected.scores["write|verb|1"] == selected.scores["write|verb|2"]
  end
end
