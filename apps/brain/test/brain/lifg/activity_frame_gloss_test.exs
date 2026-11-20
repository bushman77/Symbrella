defmodule Brain.LIFG.ActivityFrameGlossTest do
  @moduledoc """
  P-033 (staged): inside a Core :wh_be_activity_question frame, the MWE
  "up to" prefers the sense whose gloss begins with an activity cue.
  Inert without the frame.
  """
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  @willing %{id: "up to|adjective|1", norm: "up to", lemma: "up to", pos: "adjective",
             definition: "(uncommon) Willing, interested in.", activation: 0.5}
  @doing %{id: "up to|preposition|7", norm: "up to", lemma: "up to", pos: "preposition",
           definition: "Doing, involved in.", activation: 0.5}
  @until %{id: "up to|preposition|5", norm: "up to", lemma: "up to", pos: "preposition",
           definition: "Until.", activation: 0.5}

  @frame %{
    id: :wh_be_activity_question,
    source: :core_syntax,
    span: {2, 6},
    roles: %{wh: 2, auxiliary: 3, subject: 4, predicate: 5, predicate_end: 6, vocative: 0},
    required_features: %{},
    evidence: [:be_up_to_idiom, :vocative_prefix]
  }

  # Mirrors production: base tokens "up"(5) and "to"(6) plus the virtual MWE
  # token that Core.MWE.LIFGBridge appends at index length(tokens) (= 7 here),
  # carrying the real character span (COLLAB §26.1).
  defp si(signatures) do
    sentence = "hey symbrella what are you up to?"
    {s, w} = :binary.match(sentence, "up to")

    %{
      sentence: sentence,
      tokens: [
        %{index: 5, phrase: "up", n: 1, mw: false, span: {s, s + 2}},
        %{index: 6, phrase: "to", n: 1, mw: false, span: {s + 3, s + w}},
        %{index: 7, token_index: 7, phrase: "up to", n: 2, mw: true, span: {s, s + w},
          source: :mwe_lexicon}
      ],
      sense_candidates: %{7 => [@willing, @doing, @until]},
      sentence_signatures: signatures
    }
  end

  defp choice(signatures) do
    {:ok, %{choices: choices}} =
      Stage1.run(si(signatures), scores: :all, preserve_sense_candidates: true,
        cerebellum_learn?: false)

    Enum.find(choices, &(&1.token_index == 7))
  end

  test "with the activity frame, the 'Doing, involved in' sense is preferred" do
    on = choice([@frame])
    off = choice([])

    assert on.chosen_id == "up to|preposition|7"
    assert on.scores["up to|preposition|7"] > off.scores["up to|preposition|7"]
  end

  test "without the frame, the activity sense gets no bonus over its rivals" do
    off = choice([])
    on = choice([@frame])

    # Non-activity senses never gain from the frame.
    assert on.scores["up to|adjective|1"] <= off.scores["up to|adjective|1"]
    assert on.scores["up to|preposition|5"] <= off.scores["up to|preposition|5"]
  end

  test "a frame for a different predicate token is inert" do
    other = put_in(@frame, [:roles, :predicate], 3)
    a = choice([other])
    b = choice([])

    assert {a.chosen_id, a.scores} == {b.chosen_id, b.scores}
  end
end
