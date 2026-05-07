defmodule Brain.LIFG.Stage1ReliabilityTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.Stage1

  test "rejects stale candidates that do not match the token phrase" do
    si = %{
      sentence: "hey",
      tokens: [
        %{index: 0, phrase: "hey", n: 1, mw: false, span: {0, 3}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "morning|noun|0", lemma: "morning", pos: "noun", score: 0.99},
          %{id: "hey|interjection|0", lemma: "hey", pos: "interjection", score: 0.80}
        ]
      }
    }

    assert {:ok, %{choices: [choice], audit: audit}} = Stage1.run(si, scores: :all)

    assert choice.chosen_id == "hey|interjection|0"
    refute Map.has_key?(choice.scores, "morning|noun|0")
    assert audit.missing_candidates == 0
  end

  test "closed-class words prefer their grammatical function over noun or verb defaults" do
    si = %{
      sentence: "and really in about what do",
      tokens: [
        %{index: 0, phrase: "and", n: 1, mw: false, span: {0, 3}},
        %{index: 1, phrase: "really", n: 1, mw: false, span: {4, 10}},
        %{index: 2, phrase: "in", n: 1, mw: false, span: {11, 13}},
        %{index: 3, phrase: "about", n: 1, mw: false, span: {14, 19}},
        %{index: 4, phrase: "what", n: 1, mw: false, span: {20, 24}},
        %{index: 5, phrase: "do", n: 1, mw: false, span: {25, 27}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "and|noun|0", pos: "noun", norm: "and", activation: 0.9},
          %{id: "and|verb|0", pos: "verb", norm: "and", activation: 0.9}
        ],
        1 => [
          %{id: "really|verb|0", pos: "verb", norm: "really", activation: 0.9}
        ],
        2 => [
          %{id: "in|noun|0", pos: "noun", norm: "in", activation: 0.9},
          %{id: "in|verb|0", pos: "verb", norm: "in", activation: 0.9}
        ],
        3 => [
          %{id: "about|verb|0", pos: "verb", norm: "about", activation: 0.9}
        ],
        4 => [
          %{id: "what|noun|0", pos: "noun", norm: "what", activation: 0.9},
          %{id: "what|particle|0", pos: "particle", norm: "what", activation: 0.9}
        ],
        5 => [
          %{id: "do|noun|0", pos: "noun", norm: "do", activation: 0.9},
          %{id: "do|verb|0", pos: "verb", norm: "do", activation: 0.9}
        ]
      }
    }

    assert {:ok, %{choices: choices, audit: audit}} = Stage1.run(si, scores: :all)

    by_index = Map.new(choices, &{&1.token_index, &1})

    assert by_index[0].chosen_id == "and|conjunction|0"
    assert by_index[1].chosen_id == "really|adverb|0"
    assert by_index[2].chosen_id == "in|preposition|0"
    assert by_index[3].chosen_id == "about|preposition|0"
    assert by_index[4].chosen_id == "what|pronoun|0"
    assert by_index[5].chosen_id == "do|auxiliary|0"
    assert audit.weak_decisions == 0
    assert audit.low_confidence_decisions == 6
    assert audit.pos_anomalies == 0
  end

  test "greeting tokens prefer interjection over noun or verb defaults" do
    si = %{
      sentence: "hey",
      tokens: [
        %{index: 0, phrase: "hey", n: 1, mw: false, span: {0, 3}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "hey|noun|0", pos: "noun", norm: "hey", activation: 0.9},
          %{id: "hey|verb|0", pos: "verb", norm: "hey", activation: 0.9}
        ]
      }
    }

    assert {:ok, %{choices: [choice], audit: audit}} = Stage1.run(si, scores: :all)

    assert choice.chosen_id == "hey|interjection|0"
    assert choice.margin > 0.15
    assert audit.pos_anomalies == 0
  end

  test "generated MWE fallbacks are low-confidence and mark the pass degraded" do
    si = %{
      sentence: "really bad drugs",
      tokens: [
        %{index: 0, phrase: "really bad drugs", n: 3, mw: true, span: {0, 16}}
      ],
      sense_candidates: %{}
    }

    assert {:ok, %{choices: [choice], audit: audit}} =
             Stage1.run(si, scores: :all, mwe_fallback: true)

    assert choice.chosen_id == "really bad drugs|phrase|fallback"
    assert choice.score <= 0.35
    assert choice.margin == 0.0
    assert choice.weak?
    assert choice.margin_weak?
    assert choice.low_confidence?
    assert audit.degraded?
    assert :fallback_winner_rate_high in audit.degraded_reasons
    assert :low_confidence_rate_high in audit.degraded_reasons
  end

  test "suppressed MWE spans count as missing candidates but not fallback emissions" do
    si = %{
      sentence: "town and buy",
      tokens: [
        %{index: 0, phrase: "town and buy", n: 3, mw: true, span: {0, 12}}
      ],
      sense_candidates: %{}
    }

    assert {:ok, %{choices: [], audit: audit}} =
             Stage1.run(si, scores: :all, mwe_fallback: true)

    assert audit.missing_candidates == 1
    assert audit.mwe_fallbacks == 0
    assert audit.fallback_winners == 0
    assert audit.rates.fallback == 0.0
    assert audit.rates.mwe_fallback_emit == 0.0
    refute :fallback_winner_rate_high in audit.degraded_reasons
  end

  test "neighbor POS context pushes content words into syntactic lane" do
    si = %{
      sentence: "of events in the world",
      tokens: [
        %{index: 0, phrase: "of", n: 1, mw: false, span: {0, 2}},
        %{index: 1, phrase: "events", n: 1, mw: false, span: {3, 9}},
        %{index: 2, phrase: "in", n: 1, mw: false, span: {10, 12}},
        %{index: 3, phrase: "the", n: 1, mw: false, span: {13, 16}},
        %{index: 4, phrase: "world", n: 1, mw: false, span: {17, 22}}
      ],
      sense_candidates: %{
        0 => [
          %{id: "of|verb|0", pos: "verb", norm: "of", activation: 0.9},
          %{id: "OF|noun|0", pos: "noun", norm: "of", activation: 0.9}
        ],
        1 => [
          %{id: "events|noun|0", pos: "noun", norm: "events", activation: 0.5},
          %{id: "events|verb|0", pos: "verb", norm: "events", activation: 0.5}
        ],
        2 => [
          %{id: "in|verb|0", pos: "verb", norm: "in", activation: 0.9},
          %{id: "in|noun|0", pos: "noun", norm: "in", activation: 0.9}
        ],
        4 => [
          %{id: "world|noun|14", pos: "noun", norm: "world", activation: 0.5},
          %{id: "world|verb|0", pos: "verb", norm: "world", activation: 0.5}
        ]
      }
    }

    assert {:ok, %{choices: choices}} = Stage1.run(si, scores: :all)

    by_index = Map.new(choices, &{&1.token_index, &1})

    assert by_index[0].chosen_id == "of|preposition|0"
    assert by_index[1].chosen_id == "events|noun|0"
    assert by_index[2].chosen_id == "in|preposition|0"
    assert by_index[3].chosen_id == "the|determiner|0"
    assert by_index[4].chosen_id == "world|noun|14"

    assert by_index[1].margin > 0.15
    assert by_index[3].reliability == :deterministic_closed_class
    refute by_index[3].weak?
    assert by_index[4].margin > 0.15
  end

  test "duplicate sense ids collapse to a single lemma/pos family before scoring" do
    cands =
      for n <- 0..8 do
        %{id: "go|verb|#{n}", lemma: "go", pos: "verb", activation: 0.5}
      end

    si = %{
      sentence: "go",
      tokens: [
        %{index: 0, phrase: "go", n: 1, mw: false, span: {0, 2}}
      ],
      sense_candidates: %{0 => cands}
    }

    assert {:ok, %{choices: [choice]}} = Stage1.run(si, scores: :all)

    assert map_size(choice.scores) == 1
    assert choice.margin == 0.0
    assert choice.weak?
  end

  test "full LIFG output does not leak duplicate numbered variants back into alt ids" do
    cands =
      [%{id: "go|noun|0", lemma: "go", pos: "noun", activation: 0.4}] ++
        for n <- 0..8 do
          %{id: "go|verb|#{n}", lemma: "go", pos: "verb", activation: 0.5}
        end

    si = %{
      sentence: "go",
      tokens: [
        %{index: 0, phrase: "go", n: 1, mw: false, span: {0, 2}}
      ],
      sense_candidates: %{0 => cands}
    }

    assert {:ok, %{choices: [choice]}} = Brain.LIFG.run(si, pmtg_apply?: false)

    refute Enum.any?(choice.alt_ids, &String.match?(&1, ~r/^go\|verb\|\d+$/))
    assert length(Enum.filter(choice.alt_ids, &String.starts_with?(&1, "go|noun|"))) <= 1
    assert length(Enum.filter(choice.slate_alt_ids, &String.starts_with?(&1, "go|verb|"))) <= 1
    assert length(Enum.filter(choice.slate_alt_ids, &String.starts_with?(&1, "go|noun|"))) <= 1
  end

  test "degraded LIFG pass forces pMTG decision past ACC threshold edge" do
    handler_id = "lifg-pmtg-degraded-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :lifg, :pmtg_decision],
        fn event, meas, meta, _cfg -> send(parent, {:telemetry, event, meas, meta}) end,
        nil
      )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    si = %{
      sentence: "really bad drugs",
      tokens: [
        %{index: 0, phrase: "really bad drugs", n: 3, mw: true, span: {0, 16}}
      ],
      sense_candidates: %{}
    }

    assert {:ok, _out} =
             Brain.LIFG.run(
               si,
               pmtg_mode: :boost,
               acc_conflict_tau: 1.0,
               mwe_fallback: true
             )

    assert_receive {:telemetry, [:brain, :lifg, :pmtg_decision], %{needy: needy}, meta}, 500
    assert needy >= 1
    assert meta.apply?
    assert meta.force?
    assert meta.degraded?
  end
end
