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

    assert {:ok, %{choices: [choice], audit: audit}} =
             Stage1.run(si, scores: :all)

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

    assert {:ok, %{choices: choices, audit: audit}} =
             Stage1.run(si, scores: :all)

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

    assert {:ok, %{choices: [choice], audit: audit}} =
             Stage1.run(si, scores: :all)

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

  test "suppressed MWE spans are ignored as probes instead of counted as missing candidates" do
    si = %{
      sentence: "town and buy",
      tokens: [
        %{index: 0, phrase: "town and buy", n: 3, mw: true, span: {0, 12}}
      ],
      sense_candidates: %{}
    }

    assert {:ok, %{choices: [], audit: audit}} =
             Stage1.run(si, scores: :all, mwe_fallback: true)

    assert audit.missing_candidates == 0
    assert audit.ignored_mwe_probes == 1
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

    assert {:ok, %{choices: choices}} =
             Stage1.run(si, scores: :all)

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

  test "distinct numbered senses remain distinct before scoring" do
    cands = [
      %{
        id: "bank|noun|1",
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        activation: 0.5
      },
      %{
        id: "bank|noun|11",
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        activation: 0.5
      },
      %{
        id: "bank|noun|22",
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        activation: 0.5
      },
      %{
        id: "bank|verb|1",
        lemma: "bank",
        norm: "bank",
        pos: "verb",
        activation: 0.5
      }
    ]

    si = %{
      sentence: "bank",
      tokens: [
        %{
          index: 0,
          token_index: 0,
          phrase: "bank",
          n: 1,
          mw: false,
          span: {0, 4}
        }
      ],
      sense_candidates: %{0 => cands}
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(
               si,
               scores: :all,
               preserve_sense_candidates: true
             )

    assert map_size(choice.scores) == 4

    assert Map.has_key?(choice.scores, "bank|noun|1")
    assert Map.has_key?(choice.scores, "bank|noun|11")
    assert Map.has_key?(choice.scores, "bank|noun|22")
    assert Map.has_key?(choice.scores, "bank|verb|1")
  end

  test "duplicate representations of the same exact sense collapse before scoring" do
    cands = [
      %{
        id: "go|verb|3",
        lemma: "go",
        norm: "go",
        pos: "verb",
        source: :active_cells,
        activation: 0.4
      },
      %{
        id: "go|verb|3",
        lemma: "go",
        norm: "go",
        pos: "verb",
        source: :sense_candidates,
        activation: 0.8
      },
      %{
        id: "go|verb|4",
        lemma: "go",
        norm: "go",
        pos: "verb",
        source: :active_cells,
        activation: 0.5
      }
    ]

    si = %{
      sentence: "go",
      tokens: [
        %{
          index: 0,
          token_index: 0,
          phrase: "go",
          n: 1,
          mw: false,
          span: {0, 2}
        }
      ],
      sense_candidates: %{0 => cands}
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(
               si,
               scores: :all,
               preserve_sense_candidates: true
             )

    assert map_size(choice.scores) == 2

    assert Map.has_key?(choice.scores, "go|verb|3")
    assert Map.has_key?(choice.scores, "go|verb|4")
  end

  test "unigram backfill preserves exact-sense lexical evidence" do
    cell = %{
      id: "bank|noun|11",
      word: "bank",
      norm: "bank",
      pos: "noun",
      definition: "An edge of river, lake, or other watercourse.",
      example: "Tiber trembled underneath her banks.",
      synonyms: [],
      antonyms: [],
      semantic_atoms: [
        "ety:2",
        "cat:en:hydrology",
        "pos_raw:noun"
      ],
      gram_function: [],
      activation: 0.5
    }

    si = %{
      sentence: "bank",
      tokens: [
        %{
          index: 0,
          token_index: 0,
          phrase: "bank",
          n: 1,
          mw: false,
          span: {0, 4}
        }
      ],
      sense_candidates: %{},
      active_cells: [cell]
    }

    assert {:ok, %{si: out}} =
             Stage1.run(
               si,
               scores: :all,
               db_backfill?: false
             )

    [candidate] = out.sense_candidates[0]

    assert candidate.id == "bank|noun|11"
    assert candidate.pos == "noun"

    assert candidate.definition ==
             "An edge of river, lake, or other watercourse."

    assert candidate.example ==
             "Tiber trembled underneath her banks."

    assert candidate.synonyms == []
    assert candidate.antonyms == []
    assert candidate.gram_function == []

    assert "ety:2" in candidate.semantic_atoms
    assert "cat:en:hydrology" in candidate.semantic_atoms
    assert "pos_raw:noun" in candidate.semantic_atoms

    assert candidate.source == :active_cells
  end

  test "degraded LIFG pass forces pMTG decision past ACC threshold edge" do
    handler_id = "lifg-pmtg-degraded-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :lifg, :pmtg_decision],
        fn event, meas, meta, _cfg ->
          send(parent, {:telemetry, event, meas, meta})
        end,
        nil
      )

    on_exit(fn ->
      :telemetry.detach(handler_id)
    end)

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

  test "context_fit makes the river-bank sense win" do
    cands = [
      %{
        id: "bank|noun|11",
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        definition: "An edge of river, lake, or other watercourse.",
        example: "Tiber trembled underneath her banks.",
        synonyms: [],
        antonyms: [],
        semantic_atoms: ["ety:2", "cat:en:hydrology", "pos_raw:noun"],
        gram_function: [],
        activation: 0.5
      },
      %{
        id: "bank|noun|22",
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        definition: "A contiguous block of memory that is of fixed, hardware-dependent size.",
        example: nil,
        synonyms: [],
        antonyms: [],
        semantic_atoms: ["ety:3", "cat:en:computing", "pos_raw:noun"],
        gram_function: [],
        activation: 0.5
      },
      %{
        id: "bank|noun|23",
        lemma: "bank",
        norm: "bank",
        pos: "noun",
        definition: "(pinball) A set of multiple adjacent drop targets.",
        example: nil,
        synonyms: [],
        antonyms: [],
        semantic_atoms: ["ety:3", "cat:en:pinball", "qual:pinball", "pos_raw:noun"],
        gram_function: [],
        activation: 0.5
      }
    ]

    si = %{
      sentence: "I sat on the bank beside the river.",
      tokens: [
        %{
          index: 0,
          token_index: 0,
          phrase: "bank",
          n: 1,
          mw: false,
          span: {13, 17}
        }
      ],
      sense_candidates: %{0 => cands}
    }

    assert {:ok, %{choices: [choice]}} =
             Stage1.run(
               si,
               scores: :all,
               preserve_sense_candidates: true,
               weights: [
                 lex_fit: 0.0,
                 context_fit: 1.0,
                 rel_prior: 0.0,
                 activation: 0.0,
                 intent_bias: 0.0
               ]
             )

    assert choice.chosen_id == "bank|noun|11"

    assert choice.scores["bank|noun|11"] >
             choice.scores["bank|noun|22"]

    assert choice.scores["bank|noun|11"] >
             choice.scores["bank|noun|23"]
  end
end
