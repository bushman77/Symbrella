defmodule Core.MWE.DecisionsTest do
  use ExUnit.Case, async: false

  alias Core.MWE.Decisions
  alias Core.Comprehension.Summary

  defp input(words \\ ~w(i need to turn off the lights)) do
    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, index} -> %{index: index, phrase: word, n: 1} end)

    word_choices =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, index} ->
        %{token_index: index, id: "#{word}|noun|0", lemma: word, score: 0.9}
      end)

    %Core.SemanticInput{
      sentence: Enum.join(words, " "),
      tokens: tokens,
      lifg_choices: word_choices,
      intent: :command,
      keyword: "lights",
      confidence: 0.9,
      trace: []
    }
  end

  defp phrase_choice(first, last, virtual_index \\ 7) do
    %{
      token_index: virtual_index,
      token_span: {first, last},
      phrase: "turn off",
      span: {10, 8},
      id: "turn off|verb|2",
      lemma: "turn off",
      pos: "verb",
      source: :mwe_lexicon,
      score: 0.92
    }
  end

  test "lexical verb is consumed by comprehension and ATL without changing words" do
    si = input()
    original = %{si | mwe_choices: [phrase_choice(3, 4)]}
    result = Decisions.attach(original)

    assert result.tokens == original.tokens
    assert result.lifg_choices == original.lifg_choices
    assert result.mwe_choices == original.mwe_choices
    assert Enum.map(result.semantic_choices, & &1.token_index) == [0, 1, 2, 3, 5, 6]

    assert Enum.any?(result.semantic_choices, fn ch ->
             ch.id == "turn off|verb|2" and ch.pos == "verb" and ch.kind == :mwe and
               ch.token_span == {3, 4} and ch.token_index == 3
           end)

    refute Enum.any?(result.semantic_choices, &(&1.id == "turn|noun|0"))
    refute Enum.any?(result.semantic_choices, &(&1.id == "off|noun|0"))

    summary = Summary.build(result)
    assert "turn off" in summary.understood
    assert summary.unresolved == []

    with_atl = Core.Brain.ATL.ingest(result, [])
    assert Enum.any?(with_atl.atl_slate.by_norm["turn off"], &(&1.id == "turn off|verb|2"))
    refute Map.has_key?(with_atl.atl_slate.by_norm, "turn")
    refute Map.has_key?(with_atl.atl_slate.by_norm, "off")
  end

  test "a fallback, incorrect lemma, stale span or wrong source cannot shadow words" do
    si = input()
    real = phrase_choice(3, 4)

    for invalid <- [
          %{real | id: "turn off|phrase|fallback"},
          %{real | id: "turn on|verb|1"},
          %{real | source: :phrase_backfill},
          %{real | token_span: {4, 5}},
          %{real | token_span: {3, 200}}
        ] do
      result = Decisions.attach(%{si | mwe_choices: [invalid]})
      assert result.semantic_choices == si.lifg_choices
    end
  end

  test "dictionary verb cannot shadow noun object within enclosing particle construction" do
    si = input(~w(turn the lights off))

    signature = %{
      id: :verb_particle_construction,
      span: {0, 3},
      roles: %{verb: 0, determiner: 1, object: 2, particle: 3}
    }

    false_phrase = %{
      token_index: 4,
      token_span: {2, 3},
      phrase: "lights off",
      id: "lights off|verb|1",
      lemma: "lights off",
      pos: "verb",
      source: :mwe_lexicon,
      score: 0.99
    }

    si = %{si | sentence_signatures: [signature], mwe_choices: [false_phrase]}
    out = Decisions.attach(si)

    assert out.semantic_choices == si.lifg_choices
    assert out.lifg_choices == si.lifg_choices
    assert out.mwe_choices == si.mwe_choices
  end

  test "two occurrences yield separate phrase choices" do
    si = input(~w(turn off and turn off))
    result = Decisions.attach(%{si | mwe_choices: [phrase_choice(0, 1, 5), phrase_choice(3, 4, 6)]})

    phrases = Enum.filter(result.semantic_choices, &(Map.get(&1, :kind) == :mwe))
    assert Enum.map(phrases, & &1.token_span) == [{0, 1}, {3, 4}]
    assert result.lifg_choices == si.lifg_choices
  end

  test "longer overlapping phrase wins while original LIFG decisions remain intact" do
    si = input()
    longer =
      phrase_choice(3, 5, 8)
      |> Map.merge(%{phrase: "turn off the", id: "turn off the|verb|1", lemma: "turn off the"})

    result = Decisions.attach(%{si | mwe_choices: [phrase_choice(3, 4), longer]})

    assert Enum.map(Enum.filter(result.semantic_choices, &(Map.get(&1, :kind) == :mwe)),
             & &1.id) == ["turn off the|verb|1"]
    assert result.lifg_choices == si.lifg_choices
  end

  test "promotion audit characterizes a phrase winner despite unresolved sentence grammar" do
    si = input(~w(what do you know about japan's railgun development))

    phrase = %{
      token_index: 8,
      token_span: {0, 3},
      phrase: "what do you know",
      id: "what do you know|phrase|2",
      lemma: "what do you know",
      pos: "phrase",
      source: :mwe_lexicon,
      score: 0.529
    }

    signature = %{
      id: :verb_particle_construction, span: {1, 3},
      roles: %{verb: 1, object: 2, particle: 3}
    }

    si = %{si |
      sentence_signatures: [signature],
      mwe_choices: [phrase],
      lifg_opts: [trace_mwe_promotion?: true]
    }

    first = Decisions.attach(si)

    assert Enum.any?(first.semantic_choices, fn choice ->
             choice.id == phrase.id and choice.token_span == {0, 3}
           end)

    refute Enum.any?(first.semantic_choices, fn choice ->
             choice.token_index in 0..3 and Map.get(choice, :kind) != :mwe
           end)

    assert %{
             stage: :mwe_promotion_audit,
             meta: %{pass: 1, candidates: [%{
               id: "what do you know|phrase|2",
               lexical_gate_passed?: true,
               question_conflict?: false,
               verb_frame_conflict?: false,
               promoted?: true
             }]}
           } = hd(first.trace)

    # The existing reranker considers verb senses, not the selected phrase
    # POS. It does not negate the previously promoted canonical choice.
    after_rerank = Core.MWE.SenseResolution.rerank_contiguous(first)
    assert after_rerank.semantic_choices == first.semantic_choices

    # A downstream ATL mismatch is only observational: rerunning the same
    # canonical promotion gate does not consume or respond to it.
    after_atl =
      %{after_rerank | atl_slate: %{
        constituents: [
          %{id: :verb_particle_construction, status: :unresolved,
            span: {1, 3}, unresolved_roles: [:verb, :object, :particle]}
        ]
      }}
      |> Decisions.attach()

    assert after_atl.semantic_choices == first.semantic_choices
    assert %{
             stage: :mwe_promotion_audit,
             meta: %{pass: 3, candidates: [%{promoted?: true}]}
           } = hd(after_atl.trace)
  end

  test "diagnostic tracing is disabled unless requested and cannot alter winners" do
    si = input(~w(what do you know about japan's railgun development))

    phrase = %{
      token_index: 8, token_span: {0, 3}, phrase: "what do you know",
      id: "what do you know|phrase|2", lemma: "what do you know",
      pos: "phrase", source: :mwe_lexicon, score: 0.529
    }

    without = Decisions.attach(%{si | mwe_choices: [phrase]})
    with_trace = Decisions.attach(%{si |
      mwe_choices: [phrase], lifg_opts: [trace_mwe_promotion?: true]
    })

    assert without.semantic_choices == with_trace.semantic_choices
    refute Enum.any?(without.trace, fn
             %{stage: :mwe_promotion_audit} -> true
             _ -> false
           end)
    assert hd(with_trace.trace).stage == :mwe_promotion_audit
  end

  test "no lexical phrase means canonical view matches existing word decisions" do
    si = input()
    assert Decisions.attach(si).semantic_choices == si.lifg_choices
  end
end
