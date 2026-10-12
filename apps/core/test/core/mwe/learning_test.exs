defmodule Core.MWE.LearningTest do
  use ExUnit.Case, async: true

  alias Core.MWE.Learning

  defp make_input(words, spans, category \\ :phrasal_verb) do
    tokens =
      words
      |> Enum.with_index()
      |> Enum.map(fn {word, index} ->
        %{index: index, n: 1, phrase: word}
      end)

    hypotheses =
      Enum.map(spans, fn span ->
        %{
          phrase: "phase out",
          span: span,
          category: category,
          source: :mwe_discovery,
          status: :unverified,
          verified?: false
        }
      end)

    %Core.SemanticInput{
      sentence: Enum.join(words, " "),
      tokens: tokens,
      mwe_hypotheses: hypotheses,
      trace: []
    }
  end

  test "an unknown phrase accumulates diverse contexts before review, never verification" do
    server = start_supervised!({Learning, name: nil})
    inputs = [
      make_input(~w(we phase out systems), [{1, 2}]),
      make_input(~w(they phase out products), [{1, 2}]),
      make_input(~w(i phase out machines), [{1, 2}])
    ]

    [a, b, c] = Enum.map(inputs, &Learning.observe(&1, mwe_learning_server: server))

    assert hd(a.mwe_hypotheses).observation_count == 1
    assert hd(b.mwe_hypotheses).observation_count == 2

    assert [%{
      observation_count: 3,
      distinct_contexts: 3,
      review_status: :candidate_for_review,
      status: :unverified,
      verified?: false
    }] = c.mwe_hypotheses

    assert c.mwe_lexicon == []
    assert c.sense_candidates == %{}
    assert Enum.any?(c.trace, &(&1.stage == :mwe_learning))
  end

  test "repeating the identical context is not enough for review" do
    server = start_supervised!({Learning, name: nil})
    turn = make_input(~w(we phase out systems), [{1, 2}])

    last =
      Enum.reduce(1..5, turn, fn _idx, _ ->
        Learning.observe(turn, mwe_learning_server: server)
      end)

    assert [%{
      observation_count: 5,
      distinct_contexts: 1,
      review_status: :collecting
    }] = last.mwe_hypotheses
  end

  test "two occurrences in one turn are one observation" do
    server = start_supervised!({Learning, name: nil})
    si = make_input(~w(phase out and phase out), [{0, 1}, {3, 4}])
    result = Learning.observe(si, mwe_learning_server: server)

    assert Enum.map(result.mwe_hypotheses, & &1.observation_count) == [1, 1]
    assert Learning.snapshot(server).phrase_count == 1
  end

  test "noun compounds require at least five distinct turns and two contexts" do
    server = start_supervised!({Learning, name: nil})

    final =
      Enum.reduce(1..5, nil, fn idx, _ ->
        subject = "speaker#{idx}"
        si = make_input([subject, "phase", "out", "today"], [{1, 2}], :compound_noun)
        Learning.observe(si, mwe_learning_server: server)
      end)

    assert [%{review_status: :candidate_for_review, observation_count: 5}] =
             final.mwe_hypotheses
  end

  test "existing dictionary senses never contribute to tentative learning" do
    server = start_supervised!({Learning, name: nil})
    si = make_input(~w(we phase out products), [{1, 2}])

    confirmed =
      %{si | mwe_lexicon: [
        %{phrase: "phase out", senses: [%{id: "phase out|verb|1", pos: "verb"}]}
      ]}
      |> Learning.observe(mwe_learning_server: server)

    assert confirmed.mwe_lexicon == si.mwe_lexicon ++ [
      %{phrase: "phase out", senses: [%{id: "phase out|verb|1", pos: "verb"}]}
    ]
    assert Learning.snapshot(server).phrase_count == 0
  end

  test "context statistics do not cross session boundaries" do
    server = start_supervised!({Learning, name: nil})
    first = %{make_input(~w(we phase out systems), [{1, 2}]) | session_id: "alpha"}
    second = %{make_input(~w(they phase out products), [{1, 2}]) | session_id: "beta"}

    alpha = Learning.observe(first, mwe_learning_server: server)
    beta = Learning.observe(second, mwe_learning_server: server)

    assert hd(alpha.mwe_hypotheses).observation_count == 1
    assert hd(beta.mwe_hypotheses).observation_count == 1
    assert Learning.snapshot(server).phrase_count == 2
  end

  test "database confirmation supersedes a previously observed hypothesis" do
    server = start_supervised!({Learning, name: nil})
    si = make_input(~w(we phase out systems), [{1, 2}])
    observed = Learning.observe(si, mwe_learning_server: server)
    assert hd(observed.mwe_hypotheses).observation_count == 1

    known = %{si | mwe_lexicon: [
      %{phrase: "phase out", senses: [%{id: "phase out|verb|1", pos: "verb"}]}
    ]}

    now_known = Learning.observe(known, mwe_learning_server: server)

    refute Map.has_key?(hd(now_known.mwe_hypotheses), :review_status)
    refute Map.has_key?(hd(now_known.mwe_hypotheses), :observation_count)
    assert now_known.mwe_lexicon == known.mwe_lexicon
    assert Learning.snapshot(server).phrase_count == 1
  end

  test "disabled learning preserves the complete SI and reset clears counters" do
    server = start_supervised!({Learning, name: nil})
    si = make_input(~w(we phase out systems), [{1, 2}])

    assert Learning.observe(si, mwe_learning: :off, mwe_learning_server: server) == si

    _ = Learning.observe(si, mwe_learning_server: server)
    assert Learning.snapshot(server).phrase_count == 1
    assert :ok = Learning.reset(server)
    assert Learning.snapshot(server).phrase_count == 0
  end

  test "observer uses a bounded number of phrase records" do
    server = start_supervised!({Learning, name: nil, keep: 2})

    for n <- 1..3 do
      words = ["we", "phase", "out", "name#{n}"]
      si = make_input(words, [{1, 2}])
      si = %{si | mwe_hypotheses: [
        %{hd(si.mwe_hypotheses) | phrase: "phase out #{n}"}
      ]}
      Learning.observe(si, mwe_learning_server: server)
    end

    assert Learning.snapshot(server).phrase_count == 2
  end
end
