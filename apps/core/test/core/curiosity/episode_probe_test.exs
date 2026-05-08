defmodule Core.Curiosity.EpisodeProbeTest do
  use ExUnit.Case, async: false

  alias Core.Curiosity.EpisodeProbe

  setup do
    old = Application.get_env(:core, EpisodeProbe)
    EpisodeProbe.reset()

    on_exit(fn ->
      EpisodeProbe.reset()

      if is_nil(old) do
        Application.delete_env(:core, EpisodeProbe)
      else
        Application.put_env(:core, EpisodeProbe, old)
      end
    end)

    :ok
  end

  test "asks one clarification question for a recent uncertain episode" do
    si = %{
      text: "what should we do next",
      session_id: "curiosity-test",
      evidence: %{
        episodes: [
          %{
            score: 0.22,
            at: 123,
            episode: %{
              slate: %{
                si: %{sentence: "a clock that told stories"},
                winners: [%{lemma: "clock"}, %{lemma: "stories"}]
              },
              meta: %{uncertainty: 0.72}
            }
          }
        ]
      }
    }

    assert {:ok, question, meta} =
             EpisodeProbe.maybe_question(si, calm_mood(),
               every_turns: 1,
               min_gap_ms: 0,
               min_uncertainty: 0.35
             )

    assert question =~ "Curiosity check:"
    assert question =~ "a clock that told stories"
    assert question =~ "feature idea"
    assert meta.reason == :explicit_uncertainty
    assert meta.uncertainty == 0.72
  end

  test "does not repeat the same episode signature after asking" do
    si = uncertain_si("repeat-session")
    opts = [every_turns: 1, min_gap_ms: 0, min_uncertainty: 0.35]

    assert {:ok, _question, _meta} = EpisodeProbe.maybe_question(si, calm_mood(), opts)
    assert :none = EpisodeProbe.maybe_question(si, calm_mood(), opts)
  end

  test "does not ask during emergency pressure" do
    si =
      uncertain_si("unsafe-session")
      |> Map.put(:text, "this is an emergency and everything is failing")

    mood = %{mood: %{vigilance: 0.82, inhibition: 0.4, exploration: 0.3, plasticity: 0.4}}

    assert :none =
             EpisodeProbe.maybe_question(si, mood,
               every_turns: 1,
               min_gap_ms: 0,
               min_uncertainty: 0.35
             )
  end

  test "default cadence can ask on the second eligible turn" do
    Application.delete_env(:core, EpisodeProbe)

    si = uncertain_si("default-cadence-session")

    assert :none = EpisodeProbe.maybe_question(si, calm_mood(), min_gap_ms: 0)

    assert {:ok, question, meta} =
             EpisodeProbe.maybe_question(si, calm_mood(), min_gap_ms: 0)

    assert question =~ "Curiosity check:"
    assert meta.topic == "the clock telling stories"
  end

  test "recovers a sentence from older DB-shaped slate tokens" do
    si = %{
      text: "continue",
      session_id: "db-shaped-session",
      evidence: %{
        episodes: [
          %{
            score: 0.2,
            at: 789,
            episode: %{
              slate: %{
                si: %{
                  "meta" => %{},
                  "slate" => %{
                    "tokens" => [
                      %{"n" => 3, "phrase" => "i need to", "span" => [0, 9]},
                      %{"n" => 1, "phrase" => "i", "span" => [0, 1]},
                      %{"n" => 1, "phrase" => "need", "span" => [2, 6]},
                      %{"n" => 1, "phrase" => "to", "span" => [7, 9]},
                      %{"n" => 1, "phrase" => "figure", "span" => [10, 16]},
                      %{"n" => 1, "phrase" => "out", "span" => [17, 20]},
                      %{"n" => 1, "phrase" => "what", "span" => [21, 25]},
                      %{"n" => 1, "phrase" => "im", "span" => [26, 28]},
                      %{"n" => 1, "phrase" => "doing", "span" => [29, 34]}
                    ],
                    "winners" => [
                      %{"id" => "figure out|phrase|fallback", "lemma" => "figure out"}
                    ]
                  }
                }
              },
              meta: %{confidence: 0.2}
            }
          }
        ]
      }
    }

    assert {:ok, question, meta} =
             EpisodeProbe.maybe_question(si, calm_mood(),
               every_turns: 1,
               min_gap_ms: 0,
               min_uncertainty: 0.35
             )

    assert question =~ "i need to figure out what im doing"
    assert meta.topic == "i need to figure out what im doing"
  end

  test "does not ask about the current question-shaped utterance" do
    si = %{
      text: "where do i live",
      session_id: "current-question-session",
      evidence: %{
        episodes: [
          %{
            score: 0.1,
            at: 999,
            episode: %{
              slate: %{sentence: "where do i live", winners: []},
              meta: %{confidence: 0.1}
            }
          }
        ]
      }
    }

    assert :none =
             EpisodeProbe.maybe_question(si, calm_mood(),
               every_turns: 1,
               min_gap_ms: 0,
               min_uncertainty: 0.2
             )
  end

  test "does not ask clarification questions about stored user facts" do
    si = %{
      text: "continue",
      session_id: "fact-memory-session",
      evidence: %{
        episodes: [
          %{
            score: 0.1,
            at: 1000,
            episode: %{
              slate: %{sentence: "i live in Richmond, BC", tags: ["fact", "user_fact"]},
              meta: %{kind: :fact, key: "location", value: "Richmond, BC", tags: ["fact"]}
            }
          }
        ]
      }
    }

    assert :none =
             EpisodeProbe.maybe_question(si, calm_mood(),
               every_turns: 1,
               min_gap_ms: 0,
               min_uncertainty: 0.2
             )
  end

  test "idle_question asks about a recent unknown-intent episode" do
    episodes = [
      %{
        id: 42,
        sentence: "the blue bracket thing",
        intent: "unknown",
        confidence: 0.2,
        tokens: ["blue", "bracket", "thing"],
        tags: [],
        inserted_at: ~N[2026-05-08 12:00:00]
      }
    ]

    assert {:ok, question, meta} =
             EpisodeProbe.idle_question("idle-unknown-session",
               episodes: episodes,
               min_uncertainty: 0.2
             )

    assert question =~ "I found an earlier message"
    assert question =~ "the blue bracket thing"
    assert question =~ "What did you mean by that?"
    assert meta.topic == "the blue bracket thing"
    assert meta.reason == :idle_unknown_intent
  end

  test "idle_question ignores known-intent episodes" do
    episodes = [
      %{
        id: 43,
        sentence: "my location is Richmond",
        intent: "statement",
        confidence: 0.9,
        inserted_at: ~N[2026-05-08 12:00:00]
      }
    ]

    assert :none =
             EpisodeProbe.idle_question("idle-known-session",
               episodes: episodes,
               min_uncertainty: 0.2
             )
  end

  defp uncertain_si(session_id) do
    %{
      text: "continue",
      session_id: session_id,
      evidence: %{
        episodes: [
          %{
            score: 0.3,
            at: 456,
            episode: %{
              slate: %{si: %{sentence: "the clock telling stories"}, winners: []},
              meta: %{confidence: 0.2}
            }
          }
        ]
      }
    }
  end

  defp calm_mood do
    %{mood: %{vigilance: 0.4, inhibition: 0.6, exploration: 0.45, plasticity: 0.45}}
  end
end
