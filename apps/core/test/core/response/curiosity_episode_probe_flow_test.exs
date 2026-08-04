defmodule Core.Response.CuriosityEpisodeProbeFlowTest do
  use ExUnit.Case, async: false

  alias Core.Curiosity.EpisodeProbe
  alias Core.Response

  @mood %{
    mood: %{vigilance: 0.4, inhibition: 0.6, exploration: 0.45, plasticity: 0.45},
    tone_hint: nil
  }

  setup do
    old = Application.get_env(:core, EpisodeProbe)

    Application.put_env(:core, EpisodeProbe,
      enabled?: true,
      every_turns: 1,
      min_gap_ms: 0,
      max_vigilance: 0.75,
      min_uncertainty: 0.35,
      max_recent: 10
    )

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

  test "response planner does not append hippocampal clarification questions inline" do
    si = %{
      intent: :question,
      confidence: 0.7,
      text: "what should we work on next",
      session_id: "response-curiosity-test",
      evidence: %{
        episodes: [
          %{
            score: 0.25,
            at: 789,
            episode: %{
              slate: %{si: %{sentence: "a clock that told stories"}, winners: []},
              meta: %{uncertainty: 0.68}
            }
          }
        ]
      }
    }

    {_tone, text, meta} = Response.plan(si, @mood)

    refute text =~ "Curiosity check:"
    assert is_nil(meta.curiosity_probe)
  end

  test "response planner does not append curiosity during guardrail pressure" do
    si = %{
      intent: :help,
      confidence: 0.7,
      text: "im going to hurt myself",
      session_id: "response-curiosity-guardrail-test",
      evidence: %{
        episodes: [
          %{
            score: 0.25,
            at: 790,
            episode: %{
              slate: %{si: %{sentence: "a clock that told stories"}, winners: []},
              meta: %{uncertainty: 0.9}
            }
          }
        ]
      }
    }

    {_tone, text, meta} = Response.plan(si, @mood)

    refute text =~ "Curiosity check:"
    assert is_nil(meta.curiosity_probe)
  end

  test "response planner keeps casual chat separate from idle curiosity" do
    si = %{
      intent: :unknown,
      confidence: 0.2,
      text: "that was interesting hahaha",
      session_id: "response-curiosity-casual-test",
      evidence: %{
        episodes: [
          %{
            score: 0.2,
            at: 791,
            episode: %{
              slate: %{si: %{sentence: "a clock that told stories"}, winners: []},
              meta: %{uncertainty: 0.66}
            }
          }
        ]
      }
    }

    {_tone, text, meta} = Response.plan(si, @mood)

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "Yeah, that was an interesting one."
    refute text =~ "Curiosity check:"
    assert is_nil(meta.curiosity_probe)
  end
end
