defmodule Core.Response.ResponseFrameTest do
  use ExUnit.Case, async: true

  alias Core.Response.LlmPrompt
  alias Core.Response.ResponseFrame

  @url "https://www.wikidata.org/wiki/Q473209"

  defp frame_context do
    symbolic = %{
      type: :location_question, relation: :population,
      subject: "Kamloops, BC", grounding_status: :unverified
    }

    %{
      user_text: "What is the population of Kamloops, BC?",
      symbolic_frame: %{event: symbolic},
      features: %{
        intent: :ask,
        symbolic_frame: symbolic,
        discourse_reference: %{antecedent: "Kamloops, BC", source: :recent_user_turn}
      },
      mood: %{tone_hint: :warm},
      decision: %{tone: :warm, mode: :collaborator, action: :answer},
      web_evidence: %{
        status: :retrieved,
        resolution: :source_supported,
        sources: [%{
          title: "Kamloops",
          url: @url,
          excerpt: "Population statement (2021): 97,902"
        }],
        support: %{
          kind: :population, status: :source_supported,
          subject: "Kamloops, BC", value: 97_902, census_year: 2021,
          source_type: :wikidata, source_url: @url
        }
      }
    }
  end

  test "prompt accepts partial versioned response frames without optional context keys" do
    partial = %{
      version: 1,
      move: :answer_grounded_question,
      evidence: %{
        kind: :population,
        subject: "Kamloops, BC",
        value: 97_902,
        year: 2021,
        source_urls: [@url]
      }
    }

    prompt = ResponseFrame.prompt(partial)

    assert prompt =~ "CURRENT CONVERSATIONAL FRAME"
    assert prompt =~ "intent=nil"
    assert prompt =~ "subject=nil"
    assert prompt =~ "count=97902"
    assert prompt =~ "display_count=97,902"
    assert prompt =~ "The evidence fixes the facts, not the exact sentence."

    assert ResponseFrame.prompt(%{version: 1}) =~ "move=nil"
    assert ResponseFrame.prompt(%{version: 1, posture: nil}) =~ "tone=nil"
  end

  test "optional source type is not required for a grounded population prompt" do
    frame = %{
      version: 1, move: :answer_grounded_question,
      evidence: %{
        kind: :population, subject: "Kamloops, BC",
        value: 97_902, year: 2021, source_urls: [@url]
      }
    }

    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "count=97902"
    assert prompt =~ "display_count=97,902"
    assert prompt =~ "origin=nil"
    assert prompt =~ @url
  end

  test "incomplete or mistyped claim fragments are not promoted to verified evidence" do
    for evidence <- [
          %{kind: :population, subject: "Kamloops, BC", value: 97_902},
          %{kind: :population, subject: "Kamloops, BC", value: "97,902", year: 2021,
            source_urls: [@url]},
          %{kind: :proximity, subject: "Maple Ridge, BC", target: "Kamloops, BC"},
          %{kind: :identity, subject: "Kamloops, BC", source_urls: [@url]}
        ] do
      prompt = ResponseFrame.prompt(%{version: 1, move: :answer_grounded_question,
        evidence: evidence})

      assert prompt =~ "verified_evidence=none"
      refute prompt =~ "verified_evidence=population;"
      refute prompt =~ "verified_evidence=straight_line_distance;"
      refute prompt =~ "verified_evidence=place_identity;"
    end
  end

  test "converts symbolic and verified data into a prompt frame, not fixed reply text" do
    context = frame_context()
    history = [
      %{"role" => "user", "content" => "What is Kamloops?"},
      %{"role" => "assistant", "content" => "It has 17,800 residents."}
    ]

    frame = ResponseFrame.build(context, history)

    assert frame.move == :answer_grounded_question
    assert frame.subject == "Kamloops, BC"
    assert frame.topic == :population
    assert frame.preceding_turn == :question
    assert frame.posture.tone == :warm
    assert frame.evidence == %{
      kind: :population,
      subject: "Kamloops, BC",
      value: 97_902,
      year: 2021,
      source_type: :wikidata,
      source_urls: [@url]
    }

    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "CURRENT CONVERSATIONAL FRAME"
    assert prompt =~ "count=97902"
    assert prompt =~ "display_count=97,902"
    assert prompt =~ "year=2021"
    assert prompt =~ @url
    assert prompt =~ "Express Symbrella's response in your own natural words"
    refute prompt =~ "17,800"
    refute prompt =~ "Wikidata lists a dated population statement"

    # The existing LLM prompt is actually given this turn-scoped state.
    rendered =
      context
      |> Map.put(:response_frame, frame)
      |> LlmPrompt.build_system_prompt()

    assert rendered =~ "CURRENT CONVERSATIONAL FRAME"
    assert rendered =~ "count=97902"
    assert rendered =~ "You are Symbrella."
    assert rendered =~ "WEB EVIDENCE"
  end

  test "recent correction changes conversational move without treating the old answer as evidence" do
    context =
      frame_context()
      |> Map.put(:user_text, "No, that's not recent enough. Look again.")
      |> Map.put(:symbolic_frame, %{event: %{
        type: :location_question, relation: :population,
        subject: "Kamloops, BC", freshness: :latest_available,
        grounding_status: :unverified
      }})
      |> Map.put(:features, %{intent: :ask})
      |> Map.delete(:web_evidence)

    frame = ResponseFrame.build(context, [
      %{"role" => "user", "content" => "Population?"},
      %{"role" => "assistant", "content" => "It is 17,800."}
    ])

    assert frame.move == :recheck_disputed_fact
    assert frame.subject == "Kamloops, BC"
    assert frame.evidence == nil
    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "Reassess it using CURRENT retrieved evidence"
    assert prompt =~ "verified_evidence=none"
    refute prompt =~ "17,800"
  end

  test "ordinary social conversation gets posture without geographic claims" do
    frame =
      ResponseFrame.build(%{
        user_text: "Good morning Symbrella",
        features: %{intent: :greet},
        decision: %{tone: :warm, mode: :chat},
        mood: %{tone_hint: :warm}
      })

    assert frame.move == :social_exchange
    assert frame.evidence == nil
    assert frame.posture.tone == :warm

    prompt = ResponseFrame.prompt(frame)
    assert prompt =~ "Respond socially and naturally"
    assert prompt =~ "continuing conversation"
    refute prompt =~ "count="
  end

  test "raw reference search results cannot become verified evidence" do
    context = %{frame_context() |
      web_evidence: %{
        status: :retrieved, resolution: :reference_sources,
        sources: [%{title: "Kamloops", excerpt: "17,800 people", url: @url}],
        support: nil
      }
    }

    assert ResponseFrame.build(context).evidence == nil
  end
end
