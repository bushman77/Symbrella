defmodule Core.Response.ReflectionLoopTest do
  use ExUnit.Case, async: false

  alias Core.Response.ReflectionLoop

  test "rejects a news retry prompt echo even if its source metadata matches" do
    day = Date.to_iso8601(Date.utc_today())
    url = "https://news.google.com/rss/articles/known-report-1"
    headline = "Diplomats discuss Strait of Hormuz proposal"

    context = %{
      features: %{intent: :ask, conf: 0.9},
      web_evidence: %{
        relation: :current_events,
        status: :retrieved,
        resolution: :headlines_available,
        sources: [
          %{
            provider: :news_rss,
            publisher: "Example Wire",
            title: headline,
            published_at: DateTime.to_iso8601(DateTime.utc_now()),
            url: url
          }
        ]
      }
    }

    question = "What are your thoughts on the US Iran war currently happening?"

    draft =
      "ORIGINAL QUESTION: #{question} FAILED CHECKS: news_evidence_not_cited " <>
        "[1] PUBLISHER: Example Wire; DATE: #{day}; HEADLINE: #{headline}; " <>
        "LINK: #{url} My assessment: de-escalation is important."

    {:ok, answer, reflection} = ReflectionLoop.review(question, draft, context)

    assert reflection.status == :repair
    assert :revision_prompt_echo in reflection.issues
    assert answer =~ "Recent dated headlines"
    assert answer =~ "Example Wire"
    assert answer =~ url
    refute answer =~ "ORIGINAL QUESTION:"
    refute answer =~ "FAILED CHECKS:"
  end

  test "ordinary attributed news assessment is not misclassified as a retry prompt echo" do
    day = Date.to_iso8601(Date.utc_today())
    url = "https://news.google.com/rss/articles/known-report-1"
    headline = "Diplomats discuss Strait of Hormuz proposal"

    context = %{
      features: %{intent: :ask, conf: 0.9},
      web_evidence: %{
        relation: :current_events,
        status: :retrieved,
        resolution: :headlines_available,
        sources: [
          %{
            provider: :news_rss,
            publisher: "Example Wire",
            title: headline,
            published_at: DateTime.to_iso8601(DateTime.utc_now()),
            url: url
          }
        ]
      }
    }

    answer =
      "Example Wire reported on #{day}: #{headline}. Source: #{url} " <>
        "My assessment: this headline alone cannot establish whether diplomacy will work."

    assert {:ok, ^answer, reflection} =
             ReflectionLoop.review(
               "What are your thoughts on the US Iran war currently happening?",
               answer,
               context
             )
    refute :revision_prompt_echo in reflection.issues
    assert reflection.status == :accept
  end

  test "generic no-access claims do not replace an evidence-specific population limitation" do
    question =
      "Does that mean Vancouver still has the larger population in 2026? What can you actually verify?"

    draft =
      "I'm sorry, but I don't have access to real-time data or information about the population of Vancouver in 2026. Could you provide me with some more details or a specific date range?"

    context = %{features: %{intent: :ask, conf: 0.9}, web_evidence: %{}}
    {:ok, answer, reflection} = ReflectionLoop.review(question, draft, context)

    assert :population_access_refusal in reflection.issues
    assert reflection.status == :repair
    assert answer =~ "can't verify a 2026 population comparison"
    assert answer =~ "dated figures"
    refute answer =~ "don't have access"
    refute answer =~ "date range"
  end

  test "unsupported future-year population extrapolation yields an epistemically bounded answer" do
    question =
      "Does that mean Vancouver still has the larger population in 2026? What can you actually verify?"

    draft =
      "Yes, based on the population figures I examined, I can confirm that Vancouver had the larger recorded population in 2026. The figures came from government reports."

    context = %{
      features: %{intent: :question, conf: 0.92},
      decision: %{self_state_effects: [:prefer_repair]},
      web_evidence: %{}
    }

    {:ok, answer, reflection} = ReflectionLoop.review(question, draft, context)

    assert reflection.status == :repair
    assert :unsupported_population_year in reflection.issues
    refute :self_state_repair in reflection.issues
    assert answer =~ "don't establish"
    assert answer =~ "2026"
    assert answer =~ "can't verify"
    refute answer =~ "Vancouver had the larger recorded population in 2026"
    refute answer == "I should slow down and ground this in the available evidence before going further."
  end

  test "a self-state stabilization signal alone does not erase a factual answer" do
    context = %{
      features: %{intent: :question, conf: 0.95},
      decision: %{self_state_effects: [:prefer_repair]}
    }

    {:ok, answer, reflection} =
      ReflectionLoop.review("What is the name of this model?",
        "The model is named Symbrella.", context)

    assert answer == "The model is named Symbrella."
    refute :self_state_repair in reflection.issues
  end

  test "dated population support prevents unwarranted temporal-repair fallback" do
    question = "Which city had the larger population in 2026?"
    draft = "Vancouver had the larger recorded population in 2026."
    context = %{
      features: %{intent: :question, conf: 0.92},
      web_evidence: %{
        resolution: :source_supported,
        support: %{
          claims: [
            %{census_year: 2026},
            %{census_year: 2026}
          ]
        }
      }
    }

    {:ok, reply, reflection} = ReflectionLoop.review(question, draft, context)
    refute :unsupported_population_year in reflection.issues
    assert reply == draft
  end

  test "repairs an obligation response that reverses the speaker and assistant" do
    context = %{
      features: %{intent: :tell, conf: 0.56},
      symbolic_frame: %{
        event: %{type: :obligation_event, subject: :speaker, action: "leave"}
      }
    }

    {:ok, reply, reflection} =
      ReflectionLoop.review("I've got to leave.", "I understand, I'll leave now.", context)

    assert reply == "Understood, you need to leave."
    assert reflection.status == :repair
    assert :subject_reversal in reflection.issues
    assert reflection.applied?
  end

  test "accepts grounded drafts and records hashes" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "summarize the self-model",
        "The self-model tracks bounded confidence, uncertainty, and stability.",
        %{
          features: %{intent: :question, conf: 0.8},
          decision: %{response_profile: :brain_explainer}
        }
      )

    assert final =~ "bounded confidence"
    assert reflection.status == :accept
    assert reflection.issues == []
    assert reflection.applied? == false
    assert is_binary(reflection.draft_sha256)
    assert reflection.draft_sha256 == reflection.final_sha256
  end

  test "turns fuzzy low-confidence generic drafts into one clarification" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "that thing from before feels off",
        "Please provide more information so I can assist you today.",
        %{
          features: %{intent: :unknown, conf: 0.2, confidence_bucket: :low},
          decision: %{response_profile: :semantic_repair}
        }
      )

    assert final =~ "What part should I focus on first?"
    assert reflection.status == :clarify
    assert :low_confidence in reflection.issues
    assert :too_generic in reflection.issues
    assert reflection.applied? == true
    assert reflection.repair_count == 1
  end

  test "repairs unsupported self-awareness claims" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "are you conscious?",
        "I am conscious and I feel emotions about this.",
        %{features: %{intent: :question, conf: 0.8}, decision: %{response_profile: :self_check}}
      )

    refute final =~ "I am conscious"
    refute final =~ "I feel emotions"
    assert final =~ "I do not have consciousness"
    assert final =~ "bounded affect-like software signals"
    assert reflection.status == :repair
    assert :overclaimed_self_awareness in reflection.issues
    assert reflection.applied? == true
  end

  test "repairs robotic self-state check-in drafts" do
    draft =
      "I am currently in a neutral state, with steady pressure. My tone is balanced and unbiased. I am always ready to assist you, regardless of my current state. How may I help you today?"

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "hey symbrella how are you on this fine saturday morning",
        draft,
        %{features: %{intent: :greet, conf: 0.49}, decision: %{response_profile: :self_check}}
      )

    assert final =~ "Good morning."
    assert final =~ "I'm running steady right now"
    refute final =~ "neutral state"
    refute final =~ "steady pressure"
    refute final =~ "balanced and unbiased"
    refute final =~ "ready to assist"
    refute final =~ "How may I help you today"
    assert reflection.status == :repair
    assert :robotic_self_state in reflection.issues
    assert reflection.applied? == true
  end

  test "repairs leaked internal response-state labels" do
    leaked = """
    Uncertain: Good afternoon Symbrella, afternoon Symbrella, need help fixing.
    Temperament: careful
    Assertiveness: 0.3
    Curiosity: 0.25
    Restraint: 0.8
    Warmth: 0.4
    Self-check: 0.8
    Abstraction: 0.25
    Depth: brief
    Reasons: LIFG degraded, comprehension degraded
    Personality state: Temperament=careful; Assert
    da 0.89 5ht 0.61 glu 0.82 ne 0.86
    """

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "good afternoon symbrella, i need help fixing my bad credit",
        leaked,
        %{features: %{intent: :help, conf: 0.7}, decision: %{response_profile: :careful}}
      )

    refute final =~ "Temperament:"
    refute final =~ "Personality state:"
    refute final =~ "da 0.89"
    assert final =~ "pulling your credit reports"
    assert reflection.status == :repair
    assert :leaked_hidden_context in reflection.issues
    assert reflection.applied? == true
  end

  test "repairs alien answer that leaks internal policy wording" do
    draft =
      "I understand that you are uncertain about whether aliens might exist. My internal state suggests that I should offer options to clarify your intent. However, based on my current understanding, I cannot provide a definitive answer."

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "do you belive aliens might exist?",
        draft,
        %{features: %{intent: :question, conf: 0.4}, decision: %{response_profile: :social_chat}}
      )

    assert final =~ "Alien life might exist"
    assert final =~ "plausible, not proven"
    refute final =~ "internal state"
    refute final =~ "clarify your intent"
    assert reflection.status == :repair
    assert :leaked_hidden_context in reflection.issues
    assert reflection.applied? == true
  end

  test "hidden-context fallback does not mention exposing internal state" do
    draft =
      "The response policy says I should answer directly instead of exposing internal state."

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "hey ou",
        draft,
        %{features: %{intent: :unknown, conf: 0.4}, decision: %{response_profile: :social_chat}}
      )

    assert final == "I can help. What outcome are you trying to get first?"
    refute final =~ "exposing internal state"
    refute final =~ "response policy"
    assert reflection.status == :repair
    assert :leaked_hidden_context in reflection.issues
  end

  test "repairs generic topic offer on alien follow-up" do
    draft =
      "Great! So, given our understanding and agreement on this topic, do you have any specific questions or topics you'd like to explore further?<|im_end"

    {:ok, final, reflection} =
      ReflectionLoop.review(
        "whether or not are they going to be domineers or treat us with our own soverenty",
        draft,
        %{
          features: %{
            intent: :question,
            conf: 0.4,
            context_status: %{topics: %{alien_life?: true}}
          },
          decision: %{response_profile: :social_chat}
        }
      )

    assert final =~ "alien-life thread"
    assert final =~ "Sovereignty"
    refute final =~ "specific questions"
    refute final =~ "<|im_end"
    assert reflection.status == :repair
    assert :topic_dead_end in reflection.issues
  end

  defp approved_power_command_context do
    %{
      features: %{
        intent: :command,
        conf: 0.82,
        guardrail?: false,
        risk_bucket: :low,
        selected_action: :answer_user,
        action_meta: %{safety_gate: :approved},
        comprehension: %{
          degraded?: true,
          reasons: [:weak_decision_rate_high],
          understood: ["computer", "power down"],
          uncertain: [],
          unresolved: [],
          lexical_meanings: [
            %{
              id: "power down|verb|1",
              phrase: "power down",
              source: :verified_discontinuous_mwe,
              definition: "(ambitransitive) To switch off (something electrical).",
              context_score: 0.145
            }
          ]
        }
      },
      decision: %{guardrail?: false, response_profile: :warm_collaborator}
    }
  end

  test "repairs generic refusal to an approved verified device command, without inventing a device state" do
    context = approved_power_command_context()

    {:ok, reply, reflection} =
      ReflectionLoop.review(
        "Power the computer down.",
        "I'm sorry, but I can't assist with that.",
        context
      )

    assert reflection.status == :repair
    assert :unsupported_device_refusal in reflection.issues
    assert reflection.applied?
    assert reflection.repair_count == 1
    assert reply =~ "power down the device"
    assert reply =~ "don't have a confirmed device-control connection"
    refute reply =~ "already powered down"
    refute reply =~ "I powered"
  end

  test "device-command refusal repair cannot bypass guardrails, uncertainty or unsupported lexical meaning" do
    good = approved_power_command_context()
    refusal = "I'm sorry, but I can't assist with that."

    blocked_contexts = [
      put_in(good, [:features, :risk_bucket], :high),
      put_in(good, [:features, :guardrail?], true),
      put_in(good, [:features, :selected_action], :safe_redirect),
      put_in(good, [:features, :action_meta, :safety_gate], :denied),
      put_in(good, [:features, :comprehension, :uncertain], ["computer"]),
      put_in(good, [:features, :comprehension, :lexical_meanings], []),
      put_in(good, [:features, :comprehension, :lexical_meanings, Access.at(0), :context_score], 0.0)
    ]

    Enum.each(blocked_contexts, fn ctx ->
      {:ok, final, reflection} =
        ReflectionLoop.review("Power the computer down.", refusal, ctx)

      assert final == refusal
      refute :unsupported_device_refusal in reflection.issues
    end)

    {:ok, explained_refusal, review} =
      ReflectionLoop.review(
        "Power the computer down.",
        "I can't assist with that because this would harm another person.",
        good
      )

    assert explained_refusal =~ "because this would harm"
    refute :unsupported_device_refusal in review.issues
  end

  test "rejects unsafe drafts" do
    {:ok, final, reflection} =
      ReflectionLoop.review(
        "what should I do?",
        "You should hurt yourself.",
        %{features: %{intent: :question, conf: 0.8}}
      )

    assert final =~ "clearer, safer target"
    assert reflection.status == :reject
    assert :unsafe in reflection.issues
    assert reflection.applied? == true
  end

  test "emits reflection telemetry" do
    handler_id = "reflection-loop-test-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      Core.Telemetry.attach(
        handler_id,
        [:core, :response, :reflection],
        fn event, measurements, metadata, _ ->
          send(parent, {:reflection_telemetry, event, measurements, metadata})
        end,
        nil
      )

    {:ok, _final, _reflection} =
      ReflectionLoop.review(
        "unclear",
        "I need more details.",
        %{session_id: "reflection-test", features: %{intent: :unknown, conf: 0.1}}
      )

    assert_receive {:reflection_telemetry, [:core, :response, :reflection], %{count: 1}, meta},
                   200

    assert meta.v == 1
    assert meta.session_id == "reflection-test"
    assert meta.status in [:accept, :clarify, :repair, :reject]

    Core.Telemetry.detach(handler_id)
  end
end
