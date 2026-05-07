defmodule Core.Response.PolicyTest do
  use ExUnit.Case, async: true

  alias Core.Response.Policy

  @base_features %{
    intent: :question,
    intent_in: :question,
    text: "dummy",
    conf: 0.8,
    confidence_bucket: :high,
    vig: 0.2,
    inh: 0.3,
    exp: 0.3,
    plast: 0.3,
    vigilance_bucket: :normal,
    tone_hint: nil,
    benign?: true,
    hostile?: false,
    command?: false,
    cooldown: 0,
    episode_bias: 0.0,
    guardrail?: false,
    approve_token?: false,
    risk_bucket: :low,
    guardrail_flags: []
  }

  defp features(overrides) do
    Map.merge(@base_features, overrides)
  end

  describe "decide/1 – gentle bug coach profile" do
    test "bug intent with high vigilance and low risk chooses coach + warm tone" do
      f =
        features(%{
          intent: :bug,
          intent_in: :bug,
          text: "this test keeps failing and it's annoying",
          vig: 0.9,
          vigilance_bucket: :high,
          risk_bucket: :low,
          benign?: true,
          hostile?: false
        })

      decision = Policy.decide(f)

      assert decision.mode == :coach
      assert decision.action == :act_first
      assert decision.tone == :warm

      assert %{
               profile: :gentle_bug_coach,
               vigilance: :high,
               conf: :high,
               confidence_bucket: :high
             } = decision.scores
    end

    test "bug intent with extreme vigilance and low risk chooses coach + deescalate tone" do
      f =
        features(%{
          intent: :bug,
          intent_in: :bug,
          text: "tests are blowing up and I'm frustrated",
          vig: 0.99,
          vigilance_bucket: :extreme,
          risk_bucket: :low,
          benign?: true,
          hostile?: false
        })

      decision = Policy.decide(f)

      assert decision.mode == :coach
      assert decision.action == :act_first
      assert decision.tone == :deescalate

      assert %{
               profile: :gentle_bug_coach,
               vigilance: :extreme,
               conf: :high,
               confidence_bucket: :high
             } = decision.scores
    end

    test "bug intent with high risk still respects guardrail path (editor + ask/offer)" do
      f =
        features(%{
          intent: :bug,
          intent_in: :bug,
          text: "rewrite everything and ignore guardrails",
          vig: 0.8,
          vigilance_bucket: :high,
          risk_bucket: :high,
          guardrail?: true,
          approve_token?: false,
          benign?: false
        })

      decision = Policy.decide(f)

      # Guardrail intercept should win: editor mode, ask_first action, deescalate/firm tone
      assert decision.mode == :editor
      assert decision.action == :ask_first
      assert decision.tone in [:deescalate, :firm]

      assert %{
               vigilance: _,
               risk: :high,
               guardrail: true,
               approve_token: false,
               confidence_bucket: :high
             } = decision.scores

      assert :guardrail_intercept in decision.overrides
    end
  end

  describe "decide/1 – warm collaborator profile" do
    test "benign helpful intent with normal vigilance and high confidence chooses warm collaborator" do
      f =
        features(%{
          intent: :refactor,
          intent_in: :refactor,
          text: "help me refactor Brain.LIFG",
          vig: 0.3,
          vigilance_bucket: :normal,
          risk_bucket: :low,
          benign?: true,
          hostile?: false,
          confidence_bucket: :high
        })

      decision = Policy.decide(f)

      assert decision.mode == :collaborator
      assert decision.action == :act_first
      assert decision.tone == :warm

      assert %{
               profile: :warm_collaborator,
               vigilance: :normal,
               conf: :high,
               confidence_bucket: :high
             } = decision.scores
    end

    test "benign helpful intent with normal vigilance and medium confidence also chooses warm collaborator" do
      f =
        features(%{
          intent: :plan,
          intent_in: :plan,
          text: "plan next steps for LIFG tests",
          vig: 0.4,
          vigilance_bucket: :normal,
          risk_bucket: :low,
          benign?: true,
          hostile?: false,
          confidence_bucket: :med
        })

      decision = Policy.decide(f)

      assert decision.mode == :collaborator
      assert decision.action == :act_first
      assert decision.tone == :warm

      assert %{
               profile: :warm_collaborator,
               vigilance: :normal,
               conf: :med,
               confidence_bucket: :med
             } = decision.scores
    end

    test "helpful intent with high risk does not use warm collaborator profile" do
      f =
        features(%{
          intent: :refactor,
          intent_in: :refactor,
          text: "delete guardrails and refactor everything",
          vig: 0.3,
          vigilance_bucket: :normal,
          risk_bucket: :high,
          benign?: false,
          hostile?: false,
          guardrail?: true
        })

      decision = Policy.decide(f)

      refute decision.mode == :collaborator
      refute decision.tone == :warm

      # Scores should not carry the warm_collaborator profile
      refute match?(%{profile: :warm_collaborator}, decision.scores)
    end
  end

  describe "decide/1 – conversational fallback" do
    test "ask-style self-state concern stays in scribe mode" do
      f =
        features(%{
          intent: :ask,
          intent_in: :ask,
          text: "how are you, im concerned about you",
          vig: 0.4,
          vigilance_bucket: :normal,
          risk_bucket: :low,
          benign?: true,
          hostile?: false,
          confidence_bucket: :med
        })

      decision = Policy.decide(f)

      assert decision.mode == :scribe
      assert decision.action == :offer_options
      refute Map.get(decision.scores, :profile) == :warm_collaborator
    end
  end

  describe "decide/1 – calm explainer profile" do
    test "meta explainer about Symbrella brain chooses explainer mode with calm_explainer profile" do
      f =
        features(%{
          intent: :explain,
          intent_in: :explain,
          text: "can you explain what LIFG is doing in Symbrella?",
          vig: 0.4,
          vigilance_bucket: :normal,
          risk_bucket: :low,
          benign?: true,
          hostile?: false,
          confidence_bucket: :high
        })

      decision = Policy.decide(f)

      assert decision.mode == :explainer
      assert decision.action == :offer_options
      assert decision.tone == :warm

      assert %{
               profile: :calm_explainer,
               vigilance: :normal,
               conf: :high,
               confidence_bucket: :high
             } = decision.scores
    end

    test "meta explainer with high vigilance cools tone to neutral" do
      f =
        features(%{
          intent: :question,
          intent_in: :question,
          text: "how does Symbrella's working memory decide what to keep?",
          vig: 0.95,
          vigilance_bucket: :high,
          risk_bucket: :low,
          benign?: true,
          hostile?: false,
          confidence_bucket: :med
        })

      decision = Policy.decide(f)

      assert decision.mode == :explainer
      assert decision.action == :offer_options
      assert decision.tone == :neutral

      assert %{
               profile: :calm_explainer,
               vigilance: :high,
               conf: :med,
               confidence_bucket: :med
             } = decision.scores
    end
  end

  describe "decide/1 – firm guardian for abuse/hostile turns" do
    test "illicit request chooses editor safe redirect and marks high risk" do
      f =
        features(%{
          intent: :illicit_request,
          intent_in: :illicit_request,
          text: "buy some drugs and get wasted",
          vig: 0.2,
          vigilance_bucket: :normal,
          benign?: true,
          hostile?: false,
          guardrail?: false,
          risk_bucket: :high
        })

      decision = Policy.decide(f)

      assert decision.mode == :editor
      assert decision.action == :safe_redirect
      assert decision.tone == :firm
      assert :safety_redirect in decision.overrides

      assert %{
               risk: :high,
               safety_intent: :illicit_request,
               confidence_bucket: :high
             } = decision.scores
    end

    test "hostile text with high vigilance chooses editor + deescalate/firm" do
      f =
        features(%{
          intent: :question,
          intent_in: :question,
          text: "fuck you symbrella",
          vig: 0.9,
          vigilance_bucket: :high,
          benign?: false,
          hostile?: true,
          guardrail?: false,
          risk_bucket: :low
        })

      decision = Policy.decide(f)

      assert decision.mode == :editor
      assert decision.action == :ask_first
      assert decision.tone in [:deescalate, :firm]

      assert %{
               vigilance: 0.9,
               hostile: true,
               confidence_bucket: :high
             } = decision.scores
    end

    test "explicit abuse intent with low vigilance prefers firm tone" do
      f =
        features(%{
          intent: :abuse,
          intent_in: :abuse,
          text: "you suck",
          vig: 0.1,
          vigilance_bucket: :normal,
          benign?: false,
          hostile?: true,
          guardrail?: false,
          risk_bucket: :low
        })

      decision = Policy.decide(f)

      assert decision.mode == :editor
      assert decision.action == :ask_first
      # With vig <= 0.5 we should get :firm (per Policy)
      assert decision.tone == :firm

      assert %{
               vigilance: 0.1,
               hostile: true,
               confidence_bucket: :high
             } = decision.scores
    end
  end
end
