defmodule Core.Response.CompanionContinuityTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "personal housing update stays conversational instead of comprehension fallback" do
    si = %{
      intent: :unknown,
      confidence: 0.34,
      text: "good afternoon symbrella im close to getting my own place"
    }

    {tone, text, meta} = Response.plan(si, %{})

    assert tone == :warm
    assert meta.mode == :chat
    assert meta.action == :answer
    assert :personal_life_update_answer in meta.overrides
    assert meta.response_source == :model_unavailable
    refute meta.response_source == :inline_skill
    assert text =~ "No fallback response was generated."
    refute text =~ "Good afternoon"
    refute text =~ "own place"
    refute text =~ "simpler words"
    refute text =~ "Suggested next step"
    refute text =~ "module"
    refute text =~ "file"
  end

  test "misspelled casual whats up stays conversational" do
    si = %{
      intent: :unknown,
      confidence: 0.35,
      text: "cmon tell me whaats up??"
    }

    {tone, text, meta} = Response.plan(si, %{})

    assert tone == :warm
    assert meta.mode == :chat
    assert meta.action == :answer
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "I’m here with you"
    refute text =~ "module"
    refute text =~ "file"
    refute text =~ "code"
    refute text =~ "Suggested next step"
  end

  test "huh repairs a bad prior turn without asking for code" do
    si = %{
      intent: :unknown,
      confidence: 0.2,
      text: "huh?"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.mode == :chat
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "came out wrong"
    refute text =~ "module"
    refute text =~ "file"
    refute text =~ "Suggested next step"
  end

  test "companion boundary prevents engineering fallback" do
    si = %{
      intent: :unknown,
      confidence: 0.4,
      text: "i didnt make you to write code i made you as a companion"
    }

    {tone, text, meta} = Response.plan(si, %{})

    assert tone == :warm
    assert meta.mode == :chat
    assert meta.action == :companion_repair
    assert :companion_repair in meta.overrides
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "companion"
    refute text =~ "module"
    refute text =~ "file"
    refute text =~ "Suggested next step"
  end

  test "liar triggers trust repair without requiring prior mood policy" do
    si = %{
      intent: :unknown,
      confidence: 0.3,
      text: "liar"
    }

    {tone, text, meta} = Response.plan(si, %{})

    assert tone == :deescalate
    assert meta.mode == :chat
    assert meta.action == :trust_repair
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "got pulled off the thread"
    refute text =~ "module"
    refute text =~ "file"
  end

  test "leading casual greeting does not swallow substantive opinion question" do
    si = %{
      intent: :ask,
      confidence: 0.66,
      text: "hey what do you think of war?"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred == :ask
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "War"
    refute text =~ "anti-suffering"
    refute text == "Hey. I’m here with you."
    refute text =~ "Suggested next step"
    refute text =~ "module"
    refute text =~ "file"
  end

  test "misspelled alternatives to war follow-up stays on topic" do
    si = %{
      intent: :ask,
      confidence: 0.5,
      text: "what are alternnatives  too war"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred == :ask
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "Alternatives to war"
    refute text =~ "diplomacy"
    refute text =~ "ceasefires"
    refute text =~ "what are you trying to talk through"
    refute text =~ "Suggested next step"
    refute text =~ "module"
    refute text =~ "file"
  end

  test "alien universe follow-up stays on topic instead of claiming lost context" do
    si = %{
      intent: :unknown,
      confidence: 0.43,
      text:
        "i coould see if there were no aliens if the universe was the size of our solar system"
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred in [:unknown, :question]
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "scale changes the intuition"
    refute text =~ "solar system"
    refute text =~ "universe"
    refute text =~ "missed the thread"
    refute text =~ "Suggested next step"
    refute text =~ "module"
    refute text =~ "file"
  end

  test "ufo footage follow-up stays in alien conversation" do
    session_id = {:test, __MODULE__, :ufo_followup, System.unique_integer([:positive])}

    Response.plan(
      %{
        intent: :unknown,
        confidence: 0.47,
        text: "do you think aliens exist?",
        session_id: session_id
      },
      %{}
    )

    si = %{
      intent: :unknown,
      confidence: 0.43,
      text: "well recently president donald trump annouced the release of ufo footagge",
      session_id: session_id
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred in [:unknown, :question]
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "same thread"
    refute text =~ "UFO or UAP footage"
    refute text =~ "not the same as confirmed alien life"
    refute text =~ "do not have prior chat context"
    refute text =~ "what are you trying to talk through"
    refute text =~ "missed the thread"
    refute text =~ "Suggested next step"
  end

  test "vague elite follow-up stays on prior alien conversation" do
    session_id = {:test, __MODULE__, :alien_elite_followup, System.unique_integer([:positive])}

    Response.plan(
      %{
        intent: :unknown,
        confidence: 0.47,
        text: "hey do you belive aliens exist",
        session_id: session_id
      },
      %{}
    )

    si = %{
      intent: :unknown,
      confidence: 0.35,
      text: "yeahh the elite of this world would never admit it for what ever reason",
      session_id: session_id
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.intent_inferred in [:unknown, :question]
    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "same alien-life thread"
    refute text =~ "separate claim"
    refute text =~ "simpler words"
    refute text =~ "Suggested next step"
    refute text =~ "module"
    refute text =~ "file"
  end

  test "belief follow-up stays on prior alien conversation" do
    session_id = {:test, __MODULE__, :alien_belief_followup, System.unique_integer([:positive])}

    Response.plan(
      %{
        intent: :unknown,
        confidence: 0.47,
        text: "do you belive aliens might exist?",
        session_id: session_id
      },
      %{}
    )

    si = %{
      intent: :unknown,
      confidence: 0.35,
      text: "i belive they exist",
      session_id: session_id
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "same alien-life thread"
    refute text =~ "turning it into a technical target"
    refute text =~ "simpler words"
  end

  test "alien sovereignty follow-up stays on prior alien conversation" do
    session_id =
      {:test, __MODULE__, :alien_sovereignty_followup, System.unique_integer([:positive])}

    Response.plan(
      %{
        intent: :unknown,
        confidence: 0.47,
        text: "do you think aliens exist?",
        session_id: session_id
      },
      %{}
    )

    si = %{
      intent: :unknown,
      confidence: 0.35,
      text: "whether or not are they going to be domineers or treat us with our own soverenty",
      session_id: session_id
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "same alien-life thread"
    refute text =~ "turning it into a technical target"
    refute text =~ "simpler words"
  end

  test "follow-up wording signals when prompt history is unavailable" do
    session_id = {:test, __MODULE__, :missing_context, System.unique_integer([:positive])}

    si = %{
      intent: :unknown,
      confidence: 0.43,
      text: "well recently that footage was weird",
      session_id: session_id
    }

    {_tone, text, meta} = Response.plan(si, %{})

    assert meta.response_source == :model_unavailable
    assert text =~ "No fallback response was generated."
    refute text =~ "do not have prior chat context"
    refute text =~ "alien-life thread"
    refute text =~ "That fits the same thread"
    refute text =~ "what are you trying to talk through"
    refute text =~ "missed the thread"
  end
end
