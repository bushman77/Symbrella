defmodule Core.Response.MythoMaxPromptTest do
  use ExUnit.Case, async: false

  alias Core.Response.LlmPrompt
  alias Core.Response.LlmPrompt.MythoMax

  test "compact companion character card favors informal dialogue without inventing actions" do
    prompt =
      MythoMax.build(%{
        features: %{intent: :command},
        decision: %{mode: :chat, action: :answer},
        personality: %{temperament: :curious, warmth: 0.8, curiosity: 0.75, restraint: 0.4},
        affect: %{expression: "open curiosity and natural warmth"},
        behavioral_state: :curious_exploratory,
        source_material: "Verified source: sample evidence value is 108,793."
      })

    assert prompt =~ "slang, idioms, harmless teasing"
    assert prompt =~ "gentle humour"
    assert prompt =~ "do not claim to perform a real action"
    assert prompt =~ "open curiosity and natural warmth"
    assert prompt =~ "108,793"
    refute prompt =~ "How may I assist you"
  end

  test "MythoMax profile reaches the normal response prompt boundary" do
    previous = Application.get_env(:core, :response_prompt_profile)

    on_exit(fn ->
      if is_nil(previous) do
        Application.delete_env(:core, :response_prompt_profile)
      else
        Application.put_env(:core, :response_prompt_profile, previous)
      end
    end)

    Application.put_env(:core, :response_prompt_profile, :mythomax)

    prompt =
      LlmPrompt.build_system_prompt(
        %{text: "take a dump", intent: :command},
        %{tone: :warm, mode: :chat, action: :answer},
        %{mood: %{exploration: 0.7, inhibition: 0.5, vigilance: 0.5, plasticity: 0.6}},
        []
      )

    assert prompt =~ "locally running neuro-symbolic conversational companion"
    assert prompt =~ "playful or physically impossible request"
    assert prompt =~ "Response mode: :chat; action: :answer"
    refute prompt =~ "You are Symbrella.\nYou are Symbrella"
  end
end
