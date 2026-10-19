defmodule Core.Response.SpeakerLocationFidelityTest do
  use ExUnit.Case, async: true

  alias Core.Response.ReflectionLoop
  alias Core.Response.SpeakerLocationFidelity

  @incorrect_self_location "I'm currently located in Kamloops, British Columbia, Canada."
  @where_am_i "where am i atm"

  @previous_assistant_claim [
    %{"role" => "user", "content" => @where_am_i},
    %{"role" => "assistant", "content" => @incorrect_self_location}
  ]

  test "regression: user location question cannot turn into assistant self-location claim" do
    assert SpeakerLocationFidelity.issue(@where_am_i,
      @incorrect_self_location, %{}) == :user_current_location_unverified

    {:ok, final, reflection} =
      ReflectionLoop.review(@where_am_i, @incorrect_self_location,
        %{features: %{intent: :ask, conf: 0.81}})

    assert reflection.status == :repair
    assert :speaker_location_fidelity in reflection.issues
    assert reflection.applied?
    assert final =~ "can't verify your current physical location"
    refute final =~ "I'm currently located in Kamloops"
  end

  test "regression: user location cannot be fabricated in second person either" do
    for question <- [
      "where am i", "where am i atm", "Where am I right now?",
      "What is my current location?", "can you tell me where i am"
    ] do
      for draft <- [
        "You are currently located in Kamloops, British Columbia.",
        "You're in Maple Ridge, BC.",
        "Your current location is Kamloops."
      ] do
        {:ok, final, reflection} = ReflectionLoop.review(question, draft, %{})
        assert reflection.status == :repair
        assert :speaker_location_fidelity in reflection.issues
        assert final =~ "can't verify your current physical location"
        refute final =~ "You are currently located in Kamloops"
      end
    end
  end

  test "regression: direct denial cannot repeat the unsupported prior assistant self-location" do
    context = %{
      review_history: @previous_assistant_claim,
      features: %{intent: :deny, conf: 0.82}
    }

    assert SpeakerLocationFidelity.correction_to_unsupported_self_location?(
      "no you are not", context)

    {:ok, final, reflection} =
      ReflectionLoop.review("no you are not", @incorrect_self_location, context)

    assert reflection.status == :repair
    assert reflection.applied?
    assert :speaker_location_fidelity in reflection.issues
    assert final =~ "My earlier claim"
    assert final =~ "wasn't verified"
    refute final =~ "I'm currently located in Kamloops"
  end

  test "correction also repairs rephrased physical self-location claims" do
    context = %{review_history: @previous_assistant_claim}

    for question <- [
      "no you are not", "No, you're not!", "that's wrong"
    ] do
      {:ok, final, reflection} =
        ReflectionLoop.review(question, "Actually, I am in Kamloops.", context)

      assert :speaker_location_fidelity in reflection.issues
      assert reflection.status == :repair
      assert final =~ "wasn't verified"
      refute final =~ "I am in Kamloops"
    end
  end

  test "a denial after an already corrected answer cannot introduce a new self-location" do
    context = %{
      review_history: [
        %{"role" => "user", "content" => "where am i atm"},
        %{"role" => "assistant", "content" =>
          "I can't verify your current physical location from the conversation."}
      ]
    }

    {:ok, final, reflection} =
      ReflectionLoop.review("no you are not",
        "I'm currently located in Kamloops, British Columbia.", context)

    assert reflection.status == :repair
    assert :speaker_location_fidelity in reflection.issues
    assert final =~ "don't have verified evidence"
    refute final =~ "currently located in Kamloops"
  end

  test "a correct admission of uncertainty is not rewritten" do
    unsure = "I can't determine your current location from this chat."
    {:ok, ^unsure, reflection} = ReflectionLoop.review(@where_am_i, unsure, %{})
    refute :speaker_location_fidelity in reflection.issues

    corrected = "You're right; I shouldn't have claimed a physical location."
    {:ok, ^corrected, reflection} =
      ReflectionLoop.review("no you are not", corrected,
        %{review_history: @previous_assistant_claim})
    refute :speaker_location_fidelity in reflection.issues
  end

  test "where are you is about the assistant but does not authorize a physical location" do
    {:ok, final, review} =
      ReflectionLoop.review("where are you located", "I'm in Kamloops.", %{})

    assert :speaker_location_fidelity in review.issues
    assert final =~ "own physical location"

    correct = "I'm Symbrella, a locally running assistant."
    {:ok, ^correct, review} = ReflectionLoop.review("where are you?", correct, %{})
    refute :speaker_location_fidelity in review.issues
  end

  test "public place identity and sourced distances stay outside deictic attribution review" do
    for {question, answer} <- [
      {"where is kamloops bc",
       "According to Wikipedia, Kamloops is in British Columbia, Canada."},
      {"how far is Maple Ridge, BC from Kamloops, BC",
       "They are approximately 228.8 km apart in a straight line."},
      {"where is the museum",
       "The museum is in Kamloops, British Columbia."}
    ] do
      assert SpeakerLocationFidelity.issue(question, answer, %{}) == nil
    end
  end

  test "a correction needs an immediately preceding assistant self-location claim and question" do
    wrong_histories = [
      [],
      [%{"role" => "assistant", "content" => @incorrect_self_location}],
      [%{"role" => "user", "content" => "where is kamloops bc"},
       %{"role" => "assistant", "content" => @incorrect_self_location}],
      [%{"role" => "user", "content" => @where_am_i},
       %{"role" => "assistant", "content" => @incorrect_self_location},
       %{"role" => "user", "content" => "what time is it"},
       %{"role" => "assistant", "content" => "I don't know."}]
    ]

    for history <- wrong_histories do
      ctx = %{review_history: history}
      refute SpeakerLocationFidelity.correction_to_unsupported_self_location?(
        "no you are not", ctx)
      assert SpeakerLocationFidelity.issue(
        "no you are not", @incorrect_self_location, ctx) == nil
    end
  end

  test "the user mentioning a city or old model answer is not location proof" do
    context = %{
      review_history: [
        %{"role" => "user", "content" => "Kamloops is in British Columbia"},
        %{"role" => "assistant", "content" => "That's a city."}
      ]
    }

    {:ok, final, review} =
      ReflectionLoop.review(@where_am_i, @incorrect_self_location, context)
    assert review.status == :repair
    assert final =~ "can't verify your current physical location"
  end
end
