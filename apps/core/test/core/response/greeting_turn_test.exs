defmodule Core.Response.GreetingTurnTest do
  use ExUnit.Case, async: true

  alias Core.Response.GreetingTurn
  alias Core.Response.ReflectionLoop

  test "only an isolated greeting is a social greeting turn" do
    for text <- [
      "good afternoon symbrella",
      "Good afternoon, Symbrella!",
      "good morning",
      "good evenniing symbrella",
      "good eveing symbrella",
      "Hello there",
      "hey symbrella",
      "hi",
      "hiya!",
      "heya!",
      "sup"
    ] do
      assert GreetingTurn.standalone?(text), inspect(text)
    end

    for text <- [
      "good afternoon symbrella I got a new job",
      "good afternoon, could you debug this?",
      "hello Symbrella how are you?",
      "good evening Symbrella, I need your help",
      "what do you know about Japan's railgun development?"
    ] do
      refute GreetingTurn.standalone?(text), inspect(text)
    end
  end

  test "recognizes and repairs the reported reply without inventing a script" do
    user = "good afternoon symbrella"
    draft = "good afternoon symbrella. How may I assist you today?"

    assert GreetingTurn.robotic_draft?(user, draft)
    assert GreetingTurn.repair(user, draft) == "good afternoon."

    {:ok, final, reflection} =
      ReflectionLoop.review(user, draft, %{features: %{intent: :greeting, conf: 0.85}})

    assert final == "good afternoon."
    assert reflection.status == :repair
    assert reflection.applied?
    assert :robotic_greeting in reflection.issues
    assert reflection.repair_instruction =~ "standalone greeting"
  end

  test "typo greeting does not turn into a clarification request" do
    user = "good evenniing symbrella"
    draft = "I'm sorry, but I'm not entirely sure what you mean. Could you please clarify?"

    assert GreetingTurn.standalone?(user)
    assert GreetingTurn.robotic_draft?(user, draft)
    assert GreetingTurn.repair(user, draft) == "Good evening."
  end

  test "the observed MythoMax explanation is repaired for a typo greeting" do
    user = "good eveing symbrella"

    draft =
      "Hello! I understand that you're saying \"good evening\" to me. " <>
        "Is there anything specific you would like to discuss or ask about? " <>
        "If you're just saying hello, that's fine too. " <>
        "I'm here to assist you in any way I can."

    assert GreetingTurn.standalone?(user)
    assert GreetingTurn.robotic_draft?(user, draft)
    assert GreetingTurn.repair(user, draft) == "Good evening."

    {:ok, final, reflection} =
      ReflectionLoop.review(user, draft, %{features: %{text: user, intent: :unknown, conf: 0.2}})

    assert final == "Good evening."
    assert reflection.status == :repair
    assert reflection.applied?
    assert :robotic_greeting in reflection.issues
  end

  test "MythoMax does not address the user as Symbrella in a greeting" do
    user = "good evening symbrella"
    draft = "Good evening! How are you feeling tonight, Symbrella?"

    assert GreetingTurn.standalone?(user)
    assert GreetingTurn.robotic_draft?(user, draft)
    assert GreetingTurn.repair(user, draft) ==
             "Good evening! How are you feeling tonight?"

    {:ok, final, reflection} =
      ReflectionLoop.review(user, draft, %{features: %{text: user, intent: :greet, conf: 0.85}})

    assert final == "Good evening! How are you feeling tonight?"
    assert :robotic_greeting in reflection.issues
    assert reflection.status == :repair
  end

  test "trailing name repair does not touch substantive turns or Symbrella self-references" do
    answer = "Yes, my name is Symbrella."
    user = "what is your name"
    refute GreetingTurn.robotic_draft?(user, answer)
    assert GreetingTurn.repair(user, answer) == answer

    greeting = "Good evening! I'm Symbrella, and I'm glad you dropped by."
    refute GreetingTurn.robotic_draft?("good evening", greeting)
    assert GreetingTurn.repair("good evening", greeting) == greeting
  end

  test "MythoMax's natural greeting is not replaced by a scripted answer" do
    user = "good eveing symbrella"
    natural = "Evening! Anything interesting going on?"

    assert GreetingTurn.standalone?(user)
    refute GreetingTurn.robotic_draft?(user, natural)
    assert GreetingTurn.repair(user, natural) == natural
  end

  test "preserves genuine social conversation rather than replacing it with canned text" do
    user = "good afternoon symbrella"
    draft = "Good afternoon! How's your day going?"

    refute GreetingTurn.robotic_draft?(user, draft)
    assert GreetingTurn.repair(user, draft) == draft

    {:ok, final, reflection} =
      ReflectionLoop.review(user, draft, %{features: %{intent: :greeting, conf: 0.85}})

    assert final == draft
    refute :robotic_greeting in reflection.issues
  end

  test "repairs assistance offers while preserving other natural model text" do
    user = "good afternoon Symbrella"
    draft = "Good afternoon, Symbrella! It's nice to hear from you. How can I help you today?"

    result = GreetingTurn.repair(user, draft)
    assert result =~ "It's nice to hear from you."
    refute result =~ "Symbrella!"
    refute result =~ "How can I help"
  end

  test "does not remove offers from a substantive request just because it starts with a greeting" do
    user = "good afternoon Symbrella, can you help debug my code?"
    draft = "Good afternoon! I can help debug the module."

    refute GreetingTurn.robotic_draft?(user, draft)
    assert GreetingTurn.repair(user, draft) == draft
  end
end
