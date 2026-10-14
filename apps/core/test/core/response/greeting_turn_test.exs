defmodule Core.Response.GreetingTurnTest do
  use ExUnit.Case, async: true

  alias Core.Response.GreetingTurn
  alias Core.Response.ReflectionLoop

  test "only an isolated greeting is a social greeting turn" do
    for text <- [
      "good afternoon symbrella",
      "Good afternoon, Symbrella!",
      "good morning",
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
