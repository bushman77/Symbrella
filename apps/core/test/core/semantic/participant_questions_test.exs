defmodule Core.Semantic.ParticipantQuestionsTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.ParticipantQuestions

  test "question grammar selects predicate or location slots without knowing the answer" do
    assert {:forward, "bird", :predicate, %{}} ==
      ParticipantQuestions.parse("What is the bird doing?")

    assert {:forward, "bird", :location, %{}} ==
      ParticipantQuestions.parse("Where is the bird?")

    assert {:forward, "fox", :predicate, %{}} ==
      ParticipantQuestions.parse("What is the fox doing?")

    assert {:forward, "fox", :location, %{}} ==
      ParticipantQuestions.parse("Where is the fox?")

    assert {:forward, "bird", :location,
            %{predicate: "flapping", position: "on"}} ==
      ParticipantQuestions.parse("What is the bird flapping on?")
  end

  test "question parser abstains on unsupported questions instead of inventing roles" do
    assert nil == ParticipantQuestions.parse("Where was the bird?")
    assert nil == ParticipantQuestions.parse("What is the bird doing")
    assert nil == ParticipantQuestions.parse("Which bird is flapping?")
    assert nil == ParticipantQuestions.parse("The bird is flapping.")
  end
end
