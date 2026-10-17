defmodule Core.Response.IdentityContinuityTest do
  use ExUnit.Case, async: false

  alias Core.Response

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  test "assistant identity comes from configuration" do
    for input <- ["what is your name?", "what's your name?", "what should I call you?"] do
      {_tone, answer, meta} = Response.plan(%{text: input, intent: :question})
      assert answer == "My name is Symbrella."
      assert meta.identity_subject == :assistant
      assert meta.response_source == :identity
    end
  end

  test "elliptical question uses an actual name exchange" do
    session_id = "identity-test-#{System.unique_integer([:positive])}"

    assert Response.memory_reply(%{text: "and yours?", session_id: session_id}) == nil

    {_tone, _answer, _meta} =
      Response.plan(%{text: "what is my name?", session_id: session_id})

    {_tone, answer, meta} =
      Response.plan(%{text: "and yours?", session_id: session_id})

    assert answer == "My name is Symbrella."
    assert meta.identity_subject == :assistant
  end
end
