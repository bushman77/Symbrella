defmodule Core.Response.UserNameMemoryTest do
  use ExUnit.Case, async: false

  alias Core.Response

  setup do
    if Code.ensure_loaded?(Brain.Hippocampus) do
      Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
      Brain.Hippocampus.reset()
    end

    :ok
  end

  test "name question reads user_name fact from hippocampal memory" do
    Brain.Hippocampus.encode(
      %{winners: [%{id: "Curtis|proper|0", lemma: "Curtis"}]},
      %{kind: :fact, key: :user_name, value: "Curtis", tags: ["fact"]}
    )

    {_tone, text, meta} =
      Response.plan(%{
        intent: :question,
        confidence: 0.9,
        text: "what is my name?"
      })

    assert text == "Your name is Curtis."
    assert meta.action == :identity
    assert meta.user_name == "Curtis"
    refute text =~ "I don"
  end
end
