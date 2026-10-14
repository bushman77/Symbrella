defmodule Core.Response.DurableFactAcknowledgementTest do
  use ExUnit.Case, async: false

  alias Core.Response

  setup do
    Process.whereis(Brain.Hippocampus) || start_supervised!(Brain.Hippocampus)
    Brain.Hippocampus.reset()
    :ok
  end

  test "an acknowledged fact has a matching durable database value" do
    {_tone, reply, meta} =
      Response.plan(%{
        intent: :statement,
        text: "remember that my favorite mug is cyan"
      })

    assert reply == "I’ll remember that your favorite mug is cyan."
    assert meta.persistence_status == :persisted
    assert Db.Episode.latest_user_fact("favorite_mug") == "cyan"
  end

  test "a verified hippocampal fact becomes immediately recallable" do
    key = "verified_fact_#{System.unique_integer([:positive])}"

    assert :ok =
             Brain.Hippocampus.encode_verified_fact(
               %{sentence: "a verified fact", tokens: [key]},
               %{kind: :fact, key: key, value: "present", subject: :user, tags: ["fact"]}
             )

    assert Brain.Hippocampus.fact(key) == "present"
    assert Db.Episode.latest_user_fact(key) == "present"
  end
end
