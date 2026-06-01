defmodule Core.Response.TopicsTest do
  use ExUnit.Case, async: true

  alias Core.Response.Topics

  test "labels common conversation topics" do
    assert Topics.labels("do you believe aliens might exist?") |> MapSet.member?(:alien_life)
    assert Topics.labels("im close to getting my own place") |> MapSet.member?(:housing)
    assert Topics.labels("my credit score is bad") |> MapSet.member?(:credit)
    assert Topics.labels("I missed my medication dose") |> MapSet.member?(:health)
    assert Topics.labels("how do your neuromodulators feel?") |> MapSet.member?(:self_state)
  end

  test "summarizes labels from recent messages" do
    topics =
      Topics.from_messages([
        %{"role" => "user", "content" => "do aliens exist?"},
        %{"role" => "assistant", "content" => "Alien life is plausible but unconfirmed."}
      ])

    assert Topics.has?(topics, :alien_life)
    assert :alien_life in topics.labels
  end

  test "recognizes vague alien-life follow-ups" do
    assert Topics.followup?("i belive they exist", :alien_life)
    assert Topics.followup?("yeahh the elite would never admit it", :alien_life)
    assert Topics.followup?("whether or not are they going to be domineers", :alien_life)
    assert Topics.followup?("would they treat us with our own soverenty", :alien_life)
  end
end
