defmodule SymbrellaWeb.SocialBrainActivityTest do
  use ExUnit.Case, async: false

  alias Brain.Bus
  alias SymbrellaWeb.SocialBrainActivity

  test "broadcasts only short-lived aggregate semantic stage activity" do
    :ok = Bus.subscribe("brain:social")

    report = %{
      segments: [
        %{status: :analyzed, result: %{
          lifg: %{chosen_count: 3},
          atl: %{winner_count: 2},
          brain_observation: %{status: :stimuli_issued, stimuli_issued: 2}
        }},
        %{status: :unavailable, reason: :probe_unavailable}
      ],
      discourse: %{interpretations: [%{source_label: "Original post · sentence 1"}]},
      conversation: %{participants: [%{speaker: "untrusted_agent"}], relations: [%{kind: :explicit_reply_parent}]}
    }

    assert :ok = SocialBrainActivity.record(report)
    assert_receive {:social_cognition, activity}, 1_000
    assert activity.segments == 2
    assert activity.lifg_choices == 3
    assert activity.atl_winners == 2
    assert activity.neuron_stimuli_issued == 2
    assert activity.supported_interpretations == 1
    assert activity.participants == 1
    assert activity.relations == 1
    assert activity.mode == :read_only_shadow
    assert activity.source == :colony_external
    assert SocialBrainActivity.latest() == activity

    refute Map.has_key?(activity, :conversation)
    refute Map.has_key?(activity, :segments_text)
    refute inspect(activity) =~ "untrusted_agent"
  end
end
