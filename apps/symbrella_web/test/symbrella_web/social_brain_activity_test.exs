defmodule SymbrellaWeb.SocialBrainActivityTest do
  use ExUnit.Case, async: false

  alias Brain.Bus
  alias SymbrellaWeb.SocialBrainActivity

  test "publishes exactly one compact shadow receipt and neuron receipt per analyzed thread" do
    parent = self()
    handler_id = "social-thread-aggregate-#{System.unique_integer([:positive])}"

    :ok = :telemetry.attach_many(
      handler_id,
      [[:core, :social, :cognitive_shadow], [:core, :social, :neuron_observation]],
      fn event, measurements, metadata, _config ->
        send(parent, {:aggregate_telemetry, event, measurements, metadata})
      end,
      nil
    )

    on_exit(fn -> :telemetry.detach(handler_id) end)

    report = %{
      segments: [
        %{status: :analyzed, result: %{
          token_count: 10,
          lifg: %{chosen_count: 8},
          atl: %{winner_count: 8},
          brain_observation: %{stimuli_issued: 4},
          syntax: %{signatures: [%{id: :noun_phrase}], audit: %{
            pos_covered_tokens: 8, ltm_source_rows: 11
          }}
        }},
        %{status: :analyzed, result: %{
          token_count: 6,
          lifg: %{chosen_count: 5},
          atl: %{winner_count: 5},
          brain_observation: %{stimuli_issued: 2},
          syntax: %{signatures: [], audit: %{
            pos_covered_tokens: 5, ltm_source_rows: 7
          }}
        }}
      ],
      discourse: %{interpretations: []},
      conversation: %{participants: [], relations: []}
    }

    assert :ok = SocialBrainActivity.record(report)

    assert_receive {:aggregate_telemetry, [:core, :social, :cognitive_shadow],
                    shadow, %{analysis_scope: :thread_aggregate, source: :colony_external}}
    assert shadow.segments == 2
    assert shadow.neuron_stimuli_issued == 6
    assert shadow.syntax_frames == 1
    assert shadow.processed_tokens == 16

    assert_receive {:aggregate_telemetry, [:core, :social, :neuron_observation],
                    %{stimuli_issued: 6, inspected_segments: 2},
                    %{analysis_scope: :thread_aggregate, source: :colony_external}}

    refute_receive {:aggregate_telemetry, _, _, _}, 20
  end

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
    assert activity.neuron_observation_statuses == %{stimuli_issued: 1}
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
