defmodule Core.Brain.MemoryFastPathContinuityTest do
  use ExUnit.Case, async: false

  alias Core.Brain.MemoryFastPathContinuity, as: Continuity

  test "memory reply continuity is a no-response, non-persisting, non-action path" do
    opts = Continuity.options("memory-route-session")

    assert Keyword.get(opts, :mode) == :prod
    assert Keyword.get(opts, :session_id) == "memory-route-session"
    assert Keyword.get(opts, :response) == :off
    assert Keyword.get(opts, :episodes) == false
    assert Keyword.get(opts, :persist_episodes) == false
    assert Keyword.get(opts, :hippocampus_encode?) == false
    assert Keyword.get(opts, :self_model) == :off
    assert Keyword.get(opts, :execute_agency_commands?) == false
    assert Keyword.get(opts, :record_agency_commands?) == false
    assert Keyword.get(opts, :comprehension_wm?) == true
    assert Keyword.get(opts, :notify_brain?) == true
    assert Keyword.get(opts, :semantic_propagation?) == true
    assert Keyword.get(Keyword.fetch!(opts, :lifg_opts), :grammar_weight) == 1.0
    assert Keyword.get(Keyword.fetch!(opts, :lifg_opts), :cerebellum_learn?) == false
    refute Keyword.has_key?(opts, :neuron_probe_snapshot)
    refute Keyword.has_key?(opts, :semantic_probe_snapshot)
  end

  test "memory-hit follow-up reaches real STM and neural stages without drafting another reply" do
    session = "memory-route-#{System.unique_integer([:positive, :monotonic])}"

    assert {:ok, si} = Continuity.run("what did the artist draw?", session)
    assert si.session_id == session
    assert si.sentence == "what did the artist draw?"
    assert si.response_text == nil
    assert si.response_meta == nil
    assert si.episode == nil

    assert Enum.any?(si.trace, fn
      %{stage: :neuron_priming} -> true
      _ -> false
    end)

    assert Enum.any?(si.trace, fn
      %{stage: :neuron_feedback} -> true
      _ -> false
    end)
  end

  test "invalid session never attempts to update the coordinator" do
    assert {:error, :invalid_input} = Continuity.run("hello", "")
    assert {:error, :invalid_input} = Continuity.run("hello", nil)
    assert {:error, :invalid_input} = Continuity.run(nil, "memory-route-session")
    assert {:error, :invalid_input} = Continuity.run("hello", String.duplicate("x", 129))
  end
end
