defmodule Core.Social.BrainObservationTest do
  use ExUnit.Case, async: true

  alias Core.Social.BrainObservation

  defp candidate_si do
    %{
      source: :colony_external,
      experience_origin: :colony_external,
      active_cells: [%{id: "bird|noun|12", pos: "noun", token_index: 0}],
      lifg_choices: [%{id: "bird|noun|12", token_index: 0, weak?: false}],
      sentence: "Birds are free."
    }
  end

  test "disabled by default and never calls an activator" do
    result =
      BrainObservation.observe(candidate_si(),
        brain_available?: true,
        activator: fn _, _ -> flunk("unexpected activation") end
      )

    assert result.status == :disabled
    assert result.stimuli_issued == 0
    assert result.memory_written? == false
    assert result.synapses_modified? == false
  end

  test "the explicit observation reaches real lexical neuron IDs only" do
    parent = self()

    result =
      BrainObservation.observe(candidate_si(),
        enabled?: true,
        brain_available?: true,
        activator: fn ids, payload ->
          send(parent, {:neurons, ids, payload})
          :ok
        end
      )

    assert result.status == :stimuli_issued
    assert result.source == :colony_external
    assert result.stimuli_issued == 1
    assert result.maximum_delta <= 0.03
    assert result.factual_authority? == false

    assert_receive {:neurons, ["bird|noun|12"],
                    %{via: :colony_external_observation, delta: delta}}
    assert delta > 0.0
    assert delta <= 0.03
  end

  test "never stimulates if the actual Brain runtime is unavailable" do
    result =
      BrainObservation.observe(candidate_si(),
        enabled?: true,
        brain_available?: false,
        activator: fn _, _ -> flunk("no Brain runtime") end
      )

    assert result.status == :brain_unavailable
    assert result.stimuli_issued == 0
  end

  test "does not treat chat or untrusted source labels as Colony observations" do
    for source <- [:user, :prod, :external_unverified] do
      si = %{candidate_si() | source: source}

      assert %{status: :unsupported_source, stimuli_issued: 0} =
               BrainObservation.observe(si,
                 enabled?: true,
                 brain_available?: true,
                 activator: fn _, _ -> flunk("wrong source stimulated") end
               )
    end

    si = %{candidate_si() | experience_origin: :user_turn}

    assert %{status: :unsupported_source} =
             BrainObservation.observe(si,
               enabled?: true,
               brain_available?: true,
               activator: fn _, _ -> flunk("wrong provenance stimulated") end
             )
  end

  test "unattested and fallback lexical IDs cannot stimulate neurons" do
    si = %{candidate_si() | active_cells: []}

    assert %{status: :no_attested_targets, stimuli_issued: 0} =
             BrainObservation.observe(si,
               enabled?: true,
               brain_available?: true,
               activator: fn _, _ -> flunk("unattested neuron") end
             )

    si = %{candidate_si() | lifg_choices: [%{id: "bird|noun|fallback", token_index: 0}]}

    assert %{status: :no_attested_targets, stimuli_issued: 0} =
             BrainObservation.observe(si,
               enabled?: true,
               brain_available?: true,
               activator: fn _, _ -> flunk("fallback neuron") end
             )
  end

  test "caps repeated external lexical stimulation to four neurons per sentence" do
    entries =
      for i <- 1..12 do
        %{id: "item#{i}|noun|1", token_index: i}
      end

    si = %{candidate_si() | active_cells: entries, lifg_choices: entries}
    parent = self()

    result =
      BrainObservation.observe(si,
        enabled?: true,
        brain_available?: true,
        activator: fn ids, payload ->
          send(parent, {:stimulated, ids, payload})
          :ok
        end
      )

    assert result.stimuli_issued == 4

    messages =
      Stream.repeatedly(fn ->
        receive do
          {:stimulated, ids, payload} -> {:ok, ids, payload}
        after
          10 -> :done
        end
      end)
      |> Enum.take_while(&(&1 != :done))

    assert Enum.sum(Enum.map(messages, fn {:ok, ids, _} -> length(ids) end)) == 4
    assert Enum.all?(messages, fn {:ok, _, %{delta: delta}} -> delta <= 0.03 end)
  end
end
