defmodule Brain.DashboardStatusTest do
  use ExUnit.Case, async: false

  alias Brain.DLPFC
  alias Brain.OFC

  setup do
    ensure_started!(OFC)
    ensure_started!(DLPFC)

    :ok = OFC.reset()
    :ok = DLPFC.reset()

    :ok
  end

  test "OFC returns compact dashboard status" do
    assert %{
             region: :ofc,
             status: :up,
             mood: nil,
             mood_last_ms: nil,
             params: %{risk_weight: _, novelty_weight: _, mood_cap: _, mood_weights: _},
             telemetry_handlers: %{curiosity: _, mood: _}
           } = OFC.status()
  end

  test "DLPFC returns compact dashboard status" do
    assert %{
             region: :dlpfc,
             status: :up,
             opts: %{},
             stats: %{},
             last_probe_id: nil,
             last_probe: nil,
             telemetry_handlers: %{thalamus: _, curiosity: _}
           } = DLPFC.status()
  end

  test "pure semantic-control regions report dashboard availability" do
    assert %{region: :frontal, status: :available, mode: :pure_group} = Brain.Frontal.status()
    assert %{region: :prefrontal, status: :available, mode: :pure} = Brain.Prefrontal.status()
    assert %{region: :vmpfc, status: :available, mode: :pure} = Brain.VmPFC.status()
    assert %{region: :dmpfc, status: :available, mode: :pure} = Brain.DmPFC.status()
    assert %{region: :fpc, status: :available, mode: :pure} = Brain.FPC.status()
    assert %{region: :salience, status: :available, mode: :pure} = Brain.Salience.status()

    assert %{region: :basal_ganglia, status: :available, mode: :pure} =
             Brain.BasalGanglia.status()
  end

  defp ensure_started!(mod) do
    case Process.whereis(mod) do
      pid when is_pid(pid) -> :ok
      nil -> start_supervised!(mod)
    end
  end
end
