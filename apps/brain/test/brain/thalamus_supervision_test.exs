defmodule Brain.ThalamusSupervision_Test do
  use ExUnit.Case, async: false

  @moduledoc false

  test "Thalamus is a running singleton under the umbrella" do
    case Process.whereis(Brain) do
      nil -> start_supervised!(Brain)
      _pid -> :ok
    end

    pid = Process.whereis(Brain.Thalamus)

    assert is_pid(pid), """
    Brain.Thalamus is not running.

    This test enforces your umbrella topology preference: singleton Thalamus started
    by the root application (no per-app Application trees).
    """

    # Sanity: module responds to public API
    assert %{ofc_weight: _, acc_alpha: _} = Brain.Thalamus.get_params()
  end
end
