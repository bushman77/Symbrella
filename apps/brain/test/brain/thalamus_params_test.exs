defmodule Brain.ThalamusParams_Test do
  use ExUnit.Case, async: false

  @moduledoc false

  setup_all do
    # Ensure core Brain supervisor is present if your test environment expects it
    case Process.whereis(Brain) do
      nil -> start_supervised!(Brain)
      _pid -> :ok
    end

    # Thalamus must already be running as a singleton
    case Process.whereis(Brain.Thalamus) do
      nil ->
        flunk("""
        Brain.Thalamus is not running.

        These tests assume the singleton is already started under the umbrella root.
        """)

      pid when is_pid(pid) ->
        :ok
    end

    :ok
  end

  test "get_params reflects Application env defaults when no overrides set" do
    prev_w = Application.get_env(:brain, :thalamus_ofc_weight)
    prev_a = Application.get_env(:brain, :thalamus_acc_alpha)

    try do
      Application.put_env(:brain, :thalamus_ofc_weight, 0.42)
      Application.put_env(:brain, :thalamus_acc_alpha, 0.37)

      assert %{ofc_weight: 0.42, acc_alpha: 0.37} == Brain.Thalamus.get_params()
    after
      Application.put_env(:brain, :thalamus_ofc_weight, prev_w)
      Application.put_env(:brain, :thalamus_acc_alpha, prev_a)
    end
  end

  test "set_params accepts keyword list and clamps values" do
    :ok = Brain.Thalamus.set_params(ofc_weight: 1.7, acc_alpha: -0.5)
    params = Brain.Thalamus.get_params()
    assert_in_delta 1.0, params.ofc_weight, 1.0e-6
    assert_in_delta 0.0, params.acc_alpha, 1.0e-6
  end

  test "set_params accepts map and applies exactly" do
    :ok = Brain.Thalamus.set_params(%{ofc_weight: 0.25, acc_alpha: 0.75})
    params = Brain.Thalamus.get_params()
    assert_in_delta 0.25, params.ofc_weight, 1.0e-6
    assert_in_delta 0.75, params.acc_alpha, 1.0e-6
  end

  test "non-keyword list is ignored gracefully (regression for bad guards)" do
    # Capture current params
    params0 = Brain.Thalamus.get_params()

    # This used to explode when guards called Keyword.keyword?/1 at compile time.
    :ok = Brain.Thalamus.set_params([:not, :a, :keyword, :list])

    # Should be unchanged
    assert params0 == Brain.Thalamus.get_params()
  end
end
