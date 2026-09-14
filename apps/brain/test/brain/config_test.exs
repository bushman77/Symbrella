defmodule Brain.ConfigTest do
  use ExUnit.Case, async: false

  alias Brain.Config

  @sentinel :__missing_env__

  setup do
    keys = [
      {:brain, :capacity},
      {:brain, :gate_threshold},
      {:brain, :decay_ms},
      {:brain, :source_boosts},
      {:brain, :prefer_sources},
      {:brain, :wm},
      {:brain, :assistant},
      {:symbrella, :assistant}
    ]

    old_values =
      Map.new(keys, fn {app, key} ->
        {{app, key}, Application.get_env(app, key, @sentinel)}
      end)

    on_exit(fn ->
      Enum.each(old_values, fn
        {{app, key}, @sentinel} -> Application.delete_env(app, key)
        {{app, key}, value} -> Application.put_env(app, key, value)
      end)
    end)

    :ok
  end

  test "wm/0 honors supported application config values" do
    Application.put_env(:brain, :capacity, 4)
    Application.put_env(:brain, :gate_threshold, 0.31)
    Application.put_env(:brain, :source_boosts, %{curiosity: 0.12})

    cfg = Config.wm()

    assert cfg.capacity == 4
    assert_in_delta cfg.gate_threshold, 0.31, 1.0e-12
    assert cfg.source_boosts == %{curiosity: 0.12}
  end

  test "Brain initializes WM config from the authoritative helper" do
    Application.put_env(:brain, :capacity, 5)
    Application.put_env(:brain, :gate_threshold, 0.29)

    assert {:ok, state} = Brain.init(:ok)
    assert state.wm_cfg.capacity == 5
    assert_in_delta state.wm_cfg.gate_threshold, 0.29, 1.0e-12
  end

  test "wm/0 falls back safely when optional config is missing" do
    Application.delete_env(:brain, :decay_ms)
    defaults = Config.wm_defaults()

    cfg = Config.wm()

    assert cfg.decay_ms == defaults.decay_ms
    assert is_integer(cfg.capacity)
    assert is_number(cfg.gate_threshold)
  end

  test "effective WM config includes BasalGanglia decision knobs" do
    cfg = Config.wm()

    for key <- [
          :capacity,
          :gate_threshold,
          :lifg_min_score,
          :prefer_sources,
          :disprefer_sources,
          :source_boosts,
          :dup_penalty,
          :cooldown_ms,
          :fullness_penalty_mult,
          :boost_threshold,
          :boost_threshold_pref,
          :block_threshold,
          :block_threshold_disprefer
        ] do
      assert Map.has_key?(cfg, key), "missing #{inspect(key)}"
    end
  end

  test "assistant identity uses :symbrella config with :brain as legacy fallback" do
    Application.put_env(:brain, :assistant, name: "Legacy", aliases: ["legacy"])
    Application.put_env(:symbrella, :assistant, name: "Nova", norm: "nova", aliases: ["Sym"])

    assert Config.assistant() == %{name: "Nova", norm: "nova", aliases: ["sym"]}
    assert Config.assistant_match?("Sym")
    refute Config.assistant_match?("legacy")
  end
end
