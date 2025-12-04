defmodule Brain.Pipeline.LIFGStage1SelfNameTest do
  use ExUnit.Case, async: false

  alias Brain.Pipeline.LIFGStage1

  setup do
    old_names = Application.get_env(:brain, :self_names)
    old_ep = Application.get_env(:brain, :episodes_mode)

    Application.put_env(:brain, :self_names, ["symbrella"])
    Application.put_env(:brain, :episodes_mode, :off)

    on_exit(fn ->
      if is_nil(old_names),
        do: Application.delete_env(:brain, :self_names),
        else: Application.put_env(:brain, :self_names, old_names)

      if is_nil(old_ep),
        do: Application.delete_env(:brain, :episodes_mode),
        else: Application.put_env(:brain, :episodes_mode, old_ep)
    end)

    :ok
  end

  defp state0 do
    %{
      wm: [],
      wm_last_ms: nil,
      wm_cfg: %{capacity: 7, decay_ms: 30_000},
      attention: %{}
    }
  end

  test "token path: hit stamps state.attention and out.audit (case/punct tolerant)" do
    si = %{
      sentence: "hey symbrella!!!",
      tokens: [
        %{index: 0, phrase: "hey"},
        %{index: 1, phrase: "Symbrella!!!"}
      ],
      trace: []
    }

    {{:ok, out}, st1} = LIFGStage1.run(si, [], [], state0())

    assert get_in(out, [:audit, :self_name, :hit?]) == true
    assert get_in(out, [:audit, :self_name, :match]) == "symbrella"
    assert get_in(st1, [:attention, :self_name, :hit?]) == true
    assert get_in(st1, [:attention, :self_name, :match]) == "symbrella"
  end

  test "sentence-only path: hit still works (no tokens)" do
    si = %{
      sentence: "yo, Symbrella.",
      tokens: [],
      trace: []
    }

    {{:ok, out}, st1} = LIFGStage1.run(si, [], [], state0())

    assert get_in(out, [:audit, :self_name, :hit?]) == true
    assert get_in(out, [:audit, :self_name, :match]) == "symbrella"
    assert get_in(st1, [:attention, :self_name, :hit?]) == true
    assert get_in(st1, [:attention, :self_name, :match]) == "symbrella"
  end

  test "no name → hit? false (but audit key still present)" do
    si = %{
      sentence: "hey there",
      tokens: [
        %{index: 0, phrase: "hey"},
        %{index: 1, phrase: "there"}
      ],
      trace: []
    }

    {{:ok, out}, st1} = LIFGStage1.run(si, [], [], state0())

    assert Map.has_key?(out.audit, :self_name)
    assert get_in(out, [:audit, :self_name, :hit?]) == false
    assert get_in(out, [:audit, :self_name, :match]) in [nil, ""]

    assert get_in(st1, [:attention, :self_name, :hit?]) == false
  end

  test "respects config list (does not hardcode names)" do
    # baseline: only "symbrella" is configured (from setup)
    si0 = %{
      sentence: "hey Bradley!!",
      tokens: [%{index: 0, phrase: "hey"}, %{index: 1, phrase: "Bradley!!"}],
      trace: []
    }

    {{:ok, out0}, _st0} = LIFGStage1.run(si0, [], [], state0())
    assert get_in(out0, [:audit, :self_name, :hit?]) == false

    # now add "bradley" and re-run
    Application.put_env(:brain, :self_names, ["symbrella", "bradley"])

    si1 = %{
      sentence: "hey bradley!!",
      tokens: [%{index: 0, phrase: "hey"}, %{index: 1, phrase: "BRADLEY!!"}],
      trace: []
    }

    {{:ok, out1}, st1} = LIFGStage1.run(si1, [], [], state0())

    assert get_in(out1, [:audit, :self_name, :hit?]) == true
    assert get_in(out1, [:audit, :self_name, :match]) == "bradley"
    assert get_in(st1, [:attention, :self_name, :hit?]) == true
    assert get_in(st1, [:attention, :self_name, :match]) == "bradley"

    # restore baseline for any later tests in this module
    Application.put_env(:brain, :self_names, ["symbrella"])
  end
end
