# apps/brain/test/brain/pfc_self_name_policy_test.exs
defmodule Brain.PFCSelfNamePolicyTest do
  use ExUnit.Case, async: true

  alias Brain.PFC

  setup do
    old = Application.get_env(:brain, :self_names)
    Application.put_env(:brain, :self_names, ["symbrella"])

    on_exit(fn ->
      if is_nil(old),
        do: Application.delete_env(:brain, :self_names),
        else: Application.put_env(:brain, :self_names, old)
    end)

    :ok
  end

  test "self-name hit makes policy slightly more permissive (mt + min_score) and more eager (boost)" do
    goal = %{intent: :greet, keyword: "", confidence: 0.80, at_ms: 0}
    wm = %{wm: [], cfg: %{capacity: 7}}
    meta = %{conf: 1.0, conflict: 0.0, surprise: 0.0}

    si_hit = %{
      sentence: "Hey, Symbrella!!!",
      tokens: [
        %{index: 0, phrase: "hey"},
        %{index: 1, phrase: "Symbrella"}
      ]
    }

    si_miss = %{
      sentence: "hey there",
      tokens: [
        %{index: 0, phrase: "hey"},
        %{index: 1, phrase: "there"}
      ]
    }

    pol_hit = PFC.__test_compute_policy__(si_hit, goal, wm, meta)
    pol_miss = PFC.__test_compute_policy__(si_miss, goal, wm, meta)

    mt_hit = Keyword.fetch!(pol_hit, :margin_threshold)
    mt_miss = Keyword.fetch!(pol_miss, :margin_threshold)

    min_hit = Keyword.fetch!(pol_hit, :lifg_min_score)
    min_miss = Keyword.fetch!(pol_miss, :lifg_min_score)

    boost_hit = Keyword.fetch!(pol_hit, :base_boost)
    boost_miss = Keyword.fetch!(pol_miss, :base_boost)

    assert mt_hit < mt_miss
    assert min_hit < min_miss
    assert boost_hit >= boost_miss
  end
end
