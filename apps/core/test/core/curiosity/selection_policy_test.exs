defmodule Core.Curiosity.SelectionPolicyTest do
  use ExUnit.Case, async: true
  alias Core.Curiosity.SelectionPolicy

  test "shadow mode reports but never changes legacy selection" do
    candidates = [
      %{issue_id: 1, priority: 0.50, strategy: :direct},
      %{issue_id: 2, priority: 0.55, strategy: :two_hop}
    ]
    model = %{vigilance: 0.9,
      investigative_biases: %{v: 1, biases: %{direct: 0.12, two_hop: -0.12}}}

    shadow = SelectionPolicy.choose(candidates, model)
    assert shadow.mode == :shadow
    assert shadow.selected.issue_id == 1
    assert shadow.shadow_selected.issue_id == 1

    reversed = Enum.reverse(candidates)
    shadow2 = SelectionPolicy.choose(reversed, model)
    assert shadow2.selected.issue_id == 2
    assert shadow2.shadow_selected.issue_id == 1

    active = SelectionPolicy.choose(reversed, model, active?: true)
    assert active.selected.issue_id == 1
  end

  test "hard cognitive gate applies only when active" do
    candidates = [%{issue_id: 99, priority: 0.9}]
    load = %{cognitive_load: 0.95}
    assert %{status: :ready, selected: %{issue_id: 99}, would_defer?: true} =
      SelectionPolicy.choose(candidates, load)
    assert %{status: :deferred, selected: nil,
             reason: :self_model_overloaded} =
      SelectionPolicy.choose(candidates, load, active?: true)
  end

  test "learned two-hop preference can change priority without granting proof" do
    candidates = [
      %{issue_id: 10, priority: 0.58, strategy: :missing_link},
      %{issue_id: 11, priority: 0.55, strategy: :two_hop}
    ]
    state = %{plasticity: 0.8, vigilance: 0.25,
      investigative_biases: %{v: 1, biases: %{two_hop: 0.12}}}
    choice = SelectionPolicy.choose(candidates, state, active?: true)
    assert choice.selected.issue_id == 11
    assert choice.legacy_selected.issue_id == 10
  end

  test "empty queue cannot be made active by preferences" do
    assert %{status: :idle, selected: nil} =
      SelectionPolicy.choose([], %{uncertainty: 1.0}, active?: true)
  end
end
