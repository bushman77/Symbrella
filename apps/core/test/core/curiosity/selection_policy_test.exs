defmodule Core.Curiosity.SelectionPolicyTest do
  use ExUnit.Case, async: true
  alias Core.Curiosity.SelectionPolicy
  alias Brain.CuriosityLearning

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
    assert active.selected_strategy == :direct
    assert active.selected_learned_bias == 0.12
    assert shadow2.selected_strategy == :two_hop
    assert shadow2.selected_learned_bias == -0.12
    assert shadow2.shadow_strategy == :direct
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
    assert choice.selected_strategy == :two_hop
    assert choice.selected_learned_bias == 0.12
  end

  test "acquisition learning changes an eligible action hint, not proof authority" do
    candidates = [
      %{issue_id: 1, priority: 0.60, strategy: :missing_link},
      %{issue_id: 2, priority: 0.55, strategy: :acquisition}
    ]

    no_learning = SelectionPolicy.choose(candidates, %{}, active?: true)
    assert no_learning.selected.issue_id == 1

    learned = %{investigative_biases: %{
      v: 1, biases: %{acquisition: 0.12, missing_link: -0.10}
    }}
    choice = SelectionPolicy.choose(candidates, learned, active?: true)
    assert choice.selected.issue_id == 2
    assert choice.selected_strategy == :acquisition
    assert choice.selected_learned_bias == 0.12
  end

  test "a proof-authorized outcome causally changes the next eligible selection" do
    queue = [
      %{issue_id: 10, priority: 0.60, strategy: :missing_link},
      %{issue_id: 11, priority: 0.59, strategy: :direct}
    ]

    before = SelectionPolicy.choose(queue, %{}, active?: true)
    assert before.selected.issue_id == 10

    # The completed receipt is corroborated by the persisted issue's proof
    # lineage. No selection decision is allowed to create either record.
    projection =
      CuriosityLearning.project(
        [%{
          issue_id: 99, completed_at: :done, status: "resolved",
          origin: "verified_direct_proof", acquisition: "not_attempted"
        }],
        %{99 => %{status: "resolved",
          resolution: %{"reasoning_rule" => "direct_population_comparison", "proof_id" => 42}}}
      )

    assert projection.success_count == 1
    assert projection.biases.direct > projection.biases.missing_link

    after_learning =
      SelectionPolicy.choose(queue, %{investigative_biases: projection}, active?: true)
    assert after_learning.selected.issue_id == 11
    assert after_learning.selected_strategy == :direct
    assert after_learning.selected_learned_bias == projection.biases.direct

    # Passive replay reports the improved proposal but retains legacy work.
    shadow = SelectionPolicy.choose(queue, %{investigative_biases: projection})
    assert shadow.mode == :shadow
    assert shadow.selected.issue_id == 10
    assert shadow.shadow_selected.issue_id == 11
    assert shadow.selected_learned_bias == projection.biases.missing_link
  end

  test "empty queue cannot be made active by preferences" do
    assert %{status: :idle, selected: nil} =
      SelectionPolicy.choose([], %{uncertainty: 1.0}, active?: true)
  end
end
