defmodule Brain.CuriosityPolicyTest do
  use ExUnit.Case, async: true
  alias Brain.CuriosityPolicy

  test "high load and inhibition defer autonomous work without authorizing evidence" do
    assert %{allowed?: false, reason: :self_model_overloaded} =
      CuriosityPolicy.assess(:missing_link, %{cognitive_load: 0.95})
    assert %{allowed?: false} = CuriosityPolicy.assess(:two_hop, %{inhibition: 0.91})
    assert %{allowed?: true} =
      CuriosityPolicy.assess(:missing_link, %{inhibition: 0.45, cognitive_load: 0.2})
  end

  test "vigilance favours direct verification over speculative reasoning" do
    state = %{vigilance: 0.9, plasticity: 0.5, uncertainty: 0.6}
    direct = CuriosityPolicy.assess(:direct, state)
    speculative = CuriosityPolicy.assess(:two_hop, state)
    assert direct.adjustment > speculative.adjustment
  end

  test "learned preference is bounded, reversible and scoring cannot exceed 0..1" do
    sample = %{investigative_biases: %{v: 1, biases: %{missing_link: 0.12}}}
    {positive, %{learned_bias: 0.12}} = CuriosityPolicy.score(0.65, :missing_link, sample)
    {neutral, _} = CuriosityPolicy.score(0.65, :missing_link, %{})
    assert positive > neutral
    assert {1.0, _} = CuriosityPolicy.score(8.0, :missing_link, sample)
    assert {0.0, _} = CuriosityPolicy.score(-8.0, :missing_link, sample)
  end
end
