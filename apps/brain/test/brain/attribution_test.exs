defmodule Brain.AttributionTest do
  use ExUnit.Case, async: true

  alias Brain.Attribution

  test "classifies phase 3 target examples" do
    assert Attribution.target("you are wrong") == :assistant
    assert Attribution.target("I feel lost") == :user
    assert Attribution.target("he insulted me") == :other
    assert Attribution.target("the system is unstable") == :system
  end

  test "classifies configured self names as self" do
    assert Attribution.target("Symbrella is drifting", self_names: ["symbrella"]) == :self
  end

  test "returns inspectable classification metadata" do
    result = Attribution.classify("the world is unstable")

    assert result.target == :world
    assert result.source == :world_term
    assert result.confidence == 0.7
    assert %{term: "world", source: :world_term, confidence: 0.7} in result.evidence
    assert result.version == 1
  end

  test "classification includes source confidence and evidence" do
    result = Attribution.classify("Symbrella is unstable", self_names: ["symbrella"])

    assert result.target == :self
    assert result.source == :self_name
    assert result.confidence == 1.0
    assert %{term: "symbrella", source: :self_name, confidence: 1.0} in result.evidence
    assert result.version == 1
  end

  test "unknown classification is explicit" do
    assert Attribution.classify("neutral words only") == %{
             target: :unknown,
             source: :none,
             confidence: 0.0,
             evidence: [],
             version: 1
           }
  end
end
