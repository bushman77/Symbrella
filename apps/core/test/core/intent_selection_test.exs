defmodule Core.Intent.SelectionTest do
  use ExUnit.Case, async: true
  alias Core.Intent.Selection

  test "greet" do
    si = %{sentence: "hey there", tokens: ["hey", "there"]}
    {si2, res} = Selection.select(si)
    assert si2.intent == :greet
    assert res.intent == :greet
    assert si2.confidence > 0.3
  end

  test "define" do
    si = %{sentence: "what is hippocampus", tokens: ["what", "is", "hippocampus"]}
    {si2, res} = Selection.select(si)
    assert si2.intent == :define
    assert res.keyword == "hippocampus"
    assert si2.confidence > 0.3
  end

  test "translate" do
    si = %{sentence: "how do you say hello in french", tokens: ~w(how do you say hello in french)}
    {si2, res} = Selection.select(si)
    assert si2.intent == :translate
    assert si2.confidence > 0.3
  end

  test "fallback ask_info" do
    si = %{sentence: "why is the sky blue", tokens: ~w(why is the sky blue)}
    {si2, res} = Selection.select(si)
    assert si2.intent == :ask_info
    assert si2.confidence > 0.1
  end

  test "brain introspect" do
    si = %{sentence: "show Brain.Hippocampus state", tokens: ~w(show brain.hippocampus state)}
    {si2, _} = Selection.select(si)
    assert si2.intent == :brain_introspect
  end

  test "none below threshold" do
    si = %{sentence: "hmm", tokens: ["hmm"]}
    {si2, _} = Selection.select(si, threshold: 0.5)
    assert si2.intent == :none
  end
end

