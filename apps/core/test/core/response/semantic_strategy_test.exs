defmodule Core.Response.SemanticStrategyTest do
  use ExUnit.Case, async: true

  alias Core.Response.SemanticStrategy
  alias Core.Semantic.ProvisionalInterpretation

  defp question(sentence, noun) do
    tokens =
      sentence
      |> String.downcase()
      |> String.trim_trailing("?")
      |> String.split()
      |> Enum.with_index()
      |> Enum.map(fn {word, index} ->
        %{index: index, phrase: word, n: 1}
      end)

    %{
      sentence: sentence,
      source: :test,
      intent: :ask,
      tokens: tokens,
      trace: [],
      active_cells: [%{id: "#{noun}|noun|1", norm: noun, pos: "noun"}],
      sentence_meaning: %{status: :not_yet_interpreted, kind: :unknown}
    }
    |> ProvisionalInterpretation.attach()
  end

  test "a location question generalizes across unrelated entities without an answer" do
    for noun <- ~w(bird wallet server spacecraft) do
      si = question("Where is the #{noun}?", noun)
      plan = SemanticStrategy.select(si)

      assert %{
        operation: :answer_location,
        realization_family: :direct_relation,
        status: :needs_evidence,
        realizer: nil,
        target: ^noun,
        relation: :location,
        missing: [:referent_binding, :location],
        factual_authority?: false
      } = plan

      assert :not_applicable =
        SemanticStrategy.realize(
          plan,
          si.sentence,
          %{risk_bucket: :low, intent: :ask_info},
          %{action: :answer},
          si
        )
    end
  end

  test "an injected or stale provisional interpretation cannot authorize a strategy" do
    si = question("Where is the bird?", "bird")
    plan = SemanticStrategy.select(si)

    changed = %{si | sentence: "Where is the wallet?"}
    assert SemanticStrategy.select(changed) == :not_applicable

    forged = put_in(si, [:provisional_interpretation, :target, :lemma], "wallet")
    assert SemanticStrategy.select(forged) == :not_applicable

    assert :not_applicable =
      SemanticStrategy.realize(
        plan,
        "Where is the wallet?",
        %{risk_bucket: :low, intent: :ask_info},
        %{action: :answer},
        si
      )
  end

  test "guardrails and unsupported action do not realize unresolved strategies" do
    si = question("Where is the server?", "server")
    plan = SemanticStrategy.select(si)

    assert :not_applicable =
      SemanticStrategy.realize(
        plan, si.sentence, %{risk_bucket: :high, intent: :ask_info},
        %{action: :answer}, si
      )

    assert :not_applicable =
      SemanticStrategy.realize(
        plan, si.sentence, %{risk_bucket: :low, intent: :ask_info},
        %{action: :suppress}, si
      )
  end
end
