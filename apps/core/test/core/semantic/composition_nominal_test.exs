defmodule Core.Semantic.CompositionNominalTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{Expression, Lexical, Nominal, Proof, Reducer}

  @property_type {:arrow, :entity, :truth}

  test "two independent modifiers reduce to an intersected nominal property" do
    assert {:ok, proof} = Nominal.compose("bird", ["small", "black"])
    assert {:apply, _, {:apply, _, _}} = proof.initial
    assert proof.result_type == @property_type
    assert length(proof.steps) == 4
    assert :ok = Proof.verify(proof)
    assert Expression.type_of(proof.result) == {:ok, @property_type}
    assert Reducer.step(proof.result) == :normal

    assert proof.result ==
      {:lambda, "x", :entity,
       {:and,
        {:and,
         {:predicate, :bird, {:var, "x"}},
         {:predicate, :black, {:var, "x"}}},
        {:predicate, :small, {:var, "x"}}}}
  end

  test "modifier order is represented compositionally instead of hard-coded" do
    assert {:ok, first} = Nominal.compose("dog", ["small", "white"])
    assert {:ok, second} = Nominal.compose("dog", ["white", "small"])
    assert :ok = Proof.verify(first)
    assert :ok = Proof.verify(second)
    assert first.result != second.result
    assert first.result_type == second.result_type
    assert first.initial != second.initial
  end

  test "existing single adjective noun meaning preserves its original lambda term" do
    assert {:ok, noun} = Expression.noun("cat")
    assert {:ok, adjective} = Expression.adjective("black")
    assert {:ok, proof} = Nominal.compose("cat", ["black"])

    assert proof.initial == {:apply, adjective, noun}
    assert :ok = Proof.verify(proof)
    assert proof.result_type == @property_type

    assert {:ok, noun_entry} = Lexical.nominal_property("cat")
    assert {:ok, adjective_entry} = Lexical.nominal_modifier("black")
    assert noun_entry.term == noun
    assert adjective_entry.term == adjective
    assert noun_entry.type == @property_type
    assert adjective_entry.type ==
      {:arrow, @property_type, @property_type}
  end

  test "zero modifiers preserves a nominal property without asserting existence" do
    assert {:ok, proof} = Nominal.compose("bird", [])
    assert proof.steps == []
    assert :ok = Proof.verify(proof)
    assert Expression.type_of(proof.result) == {:ok, @property_type}
    assert proof.result == proof.initial
  end

  test "unsupported, nonlexical, and over-budget input fails closed" do
    assert Nominal.compose("tree", ["black"]) == :unsupported
    assert Nominal.compose("bird", ["enormous"]) == :unsupported
    assert Nominal.compose("bird", ["small", "black", "white"]) == :unsupported
    assert Nominal.compose("bird", ["small", 123]) == :unsupported
    assert Nominal.compose("bird", "small") == :unsupported
    assert Nominal.compose(nil, ["small"]) == :unsupported

    assert Lexical.nominal_property("tree") == :unsupported
    assert Lexical.nominal_modifier("enormous") == :unsupported

    assert {:ok, noun_entry} = Lexical.nominal_property("bird")
    assert {:ok, adjective_entry} = Lexical.nominal_modifier("black")
    assert Expression.type_of({:apply, noun_entry.term, adjective_entry.term}) ==
      {:error, :incompatible_application}
  end

  test "forged reduction steps are rejected" do
    assert {:ok, proof} = Nominal.compose("bird", ["small", "black"])
    [first | rest] = proof.steps
    forged = %{proof | steps: [%{first | after: {:var, "forged"}} | rest]}
    assert Proof.verify(forged) == {:error, :invalid_derivation}
  end
end
