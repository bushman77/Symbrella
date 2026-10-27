defmodule Core.Semantic.CompositionLexicalTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{Event, Expression, Lexical, Proof, Reducer}

  @event_property {:arrow, :event, :truth}
  @modifier_type {:arrow, @event_property, @event_property}

  test "under takes table as a typed argument before it modifies an event" do
    assert {:ok, preposition} = Lexical.spatial_modifier("under")
    assert {:ok, landmark} = Lexical.landmark("table")
    assert preposition.type == {:arrow, :landmark_kind, @modifier_type}
    assert landmark.type == :landmark_kind

    term = {:apply, preposition.term, landmark.term}
    assert Expression.type_of(term) == {:ok, @modifier_type}
    assert {:ok, normal, steps} = Reducer.reduce(term)
    assert length(steps) == 1
    assert Expression.type_of(normal) == {:ok, @modifier_type}
    assert :ok = Proof.verify(%{
      initial: term, result: normal, steps: steps, result_type: @modifier_type
    })

    assert normal ==
      {:lambda, "P", @event_property,
       {:lambda, "e", :event,
        {:and,
         {:apply, {:var, "P"}, {:var, "e"}},
         {:event_spatial, :under, {:landmark_kind, :table}, {:var, "e"}}}}}
  end

  test "progressive events reduce the separate spatial lexical application" do
    assert {:ok, derivation} = Event.compose(%{
      predicate: "sleeping", actor: "cat",
      preposition: "under", landmark: "table"
    })

    assert {:apply, {:apply, _, {:landmark_kind, :table}}, _} =
      derivation.initial
    assert length(derivation.steps) == 5
    assert :ok = Proof.verify(derivation)
    assert :ok = Event.verify(derivation)
    assert derivation.semantics.spatial == %{
      relation: :under, landmark_kind: :table,
      reading: :locative, arrival: :not_established
    }
    assert derivation.semantics.event_existence == :not_asserted
  end

  test "typed spatial arguments reject invalid vocabulary and mismatches" do
    assert Lexical.spatial_modifier("arrived") == :unsupported
    assert Lexical.landmark("mountain") == :unsupported
    assert Expression.type_of({:landmark_kind, :mountain}) ==
      {:error, :unapproved_event_symbol}

    assert {:ok, preposition} = Lexical.spatial_modifier("under")
    assert {:error, :incompatible_application} =
      Expression.type_of({:apply, preposition.term, {:var, "e"}},
        %{"e" => :event})

    assert {:error, :unapproved_event_symbol} =
      Expression.type_of({:event_spatial, :arrived, :table, {:var, "e"}},
        %{"e" => :event})

    assert :unsupported = Event.compose(%{
      predicate: "flying", actor: "bird",
      preposition: "under", landmark: "table"
    })
  end

  test "substitution avoids capture in lexical spatial landmark arguments" do
    term =
      {:lambda, "e", :event,
       {:event_spatial, :under, {:var, "landmark"}, {:var, "e"}}}

    assert {:lambda, "e_1", :event,
            {:event_spatial, :under, {:var, "e"}, {:var, "e_1"}}} =
      Reducer.substitute(term, "landmark", {:var, "e"})
  end
end
