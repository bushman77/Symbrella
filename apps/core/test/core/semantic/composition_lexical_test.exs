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

    assert {:apply, {:apply, _, {:landmark_kind, :table}},
            {:apply,
             {:apply, _, {:actor_kind, :cat}},
             {:apply, _, {:action_kind, :sleep}}}} =
      derivation.initial
    assert length(derivation.steps) == 7
    assert :ok = Proof.verify(derivation)
    assert :ok = Event.verify(derivation)
    assert derivation.semantics.spatial == %{
      relation: :under, landmark_kind: :table,
      reading: :locative, arrival: :not_established
    }
    assert derivation.semantics.event_existence == :not_asserted
  end

  test "actor nouns and progressive verbs compose independently as typed arguments" do
    assert {:ok, actor} = Lexical.actor("bird")
    assert {:ok, action} = Lexical.action("flying")
    assert {:ok, actor_function} = Lexical.actor_modifier()
    assert {:ok, progressive_function} = Lexical.progressive_event()

    assert actor.type == :actor_kind
    assert action.type == :action_kind
    assert actor_function.type ==
      {:arrow, :actor_kind, {:arrow, @event_property, @event_property}}
    assert progressive_function.type == {:arrow, :action_kind, @event_property}

    subject_application = {:apply, actor_function.term, actor.term}
    predicate_application = {:apply, progressive_function.term, action.term}

    assert {:ok, subject_normal, subject_steps} =
      Reducer.reduce(subject_application)
    assert length(subject_steps) == 1
    assert Expression.type_of(subject_normal) == {:ok, @modifier_type}

    assert {:ok, predicate_normal, predicate_steps} =
      Reducer.reduce(predicate_application)
    assert length(predicate_steps) == 1
    assert Expression.type_of(predicate_normal) == {:ok, @event_property}
    assert predicate_normal ==
      {:lambda, "e", :event,
       {:and, {:event_action, {:action_kind, :fly}, {:var, "e"}},
        {:event_aspect, :progressive, {:var, "e"}}}}

    full = {:apply, subject_normal, predicate_normal}
    assert {:ok, combined, _steps} = Reducer.reduce(full)
    assert Expression.type_of(combined) == {:ok, @event_property}
  end

  test "typed actor and verb vocabulary rejects unsupported lexical meanings" do
    assert Lexical.actor("robot") == :unsupported
    assert Lexical.action("dancing") == :unsupported
    assert Expression.type_of({:actor_kind, :robot}) ==
      {:error, :unapproved_event_symbol}
    assert Expression.type_of({:action_kind, :dance}) ==
      {:error, :unapproved_event_symbol}

    assert {:ok, progressive_function} = Lexical.progressive_event()
    assert {:ok, actor} = Lexical.actor("cat")
    assert {:error, :incompatible_application} =
      Expression.type_of({:apply, progressive_function.term, actor.term})

    assert :unsupported = Event.compose(%{
      predicate: "dancing", actor: "cat",
      preposition: "under", landmark: "table"
    })
  end

  test "actor and verb substitutions rename bound variables to prevent capture" do
    actor = {:lambda, "e", :event,
      {:event_actor_kind, {:var, "actor"}, {:var, "e"}}}
    action = {:lambda, "e", :event,
      {:event_action, {:var, "action"}, {:var, "e"}}}

    assert {:lambda, "e_1", :event,
            {:event_actor_kind, {:var, "e"}, {:var, "e_1"}}} =
      Reducer.substitute(actor, "actor", {:var, "e"})

    assert {:lambda, "e_1", :event,
            {:event_action, {:var, "e"}, {:var, "e_1"}}} =
      Reducer.substitute(action, "action", {:var, "e"})
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
