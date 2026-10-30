defmodule Core.Semantic.CompositionPastTenseExpressionTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.Composition.{Expression, Lexical, Proof, Reducer}

  @event_property {:arrow, :event, :truth}

  defp past_operator do
    {:lambda, "action", :action_kind,
     {:lambda, "e", :event,
      {:and,
       {:event_action, {:var, "action"}, {:var, "e"}},
       {:event_tense, :past, {:var, "e"}}}}}
  end

  test "past tense is a distinct, typed event predicate" do
    assert {:ok, {:arrow, :action_kind, @event_property}} =
             Expression.type_of(past_operator())

    assert {:ok, :truth} =
             Expression.type_of({:event_tense, :past, {:var, "e"}}, %{"e" => :event})

    refute Expression.type_of({:event_tense, :future, {:var, "e"}},
             %{"e" => :event}) == {:ok, :truth}

    refute Expression.type_of({:event_tense, :past, {:var, "x"}},
             %{"x" => :entity}) == {:ok, :truth}
  end

  test "past event terms reduce and their derivation replays" do
    initial = {:apply, past_operator(), {:action_kind, :sit}}
    assert {:ok, @event_property} = Expression.type_of(initial)
    assert {:ok, result, steps} = Reducer.reduce(initial)
    assert length(steps) > 0
    assert result ==
             {:lambda, "e", :event,
              {:and,
               {:event_action, {:action_kind, :sit}, {:var, "e"}},
               {:event_tense, :past, {:var, "e"}}}}
    assert :ok = Proof.verify(%{
      initial: initial,
      steps: steps,
      result: result,
      result_type: @event_property
    })
  end

  test "lexical past operator produces a formally verified property" do
    assert {:ok, %{term: operator, type: {:arrow, :action_kind, @event_property}}} =
             Lexical.past_event()

    initial = {:apply, operator, {:action_kind, :sit}}
    assert {:ok, result, steps} = Reducer.reduce(initial)
    assert {:ok, @event_property} = Expression.type_of(result)
    assert :ok = Proof.verify(%{
      initial: initial,
      steps: steps,
      result: result,
      result_type: @event_property
    })

    assert result ==
             {:lambda, "e", :event,
              {:and,
               {:event_action, {:action_kind, :sit}, {:var, "e"}},
               {:event_tense, :past, {:var, "e"}}}}
    assert :unsupported = Lexical.action("sat")
  end

  test "substitution descends into past tense event terms" do
    term = {:event_tense, :past, {:var, "event"}}

    assert Reducer.substitute(term, "event", {:var, "replacement"}) ==
             {:event_tense, :past, {:var, "replacement"}}
  end
end
