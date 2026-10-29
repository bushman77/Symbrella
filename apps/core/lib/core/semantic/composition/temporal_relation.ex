defmodule Core.Semantic.Composition.TemporalRelation do
  @moduledoc """
  Pure, typed temporal composition for two independently verified EVENT
  PROPERTIES. This is a formal building block, not a sentence parser.

  Given P and Q of type event -> truth, a supplied :before connective
  constructs:

      λe1.λe2. P(e1) ∧ Q(e2) ∧ precedes(e1,e2)

  :after reverses only the final relation:

      λe1.λe2. P(e1) ∧ Q(e2) ∧ precedes(e2,e1)

  P and Q are in *surface order*. Neither the typed proof nor the caller's
  connective argument establishes that a speaker actually used the word,
  that the events happened, or that the two actors are one individual.
  Independent witnessed sentence structure must authorize use in discourse.
  """

  alias Core.Semantic.Composition.{Event, Expression, Proof, Reducer}

  @event_property {:arrow, :event, :truth}
  @relation_property {:arrow, :event, {:arrow, :event, :truth}}
  @connectives [:before, :after]

  @doc "Compose two event properties in supplied surface order."
  @spec compose(atom(), map(), map()) :: {:ok, map()} | :unsupported
  def compose(connective, first, second)
      when connective in @connectives and is_map(first) and is_map(second) do
    with :ok <- Event.verify(first),
         :ok <- Event.verify(second),
         true <- first.result_type == @event_property,
         true <- second.result_type == @event_property,
         %{event_existence: :not_asserted} <- first.semantics,
         %{event_existence: :not_asserted} <- second.semantics,
         initial = {:apply, {:apply, operator(connective), first.result}, second.result},
         {:ok, @relation_property} <- Expression.type_of(initial),
         {:ok, result, steps} <- Reducer.reduce(initial),
         true <- canonical_form?(result, connective),
         derivation = %{
           kind: :temporal_event_relation_property,
           status: :derived,
           connective: connective,
           initial: initial,
           result: result,
           steps: steps,
           result_type: @relation_property,
           source_event_properties_verified?: true,
           connective_witness_verified?: false,
           referent_identity: :not_established,
           event_occurrence: :not_established,
           actual_event_chronology: :not_established,
           quantifier_applied?: false,
           factual_authority?: false
         },
         :ok <- Proof.verify(derivation) do
      {:ok, derivation}
    else
      _ -> :unsupported
    end
  end

  def compose(_, _, _), do: :unsupported

  @doc "Rebuild a complete derivation; mere type correctness is not enough."
  @spec verify(map(), atom(), map(), map()) :: :ok | {:error, atom()}
  def verify(%{} = saved, connective, first, second) do
    with :ok <- Proof.verify(saved),
         {:ok, ^saved} <- compose(connective, first, second) do
      :ok
    else
      _ -> {:error, :invalid_temporal_relation}
    end
  end

  def verify(_, _, _, _), do: {:error, :invalid_temporal_relation}

  defp operator(connective) do
    # The unbound expressions P and Q are event predicates, not event facts.
    # Actual connection to "before"/"after" in language is outside this
    # operator and must be supplied by independently checked syntax.
    ordered =
      case connective do
        :before -> {:event_precedes, {:var, "e1"}, {:var, "e2"}}
        :after -> {:event_precedes, {:var, "e2"}, {:var, "e1"}}
      end

    {:lambda, "P", @event_property,
     {:lambda, "Q", @event_property,
      {:lambda, "e1", :event,
       {:lambda, "e2", :event,
        {:and,
         {:and, {:apply, {:var, "P"}, {:var, "e1"}},
          {:apply, {:var, "Q"}, {:var, "e2"}}},
         ordered}}}}}
  end

  # Event predicates compose into two nested conjunctions, with their
  # separately replayed canonical normal forms under the binders.
  defp canonical_form?(
         {:lambda, first, :event,
          {:lambda, second, :event,
           {:and,
            {:and, _first_event_property, _second_event_property},
            {:event_precedes, {:var, earlier}, {:var, later}}}}}, connective)
       when is_binary(first) and is_binary(second) and first != second do
    case connective do
      :before -> earlier == first and later == second
      :after -> earlier == second and later == first
      _ -> false
    end
  end

  defp canonical_form?(_, _), do: false
end
