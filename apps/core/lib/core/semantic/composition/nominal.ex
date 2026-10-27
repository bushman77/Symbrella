defmodule Core.Semantic.Composition.Nominal do
  @moduledoc """
  Incremental nominal-property composition using closed, typed lexical terms.

  Modifiers are applied right-to-left to a noun property, preserving surface
  scope: "small black bird" -> small(black(bird)). This computes an entity
  property, not a resolved entity, existential assertion, or world fact.

  This pure module does not admit dictionary senses. The caller must supply
  the independently verified, same-turn lexical and structural witnesses.
  """

  alias Core.Semantic.Composition.{Expression, Lexical, Proof, Reducer}

  @property_type {:arrow, :entity, :truth}
  @max_modifiers 2

  @spec compose(String.t(), [String.t()]) :: {:ok, map()} | :unsupported
  def compose(head, modifiers)
      when is_binary(head) and is_list(modifiers) do
    with true <- length(modifiers) <= @max_modifiers,
         true <- Enum.all?(modifiers, &is_binary/1),
         {:ok, noun} <- Lexical.nominal_property(head),
         {:ok, initial} <- apply_modifiers(modifiers, noun.term),
         {:ok, @property_type} <- Expression.type_of(initial),
         {:ok, result, steps} <- Reducer.reduce(initial),
         derivation = %{
           initial: initial,
           result: result,
           steps: steps,
           result_type: @property_type
         },
         :ok <- Proof.verify(derivation) do
      {:ok, derivation}
    else
      _ -> :unsupported
    end
  end

  def compose(_, _), do: :unsupported

  defp apply_modifiers(modifiers, noun_term) do
    modifiers
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, noun_term}, fn modifier, {:ok, acc} ->
      case Lexical.nominal_modifier(modifier) do
        {:ok, %{term: modifier_term}} ->
          {:cont, {:ok, {:apply, modifier_term, acc}}}

        :unsupported ->
          {:halt, :unsupported}
      end
    end)
  end
end
