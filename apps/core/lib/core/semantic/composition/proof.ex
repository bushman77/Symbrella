defmodule Core.Semantic.Composition.Proof do
  @moduledoc """
  Replays the complete beta-reduction trace and checks preservation of type.

  Verification establishes only formal derivability from the supplied typed
  expression. It does NOT verify lexical sense, speaker intent or world truth.
  """
  alias Core.Semantic.Composition.{Expression, Reducer}

  @max_steps 24

  @spec verify(map()) :: :ok | {:error, atom()}
  def verify(%{initial: initial, steps: steps, result: result, result_type: type})
      when is_list(steps) and length(steps) <= @max_steps do
    with {:ok, ^type} <- Expression.type_of(initial),
         {:ok, ^type} <- Expression.type_of(result),
         {:ok, ^result} <- replay(initial, steps),
         :normal <- Reducer.step(result) do
      :ok
    else
      _ -> {:error, :invalid_derivation}
    end
  end

  def verify(_), do: {:error, :invalid_derivation}

  defp replay(term, []), do: {:ok, term}

  defp replay(term, [%{rule: :beta, before: term, after: next} | rest]) do
    case Reducer.step(term) do
      {:ok, ^next} -> replay(next, rest)
      _ -> {:error, :invalid_step}
    end
  end

  defp replay(_, _), do: {:error, :broken_proof_chain}
end
