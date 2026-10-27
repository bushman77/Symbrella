defmodule Core.Semantic.Composition do
  @moduledoc """
  V1: read-only, evidence-gated adjective + noun property composition.

  Runs downstream of StructuralCompetition, but never asserts a referent or
  upgrades a grammatical hypothesis to a fact. Its closed semantic vocabulary
  is deliberately smaller than the dictionary. Successful results are
  reproducible typed lambda derivations, not verified statements about reality.
  """
  alias Core.Pipeline.Trace
  alias Core.Semantic.Composition.{Expression, Proof, Reducer}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  @max_signatures 64

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    proof = derive(si)

    si
    |> Map.put(:compositional_derivation, proof)
    |> Trace.append(:semantic_composition,
      decision: if(is_map(proof), do: :derived, else: :abstained),
      reason: :closed_nominal_lambda_prototype,
      meta: if(is_map(proof), do: %{
        construction: proof.construction,
        span: proof.span,
        reduction_steps: length(proof.steps),
        formally_verified?: true,
        factual_authority?: false
      }, else: %{})
  end

  def attach(other), do: other

  @spec derive(map()) :: map() | nil
  def derive(%{} = si) do
    signatures = List.wrap(Map.get(si, :sentence_signatures))
    detected = Signatures.detect(si)

    with true <- Enum.all?(signatures, &(&1 in detected)),
         true <- structural_scope_valid?(si) do
      signatures
      |> Enum.take(@max_signatures)
      |> Enum.filter(&nominal_signature?/1)
      |> Enum.sort_by(fn sig ->
        {elem(sig.span, 1), -(elem(sig.span, 1) - elem(sig.span, 0))}
      end)
      |> Enum.find_value(&compose_signature(si, &1))
    else
      _ -> nil
    end
  end

  def derive(_), do: nil

  @doc "Accept only the current-turn, proof-checked derivation and its sources."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :compositional_derivation)

    if is_map(saved) and saved == derive(si) and Proof.verify(saved) == :ok do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp nominal_signature?(%{
         id: :noun_phrase, source: :core_syntax, span: {first, last},
         roles: %{modifier: modifier, head: head}, evidence: evidence
       })
       when is_integer(first) and is_integer(last) and is_integer(modifier) and
              is_integer(head) and is_list(evidence) do
    first >= 0 and head == last and modifier == head - 1 and
      :nominal_constituent in evidence and :pos_pattern in evidence
  end

  defp nominal_signature?(_), do: false

  defp compose_signature(si, signature) do
    %{modifier: modifier, head: head} = signature.roles

    with {:ok, adjective} <- witnessed_role(si, modifier, "adjective"),
         {:ok, noun} <- witnessed_role(si, head, "noun"),
         {:ok, modifier_term} <- Expression.adjective(adjective.word),
         {:ok, noun_term} <- Expression.noun(noun.word),
         initial = {:apply, modifier_term, noun_term},
         {:ok, {:arrow, :entity, :truth}} <- Expression.type_of(initial),
         {:ok, result, steps} <- Reducer.reduce(initial) do
      proof = %{
        version: 1,
        status: :derived,
        kind: :nominal_property,
        construction: :adjective_noun,
        span: signature.span,
        premises: %{
          signature: Map.take(signature, [:id, :span, :roles, :required_features, :evidence]),
          lexical_witnesses: [adjective, noun],
          semantic_vocabulary: :closed_nominal_v1
        },
        initial: initial,
        steps: steps,
        result: result,
        result_type: {:arrow, :entity, :truth},
        epistemic_status: :conditional_on_lexical_and_structural_premises,
        quantifier_applied?: false,
        factual_authority?: false
      }

      if Proof.verify(proof) == :ok, do: proof, else: nil
    else
      _ -> nil
    end
  end

  defp witnessed_role(si, index, pos) do
    selected =
      si
      |> Map.get(:lifg_choices, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :token_index) == index))

    token =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :index) == index and
        Map.get(&1, :n, 1) == 1))

    with [choice] <- selected,
         [surface] <- token,
         word when is_binary(word) <- Map.get(surface, :phrase),
         word <- normalize(word),
         id when is_binary(id) <- Map.get(choice, :id) || Map.get(choice, :chosen_id),
         true <- String.starts_with?(id, word <> "|" <> pos <> "|"),
         true <- Enum.any?(List.wrap(Map.get(si, :active_cells)), fn cell ->
           is_map(cell) and Map.get(cell, :id) == id and
             normalize(Map.get(cell, :norm)) == word and
             Map.get(cell, :pos) == pos
         end) do
      {:ok, %{token_index: index, word: word, pos: pos, sense_id: id}}
    else
      _ -> :not_supported
    end
  end

  defp structural_scope_valid?(si) do
    case Map.get(si, :structural_competition) do
      %{status: :unresolved} ->
        false

      %{status: :preferred} ->
        match?({:ok, _}, StructuralCompetition.verified(si))

      %{status: :no_complete_candidate} ->
        # The observed nominal phrase is still a partial interpretation, not
        # a complete sentence. No structural sentence winner is claimed.
        true

      nil ->
        false

      _ ->
        false
    end
  end

  defp normalize(value) when is_binary(value) do
    value
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp normalize(_), do: ""
end
