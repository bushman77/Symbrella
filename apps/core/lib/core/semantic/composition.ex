defmodule Core.Semantic.Composition do
  @moduledoc """
  V1 nominal properties and V2 progressive event properties. Read-only.

  Runs downstream of StructuralCompetition, but never asserts a referent or
  upgrades a grammatical hypothesis to a fact. Its closed semantic vocabulary
  is deliberately smaller than the dictionary. Successful results are
  reproducible typed lambda derivations, not verified statements about reality.
  """
  alias Core.Pipeline.Trace
  alias Core.Semantic.Composition.{Diagnosis, Event, Expression, Proof, Reducer}
  alias Core.Syntax.{Signatures, StructuralCompetition}

  @max_signatures 64
  @telemetry_event [:core, :semantic, :composition]

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    proof = derive(si)
    blockers = if(is_map(proof), do: [], else: Diagnosis.blocked_roles(si))
    abstention_reason =
      if(blockers == [], do: :no_admissible_composition, else: :lexical_role_blocked)

    updated =
      si
      |> Map.put(:compositional_derivation, proof)
      |> Trace.append(:semantic_composition,
        decision: if(is_map(proof), do: :derived, else: :abstained),
        reason: if(is_map(proof), do: :closed_typed_lambda_composition, else: abstention_reason),
        meta: if(is_map(proof), do: %{
          construction: proof.construction,
          span: proof.span,
          reduction_steps: length(proof.steps),
          formally_verified?: true,
          factual_authority?: false
        }, else: %{
          blockers: blockers,
          blocked_role_count: length(blockers),
          factual_authority?: false
        }))

    Core.Telemetry.emit(@telemetry_event,
      %{derivations: if(is_map(proof), do: 1, else: 0)},
      %{
        status: if(is_map(proof), do: :derived, else: :abstained),
        construction: if(is_map(proof), do: proof.construction, else: nil),
        reduction_steps: if(is_map(proof), do: length(proof.steps), else: 0),
        formally_verified?: is_map(proof),
        abstention_reason: if(is_map(proof), do: nil, else: abstention_reason),
        blockers: blockers,
        factual_authority?: false
      })

    updated
  end

  def attach(other), do: other

  @spec derive(map()) :: map() | nil
  def derive(%{} = si) do
    signatures = List.wrap(Map.get(si, :sentence_signatures))

    # Most turns have no supported nominal construction. Avoid another
    # dictionary/signature pass in those cases and bound the prototype.
    with true <- length(signatures) <= @max_signatures,
         true <- Enum.any?(signatures, &(nominal_signature?(&1) or event_signature?(&1))),
         true <- structural_scope_valid?(si),
         detected <- Signatures.detect(si),
         true <- Enum.all?(signatures, &(&1 in detected)) do
      event_derivation(si, signatures) ||
        (signatures
         |> Enum.take(@max_signatures)
         |> Enum.filter(&nominal_signature?/1)
         |> Enum.sort_by(fn sig ->
           {elem(sig.span, 1), -(elem(sig.span, 1) - elem(sig.span, 0))}
         end)
         |> Enum.find_value(&compose_signature(si, &1)))
    else
      _ -> nil
    end
  end

  def derive(_), do: nil

  @doc "Accept only the current-turn, proof-checked derivation and its sources."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :compositional_derivation)

    checked? =
      case saved do
        %{kind: :progressive_event_property} -> Event.verify(saved) == :ok
        %{kind: :nominal_property} -> Proof.verify(saved) == :ok
        _ -> false
      end

    if checked? and saved == derive(si) do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp event_signature?(%{
         id: id, source: :core_syntax, span: {0, 6},
         roles: %{subject_determiner: 0, subject_head: 1,
                  auxiliary: 2, predicate: 3, preposition: 4,
                  location_determiner: 5, location_head: 6},
         evidence: evidence
       }) when id in [:progressive_locative_candidate, :progressive_directional_candidate]
                 and is_list(evidence) do
    Enum.all?(
      [:contiguous_surface, :attested_nominal_subject, :attested_present_participle,
       :attested_preposition, :attested_nominal_location],
      &(&1 in evidence)
    ) and
      (id != :progressive_directional_candidate or :directional_not_arrival in evidence)
  end

  defp event_signature?(_), do: false

  defp event_derivation(si, signatures) do
    case StructuralCompetition.verified(si) do
      {:ok, %{preferred: preferred}} ->
        Enum.find_value(signatures, fn sig ->
          if event_signature?(sig) and preferred.construction == sig.id and
               preferred.span == sig.span and preferred.roles == sig.roles do
            compose_event(si, sig)
          end
        end)

      :not_applicable ->
        nil
    end
  end

  defp compose_event(si, signature) do
    r = signature.roles

    with {:ok, _subject_determiner} <- witnessed_role(si, r.subject_determiner, "determiner"),
         {:ok, actor} <- witnessed_role(si, r.subject_head, "noun"),
         {:ok, auxiliary} <- witnessed_auxiliary(si, r.auxiliary),
         true <- auxiliary.word == "is",
         {:ok, predicate} <- witnessed_role(si, r.predicate, "verb"),
         {:ok, preposition} <- witnessed_role(si, r.preposition, "preposition"),
         {:ok, _location_determiner} <- witnessed_role(si, r.location_determiner, "determiner"),
         {:ok, landmark} <- witnessed_role(si, r.location_head, "noun"),
         {:ok, event} <- Event.compose(%{
           predicate: predicate.word,
           actor: actor.word,
           preposition: preposition.word,
           landmark: landmark.word
         }),
         true <- (signature.id == :progressive_directional_candidate) ==
                   (event.semantics.spatial.reading == :directional) do
      proof = %{
        version: 2,
        status: :derived,
        kind: :progressive_event_property,
        construction: signature.id,
        span: signature.span,
        premises: %{
          signature: Map.take(signature, [:id, :span, :roles, :required_features, :evidence]),
          lexical_witnesses: [actor, auxiliary, predicate, preposition, landmark],
          semantic_vocabulary: :closed_event_v2
        },
        initial: event.initial,
        steps: event.steps,
        result: event.result,
        result_type: event.result_type,
        event_semantics: event.semantics,
        epistemic_status: :conditional_on_lexical_and_structural_premises,
        quantifier_applied?: false,
        factual_authority?: false
      }

      if Event.verify(proof) == :ok, do: proof
    else
      _ -> nil
    end
  end

  defp witnessed_auxiliary(si, index) do
    case witnessed_role(si, index, "verb") do
      {:ok, _} = found -> found
      _ -> witnessed_role(si, index, "auxiliary")
    end
  end

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

      %{status: :no_complete_candidate} = recorded ->
        # A nominal property is a partial reading; confirm that even the
        # no-clause outcome came from this exact current-turn competition.
        recorded == Brain.LIFG.StructuralCompetition.resolve(si)

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
