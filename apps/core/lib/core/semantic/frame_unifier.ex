defmodule Core.Semantic.FrameUnifier do
  @moduledoc """
  Collapse independently witnessed definition-request constructions into the
  same canonical operation, while preserving syntax and word-sense uncertainty.

  Lexical selection is NEVER inferred from candidate ID ordering. Frames are
  strictly conversational requests, not retrieved facts or definitions.
  """
  alias Core.Pipeline.Trace
  alias Core.Semantic.ConstructionSchema

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    frame = resolve(si)

    si
    |> Map.put(:semantic_request_frame, frame)
    |> Trace.append(:frame_unifier,
      decision: if(is_map(frame), do: :structurally_interpreted, else: :abstained),
      reason: :revalidated_construction_unification,
      meta: if(is_map(frame), do: %{
        operation: frame.operation, construction: frame.construction,
        target: frame.target.lemma, semantic_status: frame.semantic.status
      }, else: %{}))
  end

  def attach(other), do: other

  @spec resolve(map()) :: map() | nil
  def resolve(%{} = si) do
    recomputed = ConstructionSchema.detect(si)
    attached = Map.get(si, :construction_candidates)

    # An injected or stale candidate cannot authorize a frame.
    if is_list(attached) and attached == recomputed do
      case attached do
        [%{operation: :define, target: %{lemma: term}} = candidate] ->
          witnesses = ConstructionSchema.target_witnesses(si, term)
          senses = witnesses |> Enum.map(&cell(&1, :id)) |> Enum.uniq() |> Enum.sort()
          poses = ConstructionSchema.target_pos(si, term)

          if senses != [] do
            %{
              version: 1,
              status: :structurally_interpreted,
              speech_act: :request,
              operation: :define,
              construction: candidate.schema,
              target: candidate.target,
              grammatical: %{
                status: :supported,
                role: :lexical_target,
                pos: if(candidate.schema == :definition_imperative,
                  do: "noun", else: :undetermined),
                pos_alternatives: poses,
                provenance: candidate.provenance
              },
              semantic: %{
                status: if(length(senses) == 1, do: :single_candidate_unverified,
                  else: :multiple_candidates_unresolved),
                selected_sense: nil,
                candidate_count: length(senses),
                candidate_ids: Enum.take(senses, 8)
              },
              evidence: candidate.witnesses,
              source: :attested_construction_unification,
              original_sentence: Map.get(si, :sentence)
            }
          end

        _ ->
          nil
      end
    end
  end

  def resolve(_), do: nil

  @doc "Revalidate an attached frame against the same sentence, grammar and LTM."
  @spec verified(map(), String.t()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si, sentence) when is_binary(sentence) do
    frame = Map.get(si, :semantic_request_frame)
    expected = resolve(si)

    if is_map(frame) and frame == expected and
         frame.original_sentence == sentence and
         frame.status == :structurally_interpreted and
         frame.semantic.selected_sense == nil do
      {:ok, frame}
    else
      :not_applicable
    end
  end

  def verified(_, _), do: :not_applicable

  defp cell(%{} = row, key), do: Map.get(row, key, Map.get(row, Atom.to_string(key)))
end
