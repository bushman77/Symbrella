defmodule Core.Semantic.EpisodicProjection do
  @moduledoc """
  Read-only bridge from already interpreted Core semantic frames to the
  generalized Hippocampal event contract.

  This does not invent roles, choose dictionary senses or write to storage.
  At present, only fully resolved, first-person obligation events have an
  eligible projection. Health frames and unsupported/question interpretations
  are never admitted here.
  """

  alias Core.Semantic.EpisodicEvents
  alias Core.Semantic.SVOEvent
  alias Core.Semantic.ParticipantSpatial
  alias Core.Pipeline.Trace

  @confidence_floor 0.8

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    candidates = project(si)

    updated = Map.put(si, :episodic_candidates, candidates)

    if candidates == [] do
      updated
    else
      Trace.append(updated, :episodic_projection,
        decision: :candidate,
        reason: :structured_user_reported_event,
        scores: %{confidence: hd(candidates).frame_confidence},
        meta: %{count: length(candidates), source_frames: Enum.map(candidates, & &1.source_frame)}
      )
    end
  end

  def attach(other), do: other

  @doc "Produce only explicit, fully resolved interpretations with inspectable lineage."
  @spec project(map()) :: [map()]
  def project(%{} = si) do
    frame = get(si, :symbolic_frame)
    sentence = get(si, :sentence)
    source = get(si, :source)
    intent = get(si, :intent)

    cond do
      source not in [nil, :user, :prod, :test] -> []
      not is_binary(sentence) or String.contains?(sentence, "?") -> []
      true ->
        obligation =
          if intent in [:ask, :question, :command] or not is_map(frame),
            do: [],
            else: project_frame(frame, sentence)

        obligation ++ SVOEvent.project(si, sentence) ++ ParticipantSpatial.project(si, sentence)
    end
  end

  def project(_), do: []

  @doc "Reject injected or stale candidates by deriving the expected frame again."
  @spec validate_candidate(map(), term()) :: {:ok, map()} | :error
  def validate_candidate(%{} = si, %{} = candidate) do
    if Enum.member?(project(si), candidate) do
      case EpisodicEvents.normalize(candidate) do
        {:ok, event} -> {:ok, Map.merge(event, %{
          source_frame: candidate.source_frame,
          frame_confidence: candidate.frame_confidence,
          participants: Map.get(candidate, :participants)
        })}
        :error -> :error
      end
    else
      :error
    end
  end

  def validate_candidate(_, _), do: :error

  @doc """
  Revalidate one unambiguous participant event against this exact SemanticInput.

  This read-only boundary is shared by SentenceMeaning and WM. Do not trust
  the mere presence of a candidate: an injection, assistant-origin turn, or
  competing complete interpretation cannot become structural understanding.
  """
  @spec validated_participant(map()) :: map() | nil
  def validated_participant(%{} = si) do
    candidates =
      if get(si, :source) in [:user, :prod, :test],
        do: List.wrap(get(si, :episodic_candidates)),
        else: []

    case candidates do
      [candidate] when is_map(candidate) ->
        case validate_candidate(si, candidate) do
          {:ok, %{source_frame: frame, participants: %{"actor" => actor}} = event}
              when frame in [:finite_present_svo, :progressive_spatial] and
                   is_binary(actor) and byte_size(actor) > 0 ->
            event

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  def validated_participant(_), do: nil

  defp project_frame(%{type: :obligation_event, subject: :speaker,
      modality: :obligation, action: action, uncertain_roles: [],
      confidence: confidence}, sentence)
      when is_binary(action) and is_number(confidence) and
           confidence >= @confidence_floor do
    # A matching surface cue and sentence form are independent checks
    # against over-promoting a frame assembled from an unrelated context.
    action = String.trim(action)

    if action != "" and
         Regex.match?(~r/^i(?:'ve| have) got to /iu, String.trim(sentence)) and
         Regex.match?(Regex.compile!("\\b" <> Regex.escape(action) <> "\\b", "iu"), sentence) do
      case EpisodicEvents.normalize(%{
        entity: "me", relation: "has_obligation", value: action, status: :asserted
      }) do
        {:ok, event} ->
          [Map.merge(event, %{source_frame: :obligation_event,
            frame_confidence: confidence})]
        :error -> []
      end
    else
      []
    end
  end

  defp project_frame(_, _), do: []

  defp get(map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
end
