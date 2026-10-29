defmodule Core.Semantic.ClarifiedEvent do
  @moduledoc """
  Recompose a bounded progressive event from a speaker-confirmed lexical
  substitution, without rewriting the original turn or asserting that the
  event occurred.

  The discourse ledger replays the fingerprint of the original clause and
  checks both immediately adjacent request and resolution records. This
  module independently binds a current question to its requested participant
  role, then projects the confirmed lexical choice into the *predicate slot*
  of a provisional event frame.

  A speaker-confirmed word does not establish an exact dictionary sense.
  This is a speaker-attributed interpretation, not a verified real-world
  event, a durable lexical definition, or a replacement Hippocampal memory.
  """

  alias Core.Discourse.Ledger
  alias Core.Semantic.ParticipantQuestions

  @spec build(term(), String.t()) :: {:ok, map()} | :not_applicable
  def build(session, text) when is_binary(text) do
    with {:forward, requested_actor, requested_role, constraints} <-
           ParticipantQuestions.parse(text),
         true <- requested_role in [:predicate, :location] and is_map(constraints),
         {:ok, %{
           actor: actor, original: original, proposed: proposed,
           aspect: :progressive, position: position, location: location,
           source_turn_sha256: source_digest,
           source_request_seq: request_seq,
           role_evidence: role_evidence,
           requested_role: ^requested_role,
           question_constraints: ^constraints,
           source_provisional: original_provisional,
           status: :speaker_confirmed_spelling,
           source: :same_session_clarification,
           factual_authority?: false
         }} <- Ledger.confirmed_fuzzy_event(session, text),
         true <- requested_actor == actor,
         true <- role_evidence in [
           :validated_original_provisional_frame,
           :attested_core_syntax_gap, :surface_clause_only
         ],
         true <- valid_original_role_binding?(role_evidence, original_provisional,
           actor, original, position, location),
         true <- is_binary(original) and is_binary(proposed) and
                   original != proposed,
         true <- is_binary(position) and is_binary(location) and
                   is_binary(source_digest) and is_integer(request_seq) do
      {:ok, %{
        version: 1,
        kind: :speaker_clarified_progressive_event,
        status: :provisional,
        speaker: :user,
        actor: %{surface: actor, referent: :unresolved},
        predicate: %{surface: proposed, source: :speaker_confirmed_substitution,
                     original_surface: original, exact_dictionary_sense?: false},
        aspect: :progressive,
        spatial: %{relation: position, location: location,
                   event_location_verified?: false},
        original_interpretation: original_interpretation(role_evidence,
          original_provisional),
        source_request_seq: request_seq,
        source_turn_sha256: source_digest,
        evidence: %{
          source_clause_replayed?: true,
          speaker_clarification_verified?: true,
          question_role: requested_role,
          role_binding: role_evidence,
          pre_lifg_gap_observed?: role_evidence in [
            :attested_core_syntax_gap, :validated_original_provisional_frame
          ],
          original_semantic_frame_replayed?:
            role_evidence == :validated_original_provisional_frame,
          event_occurrence_verified?: false
        },
        factual_authority?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  defp valid_original_role_binding?(
         :validated_original_provisional_frame,
         %{
           kind: :unresolved_progressive_predicate,
           actor: original_actor, predicate: original_predicate,
           position: original_position, location: original_location,
           factual_authority?: false,
           event_occurrence_verified?: false
         },
         actor, predicate, position, location)
       when original_actor == actor and original_predicate == predicate and
              original_position == position and original_location == location,
       do: true

  defp valid_original_role_binding?(role_evidence, nil, _actor, _predicate,
                                    _position, _location)
       when role_evidence in [:attested_core_syntax_gap, :surface_clause_only],
       do: true

  defp valid_original_role_binding?(_, _, _, _, _, _), do: false

  defp original_interpretation(:validated_original_provisional_frame,
         %{source_span_sha256: digest,
           kind: :unresolved_progressive_predicate,
           event_occurrence_verified?: false,
           factual_authority?: false}) do
    %{
      kind: :unresolved_progressive_predicate,
      status: :provisional,
      source_span_sha256: digest,
      speaker_clarification_confirmed_at_source?: false,
      event_occurrence_verified?: false,
      factual_authority?: false
    }
  end

  defp original_interpretation(_, _), do: nil

  def build(_, _), do: :not_applicable

  @doc """
  Select the requested role from the recomposed, speaker-attributed frame.
  Source attribution is kept in the wording; no location or activity is
  asserted as an independently verified real-world fact.
  """
  @spec answer(term(), String.t()) :: {:ok, String.t(), map()} | :not_applicable
  def answer(session, question) when is_binary(question) do
    case build(session, question) do
      {:ok, %{kind: :speaker_clarified_progressive_event,
              actor: %{surface: actor, referent: :unresolved},
              predicate: %{surface: predicate, exact_dictionary_sense?: false},
              spatial: %{relation: position, location: location,
                         event_location_verified?: false},
              evidence: %{question_role: role},
              factual_authority?: false} = frame} ->
        response =
          case role do
            :predicate -> "In your description, the #{actor} is #{predicate}."
            :location -> "In your description, the #{actor} is #{position} #{location}."
            _ -> nil
          end

        if is_binary(response) do
          {:ok, response, %{
            action: :conversation_recall,
            memory_source: :same_session_clarified_event,
            response_source: :symbolic_conversation,
            provenance: :speaker_clarified_progressive_event,
            semantic_frame: frame,
            requested_role: role,
            retrieval_found?: true,
            semantic_verified?: false,
            factual_authority?: false,
            llm_called?: false
          }}
        else
          :not_applicable
        end

      _ -> :not_applicable
    end
  end

  def answer(_, _), do: :not_applicable
end
