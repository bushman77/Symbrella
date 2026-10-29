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
    with {:forward, requested_actor, :predicate, constraints} <-
           ParticipantQuestions.parse(text),
         true <- constraints == %{},
         {:ok, %{
           actor: actor, original: original, proposed: proposed,
           aspect: :progressive, position: position, location: location,
           source_turn_sha256: source_digest,
           source_request_seq: request_seq,
           status: :speaker_confirmed_spelling,
           source: :same_session_clarification,
           factual_authority?: false
         }} <- Ledger.confirmed_fuzzy_activity(session, text),
         true <- requested_actor == actor,
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
        source_request_seq: request_seq,
        source_turn_sha256: source_digest,
        evidence: %{
          source_clause_replayed?: true,
          speaker_clarification_verified?: true,
          question_role: :predicate,
          role_binding: :surface_progressive_clause,
          event_occurrence_verified?: false
        },
        factual_authority?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  def build(_, _), do: :not_applicable

  @doc """
  Answer only from the recomposed activity slot. The natural response is
  explicitly attributed to the user's description, not to observed reality.
  """
  @spec answer(term(), String.t()) :: {:ok, String.t(), map()} | :not_applicable
  def answer(session, question) when is_binary(question) do
    case build(session, question) do
      {:ok, %{
        kind: :speaker_clarified_progressive_event,
        actor: %{surface: actor, referent: :unresolved},
        predicate: %{surface: predicate, exact_dictionary_sense?: false},
        factual_authority?: false
      } = frame} ->
        {:ok, "In your description, the #{actor} is #{predicate}.", %{
          action: :conversation_recall,
          memory_source: :same_session_clarified_event,
          response_source: :symbolic_conversation,
          provenance: :speaker_clarified_progressive_event,
          semantic_frame: frame,
          requested_role: :predicate,
          retrieval_found?: true,
          semantic_verified?: false,
          factual_authority?: false,
          llm_called?: false
        }}

      _ -> :not_applicable
    end
  end

  def answer(_, _), do: :not_applicable
end
