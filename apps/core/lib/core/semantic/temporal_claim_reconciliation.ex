defmodule Core.Semantic.TemporalClaimReconciliation do
  @moduledoc """
  Read-only, bounded comparison of speaker-attributed temporal ledger claims.

  Reverse ordering of identical closed event *descriptions* is a possible
  disagreement between reports, NOT a logical contradiction about unique
  real-world events. Two descriptions may refer to different occurrences.
  Neither a stored proof hash nor matching actor kinds establishes identity.

  Never edit, retract, merge, or promote a ledger record. A single unique
  opposing pair may authorize a narrowly scoped clarification question.
  """
  alias Core.Discourse.Ledger

  @max_records 128
  @question ~r/\A\s*(?:did i describe conflicting temporal orders|did i contradict myself about the order)\?\s*\z/iu
  @actions [:sit, :sleep]
  @actors [:bird, :cat, :dog]
  @relations [:on, :in, :under, :beside, :above, :below]
  @landmarks [:fence, :tree, :table]

  @doc """
  Compare only retained, independently witnessed reports in one session.
  Return one *candidate* conflict, or abstain if evidence is absent,
  ambiguous, or the configured scan cannot cover all retained records.
  """
  @spec detect(term()) :: {:ok, map()} | :not_found | :ambiguous | :window_incomplete
  def detect(session) when is_binary(session) and byte_size(session) in 1..128 do
    entries = Ledger.entries(session)

    if length(entries) > @max_records do
      :window_incomplete
    else
      records = Enum.filter(entries, &qualified?/1)

      pairs =
        for newer <- records, older <- records,
            older.seq < newer.seq,
            reverse_descriptions?(older, newer) do
          %{older: project(older), newer: project(newer)}
        end

      case pairs do
        [pair] ->
          {:ok, %{
            kind: :candidate_temporal_order_conflict,
            status: :provisional,
            scope: :same_session,
            relation: :opposing_speaker_claims,
            matching_basis: :equal_closed_event_descriptions,
            older_report: pair.older,
            newer_report: pair.newer,
            report_order: :verified_from_session_sequence,
            occurrence_identity: :unresolved,
            event_occurrence: :not_established,
            actual_event_chronology: :not_established,
            contradiction_verified?: false,
            speaker_claims_verified?: true,
            factual_authority?: false
          }}

        [] -> :not_found
        _ -> :ambiguous
      end
    end
  end

  def detect(_), do: :not_found

  @doc "Recognize one explicit question only when a unique pair is available."
  @spec question(map()) :: map() | nil
  def question(%{} = si) do
    session = Map.get(si, :session_id)
    sentence = Map.get(si, :sentence)

    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- is_binary(session) and byte_size(session) in 1..128,
         true <- is_binary(sentence) and byte_size(sentence) <= 120,
         true <- Regex.match?(@question, sentence),
         {:ok, conflict} <- detect(session) do
      %{
        version: 1,
        kind: :temporal_claim_conflict_question,
        status: :provisional,
        sentence: sentence,
        definition: nil,
        speech_act: :request_information,
        source: :revalidated_temporal_claim_reconciliation,
        operation: :clarify_opposing_temporal_reports,
        conflict_evidence: conflict,
        unresolved: [:event_occurrence, :actual_event_chronology,
          :event_identity, :exact_dictionary_senses],
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end

  def question(_), do: nil

  @doc "Replay the same-session ledger query instead of trusting saved SI."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :sentence_meaning)

    if is_map(saved) and
       Map.get(saved, :kind) == :temporal_claim_conflict_question and
       Map.get(saved, :factual_authority?) == false and
       saved == question(si) do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp reverse_descriptions?(older, newer) do
    older.earlier != older.later and
      older.earlier == newer.later and
      older.later == newer.earlier
  end

  defp project(record) do
    Map.take(record, [:seq, :connective, :earlier, :later, :proof_sha256])
  end

  defp qualified?(%{
         kind: :reported_temporal_relation,
         status: :provisional,
         relation: :before,
         seq: seq,
         connective: connective,
         earlier: earlier,
         later: later,
         proof_sha256: proof,
         source: :revalidated_temporal_clause_composition,
         provenance: :revalidated_temporal_clause_composition,
         verification: :conversation_context_only,
         speaker_attributed?: true,
         event_occurrence_verified?: false,
         actual_event_chronology_verified?: false,
         referent_identity_verified?: false,
         factual_authority?: false
       })
       when is_integer(seq) and seq > 0 and
            connective in [:before, :after] and is_binary(proof) do
    byte_size(proof) == 64 and
      Regex.match?(~r/\A[0-9a-f]{64}\z/, proof) and
      event_descriptor?(earlier) and event_descriptor?(later)
  end

  defp qualified?(_), do: false

  defp event_descriptor?(%{
         action: action, actor_kind: actor,
         relation: relation, landmark: landmark
       } = descriptor) do
    map_size(descriptor) == 4 and action in @actions and actor in @actors and
      relation in @relations and landmark in @landmarks
  end

  defp event_descriptor?(_), do: false
end
