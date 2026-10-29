defmodule Core.Semantic.TemporalClarificationResolution do
  @moduledoc """
  Interpret one explicit, immediately following reply to a pending temporal
  reconciliation request. An answer selects the speaker's *intended report*,
  not the verified order of real-world events.

  The request and both original claims must still be present and agree with
  the same-session, independently qualified reconciliation candidate.
  """
  alias Core.Discourse.Ledger
  alias Core.Semantic.TemporalClaimReconciliation

  @answer ~r/\A\s*i meant report ([1-9][0-9]{0,5})\.\s*\z/iu

  @spec build(map()) :: map() | nil
  def build(%{} = si) do
    session = Map.get(si, :session_id)
    sentence = Map.get(si, :sentence)

    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- is_binary(session) and byte_size(session) in 1..128,
         true <- is_binary(sentence) and byte_size(sentence) <= 80,
         [_, report_number] <- Regex.run(@answer, sentence),
         {chosen_seq, ""} <- Integer.parse(report_number),
         [pending | _] <- Ledger.entries(session),
         {:ok, conflict} <- TemporalClaimReconciliation.detect(session),
         %{older_report: older, newer_report: newer} <- conflict,
         true <- valid_pending?(pending, older, newer),
         chosen when is_map(chosen) <- choose(chosen_seq, older, newer) do
      %{
        version: 1,
        kind: :temporal_clarification_resolution,
        status: :provisional,
        sentence: sentence,
        definition: nil,
        speech_act: :clarification,
        source: :revalidated_temporal_clarification_answer,
        operation: :select_intended_report_order,
        pending_request_seq: pending.seq,
        conflict_pair: %{
          older_report_seq: older.seq,
          newer_report_seq: newer.seq,
          older_proof_sha256: older.proof_sha256,
          newer_proof_sha256: newer.proof_sha256
        },
        chosen_report_seq: chosen.seq,
        chosen_claim: %{
          earlier: chosen.earlier,
          later: chosen.later,
          connective: chosen.connective
        },
        speaker_attributed?: true,
        contradiction_verified?: false,
        event_occurrence_verified?: false,
        actual_event_chronology_verified?: false,
        referent_identity_verified?: false,
        unresolved: [:event_identity, :event_occurrence,
          :actual_event_chronology, :exact_dictionary_senses],
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end

  def build(_), do: nil

  @doc "Rebuild against the pending request and unchanged current ledger."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :sentence_meaning)

    if is_map(saved) and
       Map.get(saved, :kind) == :temporal_clarification_resolution and
       Map.get(saved, :factual_authority?) == false and
       saved == build(si) do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp choose(seq, %{seq: candidate} = older, _newer)
       when seq == candidate, do: older
  defp choose(seq, _older, %{seq: candidate} = newer)
       when seq == candidate, do: newer
  defp choose(_, _, _), do: nil

  defp valid_pending?(%{
         kind: :temporal_clarification_request,
         status: :provisional,
         seq: pending_seq,
         older_report_seq: older_seq,
         newer_report_seq: newer_seq,
         older_proof_sha256: older_sha,
         newer_proof_sha256: newer_sha,
         provenance: :revalidated_temporal_claim_reconciliation,
         verification: :conversation_context_only,
         contradiction_verified?: false,
         factual_authority?: false
       }, older, newer) do
    is_integer(pending_seq) and pending_seq > newer.seq and
      older_seq == older.seq and newer_seq == newer.seq and
      older_sha == older.proof_sha256 and newer_sha == newer.proof_sha256
  end

  defp valid_pending?(_, _, _), do: false
end
