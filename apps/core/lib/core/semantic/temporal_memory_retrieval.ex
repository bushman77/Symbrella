defmodule Core.Semantic.TemporalMemoryRetrieval do
  @moduledoc """
  Read-only recall of one previously resolved temporal-order disagreement.

  The chosen order is the speaker's *stated intention*, not an assertion that
  either event occurred or that two descriptions refer to the same events.
  No LLM, transcript, latest-turn guessing, or memory mutation is used.

  Retrieval inherits TemporalClaimReconciliation's entire retained-session
  bound and unique-pair requirement. Missing, trimmed, or ambiguous evidence
  fails closed.
  """

  alias Core.Discourse.Ledger
  alias Core.Semantic.TemporalClaimReconciliation

  @question ~r/\A\s*(?:which order did i finally mean|which temporal order did i choose)\?\s*\z/iu
  @actors [:bird, :cat, :dog]
  @actions [:sit, :sleep]
  @relations [:on, :in, :under, :beside, :above, :below]
  @landmarks [:fence, :tree, :table]

  @doc "Revalidate a selected speaker report from the current same-session ledger."
  @spec recall(term()) :: {:ok, map()} | :not_found
  def recall(session) when is_binary(session) and byte_size(session) in 1..128 do
    with {:resolved, %{
           kind: :speaker_preferred_temporal_report,
           status: :speaker_preference_recorded,
           older_report_seq: older_seq,
           newer_report_seq: newer_seq,
           chosen_report_seq: chosen_seq,
           resolution_seq: resolution_seq,
           event_identity: :unresolved,
           event_occurrence: :not_established,
           actual_event_chronology: :not_established,
           contradiction_verified?: false,
           factual_authority?: false
         }} <- TemporalClaimReconciliation.detect(session),
         true <- is_integer(chosen_seq) and
                   chosen_seq in [older_seq, newer_seq],
         true <- is_integer(resolution_seq) and resolution_seq > newer_seq,
         records <- Ledger.entries(session),
         [chosen] <- Enum.filter(records, &(&1.seq == chosen_seq)),
         [resolution] <- Enum.filter(records, &(&1.seq == resolution_seq)),
         true <- qualified_chosen?(chosen),
         true <- qualified_resolution?(resolution, chosen, older_seq, newer_seq),
         {:ok, earlier} <- descriptor(chosen.earlier),
         {:ok, later} <- descriptor(chosen.later) do
      {:ok, %{
        kind: :recalled_temporal_speaker_preference,
        status: :revalidated_from_ledger,
        chosen_report_seq: chosen_seq,
        resolution_seq: resolution_seq,
        pending_request_seq: resolution.pending_request_seq,
        older_report_seq: older_seq,
        newer_report_seq: newer_seq,
        connective: chosen.connective,
        earlier: earlier,
        later: later,
        provenance: :revalidated_temporal_preference_ledger,
        speaker_attributed?: true,
        event_identity_verified?: false,
        event_occurrence_verified?: false,
        actual_event_chronology_verified?: false,
        contradiction_verified?: false,
        factual_authority?: false
      }}
    else
      _ -> :not_found
    end
  end

  def recall(_), do: :not_found

  @doc "Recognize a bounded explicit question, not a general memory search."
  @spec question(map()) :: map() | nil
  def question(%{} = si) do
    session = Map.get(si, :session_id)
    sentence = Map.get(si, :sentence)

    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- is_binary(session) and byte_size(session) in 1..128,
         true <- is_binary(sentence) and byte_size(sentence) <= 120,
         true <- Regex.match?(@question, sentence),
         {:ok, recalled} <- recall(session) do
      %{
        version: 1,
        kind: :temporal_preference_recall_question,
        status: :provisional,
        sentence: sentence,
        definition: nil,
        speech_act: :request_information,
        source: :revalidated_temporal_preference_ledger,
        operation: :recall_intended_temporal_order,
        temporal_evidence: recalled,
        unresolved: [:event_identity, :event_occurrence,
          :actual_event_chronology, :exact_dictionary_senses],
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end

  def question(_), do: nil

  @doc "Re-query the live ledger instead of trusting stale saved meaning."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :sentence_meaning)

    if is_map(saved) and
       Map.get(saved, :kind) == :temporal_preference_recall_question and
       Map.get(saved, :factual_authority?) == false and
       saved == question(si) do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  defp qualified_chosen?(%{
         kind: :reported_temporal_relation,
         status: :provisional,
         relation: :before,
         connective: connective,
         proof_sha256: proof,
         source: :revalidated_temporal_clause_composition,
         provenance: :revalidated_temporal_clause_composition,
         verification: :conversation_context_only,
         speaker_attributed?: true,
         event_occurrence_verified?: false,
         actual_event_chronology_verified?: false,
         referent_identity_verified?: false,
         factual_authority?: false
       }) when connective in [:before, :after] and is_binary(proof) do
    Regex.match?(~r/\A[0-9a-f]{64}\z/, proof)
  end

  defp qualified_chosen?(_), do: false

  defp qualified_resolution?(%{
         kind: :temporal_clarification_resolution,
         status: :provisional,
         pending_request_seq: request_seq,
         conflict_pair: %{
           older_report_seq: older_seq,
           newer_report_seq: newer_seq,
           older_proof_sha256: older_proof,
           newer_proof_sha256: newer_proof
         },
         chosen_report_seq: chosen_seq,
         chosen_claim: claim,
         provenance: :revalidated_temporal_clarification_answer,
         verification: :conversation_context_only,
         speaker_attributed?: true,
         contradiction_verified?: false,
         event_occurrence_verified?: false,
         actual_event_chronology_verified?: false,
         referent_identity_verified?: false,
         factual_authority?: false
       }, chosen, older_seq, newer_seq) do
    is_integer(request_seq) and request_seq > newer_seq and
      chosen_seq == chosen.seq and
      is_binary(older_proof) and is_binary(newer_proof) and
      chosen.proof_sha256 ==
        if(chosen_seq == older_seq, do: older_proof, else: newer_proof) and
      claim == Map.take(chosen, [:earlier, :later, :connective])
  end

  defp qualified_resolution?(_, _, _, _), do: false

  defp descriptor(%{
         action: action, actor_kind: actor,
         relation: relation, landmark: landmark
       } = event)
       when action in @actions and actor in @actors and
            relation in @relations and landmark in @landmarks do
    if map_size(event) == 4,
      do: {:ok, %{action: action, actor_kind: actor,
        relation: relation, landmark: landmark}},
      else: :unsupported
  end

  defp descriptor(_), do: :unsupported
end
