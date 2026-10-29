defmodule Core.Semantic.ReportVerification do
  @moduledoc """
  Answer whether the speaker previously made a specified, evidence-qualified
  progressive event description in this session.

  A positive result confirms a *recorded description*, never event occurrence.
  A nonmatch means only that no matching interpreted report is available in
  the complete retained ledger. It does not prove the speaker never said it.

  Questions use a bounded grammatical construction, while role semantics are
  checked using the existing typed compositional event compiler. The proof
  fingerprint must match a protected ledger record. No LLM or transcript read.
  """

  alias Core.Discourse.Ledger
  alias Core.Discourse.Ledger.Owner
  alias Core.Semantic.Composition.Event

  @max_records 128

  @question ~r/\A\s*(?:did\s+i\s+(?:say|describe|mention)|have\s+i\s+(?:said|described|mentioned))\s+(?:that\s+)?(?:the|a|an)\s+(?<actor>\p{L}{1,40})\s+(?:(?:is|was)\s+)?(?<predicate>\p{L}{1,40})\s+(?<relation>\p{L}{1,40})\s+(?:the|a|an)\s+(?<landmark>\p{L}{1,40})\s*[?.]?\s*\z/iu

  @spec reply(term(), String.t()) :: {:ok, String.t(), map()} | :not_applicable
  def reply(session, text)
      when is_binary(session) and byte_size(session) in 1..128 and
           is_binary(text) and byte_size(text) <= 180 do
    with true <- is_pid(Process.whereis(Owner)),
         %{} = match <- Regex.named_captures(@question, text),
         {:ok, proof} <- compose_query(match),
         :ok <- Event.verify(proof),
         entries <- Ledger.entries(session),
         true <- complete_history?(entries),
         digest <- proof_digest(proof),
         matches <- Enum.filter(entries, &matching_report?(&1, proof.semantics, digest)) do
      description = describe(match)

      if matches == [] do
        {:ok,
         "I can't find a matching interpreted report of #{description} in this retained session. That does not establish you never said it.",
         metadata(nil, 0)}
      else
        newest = hd(matches)
        {:ok,
         "Yes. You described #{description} in this session. That confirms the reported description, not that the event occurred.",
         metadata(newest.seq, length(matches))}
      end
    else
      _ -> :not_applicable
    end
  end

  def reply(_, _), do: :not_applicable

  defp compose_query(match) do
    Event.compose(%{
      actor: norm(match["actor"]),
      predicate: norm(match["predicate"]),
      preposition: norm(match["relation"]),
      landmark: norm(match["landmark"])
    })
  end

  defp describe(match) do
    "a #{norm(match["actor"])} #{norm(match["predicate"])} " <>
      "#{norm(match["relation"])} a #{norm(match["landmark"])}"
  end

  defp norm(value), do: value |> String.downcase() |> String.trim()

  defp complete_history?([]), do: true
  defp complete_history?(records) when is_list(records) do
    length(records) <= @max_records and match?(%{seq: 1}, List.last(records))
  end

  defp proof_digest(proof) do
    :crypto.hash(:sha256, :erlang.term_to_binary(proof.result))
    |> Base.encode16(case: :lower)
  end

  defp matching_report?(
         %{
           kind: :reported_progressive_event, status: :provisional,
           seq: seq, actor_kind: actor, action: action, relation: relation,
           landmark: landmark, reading: reading,
           typed_proof_sha256: stored_digest,
           arrival: :not_established, event_existence: :not_asserted,
           referent_verified?: false, event_occurrence_verified?: false,
           provenance: :revalidated_event_composition,
           verification: :conversation_context_only,
           factual_authority?: false
         },
         %{
           aspect: :progressive, predicate: required_action,
           actor: %{kind: required_actor, referent: :unresolved},
           spatial: %{relation: required_relation, landmark_kind: required_landmark,
                      reading: required_reading, arrival: :not_established},
           event_existence: :not_asserted
         },
         digest
       ) when is_integer(seq) and seq > 0 and
              is_binary(digest) and byte_size(digest) == 64 do
    stored_digest == digest and actor == required_actor and
      action == required_action and relation == required_relation and
      landmark == required_landmark and reading == required_reading
  end

  defp matching_report?(_, _, _), do: false

  defp metadata(sequence, matches) do
    %{
      action: :conversation_recall,
      response_source: :symbolic_conversation,
      memory_source: :same_session_progressive_report,
      provenance: :revalidated_event_composition,
      requested_slot: :reported_event_confirmation,
      report_seq: sequence,
      matching_reports: matches,
      retrieval_found?: matches > 0,
      scope: :same_session,
      event_occurrence_verified?: false,
      actual_event_chronology_verified?: false,
      same_actor_identity_verified?: false,
      factual_authority?: false,
      llm_called?: false
    }
  end
end
