defmodule Core.Semantic.FinitePastDiscourseQuestion do
  @moduledoc """
  Symbolic Phase 3 gate 3: strictly read-only, same-session projection of an
  explicitly asked past-tense *speaker-description* question.

  This does not establish that an event happened or that repeated nouns are
  the same entity. It returns a typed reported property, NOT a response string,
  and does not integrate with the live planner, memory admission, LIFG or LLM.

  Only previously admitted finite-past records are eligible. An eligible
  record must be the most recent turn, and multiple reports of the same
  actor-kind never resolve through recency or apparent textual identity.
  """

  alias Core.Discourse.Ledger

  @actors [:bird, :cat, :dog]
  @actions [:sit, :sleep]
  @relations [:on, :in, :under, :beside, :above, :below]
  @landmarks [:fence, :tree, :table]
  @max_question_bytes 128
  @max_ambiguity_candidates 5

  @type role :: :predicate | :location
  @spec project(term(), String.t()) ::
          {:ok, map()} | {:ambiguous, [map()]} | :not_applicable
  def project(session, question)
      when is_binary(session) and is_binary(question) do
    with true <- valid_session?(session),
         {:ok, actor, role} <- parse_question(question),
         [latest | _] = records <- Ledger.entries(session),
         true <- same_actor_record?(latest, actor),
         {:ok, _} <- validated_report(latest, actor),
         true <- not Enum.any?(records, &blocking_record?(&1, actor)),
         reports <- Enum.flat_map(records, fn record ->
           case validated_report(record, actor) do
             {:ok, report} -> [report]
             _ -> []
           end
         end) do
      case reports do
        [single] ->
          {:ok, project_role(single, role)}

        [_ | _] ->
          {:ambiguous,
           reports
           |> Enum.take(@max_ambiguity_candidates)
           |> Enum.map(&ambiguous_source/1)}

        _ ->
          :not_applicable
      end
    else
      _ -> :not_applicable
    end
  end

  def project(_, _), do: :not_applicable

  @doc """
  Parses only whole, explicit, past-tense questions about an actor kind.
  A recognized noun kind is not a verified unique entity or anaphoric binding.
  """
  @spec parse_question(String.t()) :: {:ok, atom(), role()} | :not_applicable
  def parse_question(question)
      when is_binary(question) and byte_size(question) <= @max_question_bytes do
    case Regex.named_captures(
           ~r/\A\s*(?<question>what\s+was\s+the\s+(?<actor_activity>bird|cat|dog)\s+doing|where\s+was\s+the\s+(?<actor_location>bird|cat|dog))\?\s*\z/iu,
           question
         ) do
      %{"actor_activity" => actor} when is_binary(actor) and actor != "" ->
        {:ok, String.downcase(actor) |> String.to_existing_atom(), :predicate}

      %{"actor_location" => actor} when is_binary(actor) and actor != "" ->
        {:ok, String.downcase(actor) |> String.to_existing_atom(), :location}

      _ -> :not_applicable
    end
  end

  def parse_question(_), do: :not_applicable

  defp project_role(record, role) do
    descriptor =
      case role do
        :predicate ->
          %{kind: :reported_predicate, lemma: record.predicate, tense: :past}

        :location ->
          %{kind: :reported_location, relation: record.relation,
            landmark_kind: record.landmark_kind, tense: :past}
      end

    %{
      status: :speaker_description_candidate,
      requested_role: role,
      actor_kind: record.actor_kind,
      description: descriptor,
      source_turn_seq: record.seq,
      source_turn_sha256: record.source_turn_sha256,
      typed_proof_sha256: record.typed_proof_sha256,
      evidence: :admitted_revalidated_finite_past_report,
      verification: :conversation_context_only,
      referent_identity_verified?: false,
      event_occurrence_verified?: false,
      factual_authority?: false
    }
  end

  defp ambiguous_source(record),
    do: %{
      source_turn_seq: record.seq,
      source_turn_sha256: record.source_turn_sha256,
      actor_kind: record.actor_kind,
      referent_identity_verified?: false,
      factual_authority?: false
    }

  # A malformed or unresolved record about this actor is an uncertainty
  # barrier; do not skip it to retrieve older, easier evidence. Unknown records
  # are not automatically judged to concern the same actor.
  defp blocking_record?(record, actor) do
    same_actor_record?(record, actor) and
      not match?({:ok, _}, validated_report(record, actor))
  end

  defp same_actor_record?(%{actor_kind: candidate}, actor)
       when actor in @actors and candidate == actor, do: true

  defp same_actor_record?(%{actor: actor}, kind)
       when is_binary(actor) and kind in @actors,
       do: String.downcase(actor) == Atom.to_string(kind)

  defp same_actor_record?(%{antecedent_head: actor}, kind)
       when is_binary(actor) and kind in @actors,
       do: String.downcase(actor) == Atom.to_string(kind)

  defp same_actor_record?(%{head: actor}, kind)
       when is_binary(actor) and kind in @actors,
       do: String.downcase(actor) == Atom.to_string(kind)

  defp same_actor_record?(_, _), do: false

  defp validated_report(
         %{
           kind: :reported_finite_past_locative_candidate,
           seq: seq,
           status: :provisional,
           tense: :past,
           predicate: predicate,
           actor_kind: actor_kind,
           relation: relation,
           landmark_kind: landmark,
           speaker: :user,
           speech_act: :declarative_candidate,
           session_scope: :current_session_only,
           provenance: :revalidated_current_turn_finite_past_composition,
           verification: :conversation_context_only,
           source_turn_sha256: source_hash,
           typed_proof_sha256: proof_hash,
           event_existence: :not_asserted,
           event_occurrence_verified?: false,
           referent_identity_verified?: false,
           semantic_compatibility_proven?: false,
           factual_authority?: false
         } = record, actor)
       when is_integer(seq) and seq > 0 and actor in @actors and
              actor_kind == actor and
              predicate in @actions and relation in @relations and
              landmark in @landmarks do
    if hex_digest?(source_hash) and hex_digest?(proof_hash),
      do: {:ok, record}, else: :not_applicable
  end

  defp validated_report(_, _), do: :not_applicable

  defp hex_digest?(value) when is_binary(value),
    do: Regex.match?(~r/\A[0-9a-f]{64}\z/, value)

  defp hex_digest?(_), do: false

  defp valid_session?(session),
    do: byte_size(session) in 1..128
end
