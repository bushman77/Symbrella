defmodule Core.Semantic.FinitePastDiscourse do
  @moduledoc """
  Phase 3, gate 1: a pure, session-scoped *candidate* for later discourse
  integration of a verified finite-past locative property.

  A saved proof is not trusted alone. Project/2 independently re-derives it
  from the current SemanticInput, including grammar, LIFG scoring witnesses
  and canonical lemma dictionary evidence. Only an exact match is admitted.

  This module does NOT write to the Discourse Ledger, Working Memory or
  Hippocampus; it does not resolve referent identity, assert event occurrence,
  answer a question, or modify LIFG winners. A reported declarative candidate
  is not a verified real-world assertion or factual authority.
  """

  alias Core.Semantic.Composition.{
    FinitePastLocative,
    FinitePastLocativeIntegration
  }

  @max_sentence_bytes 192

  @doc """
  Return a bounded candidate for a current, explicitly session-scoped
  user/prod turn, or abstain. Options are passed only to the existing
  finite-past integration revalidator (e.g. a deterministic test lookup).
  """
  @spec project(map(), keyword()) :: {:ok, map()} | :not_applicable
  def project(si, opts \\ [])

  def project(%{} = si, opts) when is_list(opts) do
    with true <- Keyword.keyword?(opts),
         true <- Map.get(si, :source) in [:user, :prod],
         true <- Map.get(si, :intent) not in [:ask, :ask_info, :question,
                                              :command, :define, :imperative],
         true <- valid_session?(Map.get(si, :session_id)),
         sentence when is_binary(sentence) <- Map.get(si, :sentence),
         true <- declarative_surface?(sentence),
         %{kind: :finite_past_locative_property} = attached <-
           Map.get(si, :finite_past_locative_derivation),
         :ok <- FinitePastLocative.verify(attached),
         {:ok, ^attached} <- FinitePastLocativeIntegration.derive(si, opts),
         {:ok, candidate} <- describe(attached, sentence) do
      {:ok, candidate}
    else
      _ -> :not_applicable
    end
  end

  def project(_, _), do: :not_applicable

  # A closed complete-clause surface is a *syntactic* declarative candidate,
  # not proof of actual speaker commitment. Reject questions, imperatives,
  # quotation, negation, modality, and extra-clause/reporting material.
  defp declarative_surface?(sentence) when is_binary(sentence) do
    byte_size(sentence) <= @max_sentence_bytes and
      Regex.match?(
        ~r/\A\s*(?:the|a|an)\s+\p{L}+\s+\p{L}+\s+(?:on|in|under|beside|above|below)\s+(?:the|a|an)\s+\p{L}+\.?\s*\z/iu,
        sentence
      )
  end

  defp describe(%{
         kind: :finite_past_locative_property,
         status: :derived,
         construction: :finite_past_locative_candidate,
         epistemic_status: :conditional_on_supplied_evidence,
         semantic_compatibility_proven?: false,
         factual_authority?: false,
         event_semantics: %{
           tense: :past,
           predicate: predicate,
           actor: %{kind: actor, referent: :unresolved},
           spatial: %{
             relation: relation,
             landmark_kind: landmark,
             arrival: :not_established
           },
           event_existence: :not_asserted
         }
       } = proof, sentence)
       when is_atom(predicate) and is_atom(actor) and is_atom(relation) and
              is_atom(landmark) do
    {:ok, %{
      kind: :reported_finite_past_locative_candidate,
      status: :provisional,
      tense: :past,
      predicate: predicate,
      actor_kind: actor,
      relation: relation,
      landmark_kind: landmark,
      speaker: :user,
      speech_act: :declarative_candidate,
      session_scope: :current_session_only,
      source_turn_sha256: source_hash(sentence),
      typed_proof_sha256: proof_hash(proof.result),
      provenance: :revalidated_current_turn_finite_past_composition,
      verification: :conversation_context_only,
      event_existence: :not_asserted,
      event_occurrence_verified?: false,
      referent_identity_verified?: false,
      semantic_compatibility_proven?: false,
      factual_authority?: false
    }}
  end

  defp describe(_, _), do: :not_applicable

  # Match the existing Ledger digest conventions for later, explicit joins.
  defp source_hash(sentence),
    do: :crypto.hash(:sha256, sentence) |> Base.encode16(case: :lower)

  defp proof_hash(term),
    do: :crypto.hash(:sha256, :erlang.term_to_binary(term))
        |> Base.encode16(case: :lower)

  defp valid_session?(session),
    do: is_binary(session) and byte_size(session) in 1..128
end
