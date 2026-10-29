defmodule Core.Discourse.Ledger do
  @moduledoc """
  Session-scoped, append-only conversational *semantic evidence* ledger (V1).

  Every eligible user turn advances a sequence; independently replayed
  nominal descriptions, validated provisional spatial reports, and explicitly
  witnessed temporal claims may attach bounded semantic records. No transcript
  text, assumed referent identity,
  verified real-world event, dictionary sense guess, or LLM inference is
  stored as a fact.

  Query explicit noun-phrase mentions across the entire retained session,
  not merely the preceding WM frame. Multiple possible antecedents produce
  :ambiguous, never a recency-based guess. Unqualified "it" remains handled
  by the active WM/discourse rules; a distant pronoun alone cannot revive an
  inactive topic.

  Storage is protected ETS under Ledger.Owner: at most 4096 turns per session
  by default (configurable up to 16_384). This V1 is NOT durable across restarts.
  """
  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Discourse.Ledger.Owner
  alias Core.Semantic.{Composition, DiscourseComposition, TemporalSentence,
    TemporalClaimReconciliation, TemporalClarificationResolution}
  alias Core.Pipeline.Trace
  alias Core.Text.Fuzzy

  @fuzzy_confirmation_window_ms 5 * 60 * 1_000

  @max_query_bytes 256
  @max_public_candidates 5

  @doc "Store the current independently interpreted user turn, without altering SI."
  @spec record(map(), keyword()) :: map()
  def record(si, opts \\ [])
  def record(%{} = si, opts) when is_list(opts) do
    session = Map.get(si, :session_id)
    sentence = Map.get(si, :sentence)

    if enabled?(opts) and valid_session?(session) and
         Map.get(si, :source) in [:user, :prod, :test] and
         is_binary(sentence) and sentence != "" do
      payload = semantic_record(si)
      case owner_append(session, payload) do
        {:ok, sequence} ->
          Trace.append(si, :discourse_ledger,
            decision: :recorded,
            reason: :session_semantic_timeline,
            meta: %{sequence: sequence, kind: payload.kind,
              factual_authority?: false})

        _ -> si
      end
    else
      si
    end
  end
  def record(si, _), do: si

  @doc "Return retained records, newest first, within exactly this session."
  @spec entries(term()) :: [map()]
  def entries(session) do
    if valid_session?(session) and is_pid(Process.whereis(Owner)),
      do: Owner.entries(session), else: []
  end

  @doc """
  The latest ledger entry must be an actually issued, same-session fuzzy
  question, and the immediate user response must explicitly confirm its
  proposed word. This is a discourse preference, NOT a dictionary definition.
  """
  @spec pending_fuzzy_confirmation(term(), String.t()) ::
          {:ok, map()} | :not_applicable
  def pending_fuzzy_confirmation(session, text) when is_binary(text) do
    case entries(session) do
      [%{
        kind: :fuzzy_clarification_request, status: :provisional,
        original: original, proposed: proposed,
        source_turn_sha256: digest, asked_at_ms: asked,
        verification: :conversation_context_only,
        factual_authority?: false
      } = pending | _] ->
        now = System.system_time(:millisecond)
        normalized = text |> String.downcase() |> String.trim()
        expected = String.downcase(proposed)
        explicit_confirmation? =
          normalized in ["yes", "yes.", "yeah", "yeah.", "yep", "yep.",
            "yes #{expected}", "yes #{expected}.",
            "yes, #{expected}", "yes, #{expected}.",
            "yeah #{expected}", "yeah, #{expected}.",
            "yep #{expected}", "yep, #{expected}."]

        if is_binary(original) and is_binary(proposed) and
             Regex.match?(~r/\A\p{L}{5,24}\z/u, original) and
             Regex.match?(~r/\A\p{L}{5,24}\z/u, proposed) and
             is_binary(digest) and byte_size(digest) == 64 and
             is_integer(asked) and now >= asked and
             now - asked <= @fuzzy_confirmation_window_ms and
             explicit_confirmation? do
          {:ok, pending}
        else
          :not_applicable
        end

      _ ->
        :not_applicable
    end
  end

  def pending_fuzzy_confirmation(_, _), do: :not_applicable

  @doc """
  Resolve an explicitly named noun phrase from retained semantic records.
  A unique match is a *candidate* only, not verified identity or existence.
  """
  @spec resolve(term(), String.t()) ::
          {:ok, map()} | {:ambiguous, [map()]} | :not_found
  def resolve(session, text) when is_binary(text) do
    if valid_session?(session) and byte_size(text) <= @max_query_bytes do
      matches =
        session
        |> entries()
        |> Enum.filter(&(&1.kind == :nominal_description))
        |> Enum.flat_map(fn entry ->
          case mention_specificity(text, entry) do
            size when is_integer(size) and size > 0 -> [{size, entry}]
            _ -> []
          end
        end)

      case matches do
        [] ->
          :not_found

        _ ->
          specificity = matches |> Enum.map(&elem(&1, 0)) |> Enum.max()
          best = for {^specificity, entry} <- matches, do: public_candidate(entry)

          case best do
            [candidate] -> {:ok, candidate}
            _ -> {:ambiguous, Enum.take(best, @max_public_candidates)}
          end
      end
    else
      :not_found
    end
  end
  def resolve(_, _), do: :not_found

  @doc "Attach current-turn retrieval as conversation context, not a factual premise."
  @spec recall(map(), keyword()) :: map()
  def recall(si, opts \\ [])
  def recall(%{} = si, opts) when is_list(opts) do
    if enabled?(opts) do
      case resolve(Map.get(si, :session_id), Map.get(si, :sentence)) do
        {:ok, candidate} ->
          Map.put(si, :discourse_context, %{
            status: :candidate_same_session, source: :discourse_ledger,
            candidate: candidate, verification: :conversation_context_only,
            factual_authority?: false
          })

        {:ambiguous, candidates} ->
          Map.put(si, :discourse_context, %{
            status: :ambiguous, source: :discourse_ledger,
            candidates: candidates, verification: :conversation_context_only,
            factual_authority?: false
          })

        _ -> si
      end
    else
      si
    end
  end
  def recall(si, _), do: si

  @doc """
  Re-check both the recorded reference and the current explicit mention.
  No implicit pronoun or externally verified fact can be produced here.
  """
  @spec reference(map()) :: map() | nil
  def reference(%{} = si) do
    context = Map.get(si, :discourse_context)

    with %{status: :candidate_same_session, source: :discourse_ledger,
           candidate: candidate} <- context,
         {:ok, ^candidate} <- resolve(Map.get(si, :session_id), Map.get(si, :sentence)),
         %{head: head, modifiers: modifiers, surface: surface, seq: seq,
           provenance: :revalidated_nominal_composition} <- candidate do
      %{
        pronoun: nil, antecedent: surface, antecedent_head: head,
        antecedent_modifiers: modifiers, source: :discourse_ledger,
        evidence: :historical_verified_nominal_composition,
        antecedent_turn_seq: seq,
        referent_status: :candidate_same_session,
        verification: :conversation_context_only,
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end
  def reference(_), do: nil

  @doc "Explicit session reset; also clears its lexical reference candidates."
  @spec clear(term()) :: :ok
  def clear(session) do
    if valid_session?(session) and is_pid(Process.whereis(Owner)),
      do: Owner.clear(session), else: :ok
  end

  defp semantic_record(si) do
    case fuzzy_dialogue_record(si) do
      %{} = record -> record
      _ -> semantic_record_without_fuzzy(si)
    end
  end

  defp semantic_record_without_fuzzy(si) do
    case TemporalSentence.verified(si) do
      {:ok, meaning} -> temporal_claim_record(meaning)
      :not_applicable ->
        case TemporalClarificationResolution.verified(si) do
          {:ok, meaning} -> temporal_resolution_record(meaning)
          :not_applicable ->
            case TemporalClaimReconciliation.verified(si) do
              {:ok, meaning} -> temporal_request_record(meaning)
              :not_applicable -> ordinary_semantic_record(si)
            end
        end
    end
  end

  # Persist only a question that was actually selected and shown by the
  # response planner, never an unselected fuzzy suggestion from pre-intent
  # normalization. The source sentence itself is not copied into the ledger.
  defp fuzzy_dialogue_record(si) do
    meta = Map.get(si, :response_meta, %{})
    reflection = if is_map(meta), do: Map.get(meta, :response_reflection), else: nil
    response = Map.get(si, :response_text)
    sentence = Map.get(si, :sentence)
    comprehension = Map.get(si, :comprehension, %{})
    unresolved = if is_map(comprehension), do: Map.get(comprehension, :unresolved, []), else: []

    cond do
      not is_map(meta) or
          Map.get(meta, :response_source) != :wm_attention_fallback or
          Map.get(meta, :risk_bucket) != :low ->
        nil

      not is_binary(sentence) or byte_size(sentence) > 256 or
          not is_binary(response) ->
        nil

      match?(%{status: :attention_comprehension_repair}, reflection) ->
        Enum.find_value(List.wrap(unresolved), fn term ->
          if is_binary(term) do
            case Fuzzy.suggest_word(term, sentence) do
              %{original: original, replacement: proposed, status: :hypothesis_only}
              when is_binary(original) and is_binary(proposed) ->
                if String.ends_with?(response, ~s|Did you mean "#{proposed}"?|) do
                  %{
                    kind: :fuzzy_clarification_request,
                    status: :provisional,
                    original: original,
                    proposed: proposed,
                    source_turn_sha256: :crypto.hash(:sha256, sentence)
                      |> Base.encode16(case: :lower),
                    asked_at_ms: System.system_time(:millisecond),
                    provenance: :actual_attention_fuzzy_question,
                    verification: :conversation_context_only,
                    factual_authority?: false
                  }
                end

              _ -> nil
            end
          end
        end)

      match?(%{status: :fuzzy_clarification_confirmed}, reflection) ->
        case pending_fuzzy_confirmation(Map.get(si, :session_id), sentence) do
          {:ok, pending} ->
            %{
              kind: :fuzzy_clarification_resolution,
              status: :speaker_confirmed_hypothesis,
              original: pending.original,
              proposed: pending.proposed,
              request_seq: pending.seq,
              source_turn_sha256: pending.source_turn_sha256,
              provenance: :explicit_immediate_user_confirmation,
              verification: :conversation_context_only,
              factual_authority?: false
            }

          _ -> nil
        end

      true -> nil
    end
  end

  defp ordinary_semantic_record(si) do
    core = %{kind: :uninterpreted, status: :no_verified_semantics,
      factual_authority?: false}

    case {Composition.verified(si), DiscourseComposition.verified(si)} do
      {{:ok, %{kind: :nominal_property, construction: :stacked_adjective_noun,
               factual_authority?: false} = proof}, _} ->
        case verified_nominal(si) do
          %{head: head, modifiers: modifiers, surface: surface} ->
            Map.merge(core, %{
              kind: :nominal_description, status: :provisional,
              head: head, modifiers: modifiers, surface: surface,
              provenance: :revalidated_nominal_composition,
              proof_sha256: proof_hash(proof.result),
              proof_steps: length(proof.steps),
              verification: :conversation_context_only
            })

          _ -> core
        end

      {_, {:ok, %{kind: :anaphoric_spatial_report,
                  factual_authority?: false,
                  compositional_evidence: %{
                    typed_cross_turn_join_verified?: true,
                    typed_cross_turn_join: %{formally_verified?: true,
                      type: {:arrow, :entity, {:arrow, :event, :truth}},
                      result: joined}}} = meaning}} ->
        Map.merge(core, %{
          kind: :reported_spatial_description, status: :provisional,
          antecedent: meaning.antecedent.description,
          antecedent_head: meaning.antecedent.head,
          action: meaning.event_semantics.predicate,
          relation: meaning.relation,
          landmark: meaning.event_semantics.spatial.landmark_kind,
          typed_join_verified?: true,
          typed_join_type: {:arrow, :entity, {:arrow, :event, :truth}},
          typed_join_proof_sha256: proof_hash(joined),
          event_existence: :not_asserted,
          provenance: :revalidated_cross_turn_composition,
          verification: :conversation_context_only,
          event_occurrence_verified?: false,
          referent_verified?: false
        })

      _ -> core
    end
  end

  # A meaning saved on SI is never sufficient by itself: semantic_record/1
  # requires TemporalSentence.verified/1 to independently rebuild the full
  # two-clause interpretation and its typed proof from current witnesses.
  # This record describes the speaker's ordering CLAIM, not ledger turn order.
  defp temporal_claim_record(%{
         connective: connective,
         temporal_claim: %{
           relation: :before,
           earlier_event_property: earlier,
           later_event_property: later,
           speaker_attributed?: true,
           external_chronology_verified?: false,
           event_occurrence_verified?: false,
           referent_identity_verified?: false
         },
         compositional_evidence: %{
           result: proof,
           temporal_operator_verified?: true,
           event_properties_verified?: true,
           connective_witness_verified?: true
         },
         factual_authority?: false
       }) when connective in [:before, :after] do
    with {:ok, earlier_descriptor} <- temporal_event_descriptor(earlier),
         {:ok, later_descriptor} <- temporal_event_descriptor(later) do
      %{
        kind: :reported_temporal_relation,
        status: :provisional,
        relation: :before,
        connective: connective,
        earlier: earlier_descriptor,
        later: later_descriptor,
        proof_sha256: proof_hash(proof),
        source: :revalidated_temporal_clause_composition,
        provenance: :revalidated_temporal_clause_composition,
        verification: :conversation_context_only,
        speaker_attributed?: true,
        event_occurrence_verified?: false,
        actual_event_chronology_verified?: false,
        referent_identity_verified?: false,
        factual_authority?: false
      }
    else
      _ -> uninterpreted_record()
    end
  end

  defp temporal_claim_record(_), do: uninterpreted_record()

  # Store only a small whitelist of the event property, never arbitrary
  # input text, full lambda AST, unbounded dictionary rows or LIFG scores.
  defp temporal_event_descriptor(%{
         aspect: :progressive,
         predicate: action,
         actor: %{kind: actor_kind, referent: :unresolved},
         spatial: %{relation: relation, landmark_kind: landmark},
         event_existence: :not_asserted
       })
       when action in [:sit, :sleep] and actor_kind in [:bird, :cat, :dog] and
            relation in [:on, :in, :under, :beside, :above, :below] and
            landmark in [:fence, :tree, :table] do
    {:ok, %{action: action, actor_kind: actor_kind,
      relation: relation, landmark: landmark}}
  end

  defp temporal_event_descriptor(_), do: :unsupported

  defp uninterpreted_record do
    %{kind: :uninterpreted, status: :no_verified_semantics,
      factual_authority?: false}
  end

  # A conflict question becomes an explicit pending request, not a hidden
  # dialogue-state assumption. Its report identifiers and proof fingerprints
  # must match the unique independently revalidated candidate.
  defp temporal_request_record(%{
         conflict_evidence: %{
           older_report: %{seq: old_seq, proof_sha256: old_sha},
           newer_report: %{seq: new_seq, proof_sha256: new_sha},
           contradiction_verified?: false,
           factual_authority?: false
         },
         factual_authority?: false
       }) do
    %{
      kind: :temporal_clarification_request,
      status: :provisional,
      older_report_seq: old_seq,
      newer_report_seq: new_seq,
      older_proof_sha256: old_sha,
      newer_proof_sha256: new_sha,
      provenance: :revalidated_temporal_claim_reconciliation,
      verification: :conversation_context_only,
      contradiction_verified?: false,
      factual_authority?: false
    }
  end

  defp temporal_request_record(_), do: uninterpreted_record()

  # This is a separate correction-of-intent record. It never changes the
  # source reports, their mathematical proofs, or event occurrence status.
  defp temporal_resolution_record(%{
         pending_request_seq: pending_seq,
         conflict_pair: pair,
         chosen_report_seq: chosen_seq,
         chosen_claim: claim,
         speaker_attributed?: true,
         contradiction_verified?: false,
         event_occurrence_verified?: false,
         actual_event_chronology_verified?: false,
         referent_identity_verified?: false,
         factual_authority?: false
       }) do
    %{
      kind: :temporal_clarification_resolution,
      status: :provisional,
      pending_request_seq: pending_seq,
      conflict_pair: pair,
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
    }
  end

  defp temporal_resolution_record(_), do: uninterpreted_record()

  defp verified_nominal(si) do
    context = WMComprehension.context_from(si, Map.get(si, :comprehension) || %{})
    focus = Map.get(context, :nominal_focus)
    session = Map.get(si, :session_id)

    if is_map(focus) and focus[:source] == :revalidated_nominal_composition and
         focus[:kind] == :nominal_description and
         focus[:scope_id] == WMComprehension.frame_id(session) and
         focus[:verification] == :conversation_context_only and
         focus[:factual_authority?] == false do
      focus
    end
  end

  defp public_candidate(entry) do
    Map.take(entry, [:seq, :kind, :status, :head, :modifiers, :surface,
      :provenance, :proof_sha256, :verification, :factual_authority?])
  end

  # A named NP must have an explicit determiner and either the whole modifier
  # sequence or a suffix (e.g. "that black bird" for "small black bird").
  # The bare head is eligible only if no other candidate matches it.
  defp mention_specificity(text, %{head: head, modifiers: modifiers})
       when is_binary(head) and is_list(modifiers) do
    normalized =
      text |> String.downcase() |> String.replace("’", "'")

    Enum.map(0..length(modifiers), fn skipped ->
      phrase = Enum.join(Enum.drop(modifiers, skipped) ++ [head], " ")
      regex = Regex.compile!(
        "(?:\\A|[^\\p{L}\\p{N}])(?:that|this|the)\\s+" <>
        Regex.escape(phrase) <> "(?=\\z|[^\\p{L}\\p{N}])", "u"
      )

      if Regex.match?(regex, normalized), do: String.length(phrase), else: 0
    end)
    |> Enum.max(fn -> 0 end)
  end
  defp mention_specificity(_, _), do: 0

  defp proof_hash(term) do
    :crypto.hash(:sha256, :erlang.term_to_binary(term))
    |> Base.encode16(case: :lower)
  end

  defp owner_append(session, payload) do
    if is_pid(Process.whereis(Owner)),
      do: Owner.append(session, payload), else: {:error, :unavailable}
  end

  defp valid_session?(session),
    do: is_binary(session) and byte_size(session) in 1..128

  defp enabled?(opts) do
    config = Application.get_env(:core, __MODULE__, [])
    default =
      if is_list(config), do: Keyword.get(config, :enabled?, true), else: true
    Keyword.get(opts, :discourse_ledger?, default) == true
  end
end
