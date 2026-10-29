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
  alias Core.Syntax.Signatures
  alias Core.Semantic.ProvisionalInterpretation
  alias Core.Semantic.ParticipantQuestions

  @fuzzy_confirmation_window_ms 5 * 60 * 1_000

  @max_query_bytes 256
  @max_public_candidates 5
  @max_clarified_followup_questions 3

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
  The next same-session question may request an actor's activity or described
  location from one speaker-confirmed, fingerprint-replayed progressive frame.
  The requested role is selected by ParticipantQuestions, not by inspecting
  the answer's text. No verified event or word meaning is implied.
  """
  @spec confirmed_fuzzy_event(term(), String.t()) :: {:ok, map()} | :not_applicable
  def confirmed_fuzzy_event(session, question) when is_binary(question) do
    with {:forward, actor, requested_role, constraints} <-
           ParticipantQuestions.parse(question),
         true <- requested_role in [:predicate, :location] and is_map(constraints),
         {:ok, resolution, request, continuity_questions} <-
           active_clarified_pair(session, actor),
         %{
           kind: :fuzzy_clarification_resolution,
           status: :speaker_confirmed_hypothesis,
           actor: subject, original: original, proposed: proposed,
           request_seq: request_seq,
           source_turn_sha256: digest,
           verification: :conversation_context_only,
           factual_authority?: false
         } <- resolution,
         %{
           kind: :fuzzy_clarification_request,
           status: :provisional,
           seq: ^request_seq,
           actor: ^subject, original: ^original, proposed: ^proposed,
           source_turn_sha256: ^digest,
           source_tokens: source_tokens,
           verification: :conversation_context_only,
           factual_authority?: false
         } <- request,
         true <- is_binary(subject) and String.downcase(actor) == subject,
         true <- is_binary(original) and is_binary(proposed) and
                   original != proposed,
         {:ok, clause} <- replay_fuzzy_source(source_tokens, original, digest),
         witness <- verified_source_provisional(request, source_tokens, clause),
         true <- witness != :invalid_source_evidence,
         true <- clause.actor == subject,
         true <- Enum.all?(constraints, fn
           {:predicate, predicate} -> predicate == proposed
           {:position, position} -> position == clause.position
           _ -> false
         end),
         true <- String.ends_with?(original, "ing") and
                   String.ends_with?(proposed, "ing") do
      {:ok, %{
        actor: subject,
        original: original,
        proposed: proposed,
        aspect: :progressive,
        location: clause.location,
        position: clause.position,
        requested_role: requested_role,
        question_constraints: constraints,
        continuity_questions: continuity_questions,
        source_turn_sha256: digest,
        source_request_seq: request_seq,
        role_evidence: if(is_map(witness),
          do: :validated_original_provisional_frame,
          else: semantic_role_evidence(request, source_tokens)),
        source_provisional: witness,
        status: :speaker_confirmed_spelling,
        source: :same_session_clarification,
        factual_authority?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  def confirmed_fuzzy_event(_, _), do: :not_applicable

  # Only previously answered, explicitly matched questions can extend the
  # pair's lifetime. Any intervening statement, different actor, correction,
  # malformed marker, or overflow stops the scan; no arbitrary transcript
  # search and no owner-wide episodic inference.
  defp active_clarified_pair(session, actor) do
    records = entries(session)

    {markers, remainder} =
      Enum.split_while(records, fn
        %{kind: :clarified_event_query} -> true
        _ -> false
      end)

    case remainder do
      [%{kind: :fuzzy_clarification_resolution} = resolution,
       %{kind: :fuzzy_clarification_request, seq: request_seq} = request | _] ->
        valid_markers? =
          length(markers) <= @max_clarified_followup_questions and
            is_integer(request_seq) and
            Enum.all?(markers, fn
              %{kind: :clarified_event_query, status: :role_projected,
                actor: ^actor, source_request_seq: ^request_seq,
                source_turn_sha256: digest,
                requested_role: role,
                provenance: :revalidated_same_session_role_answer,
                verification: :conversation_context_only,
                factual_authority?: false}
              when is_binary(digest) and role in [:predicate, :location] ->
                digest == Map.get(request, :source_turn_sha256)

              _ -> false
            end) and
            Enum.all?(Enum.chunk_every(markers ++ [resolution], 2, 1, :discard),
              fn
                [%{seq: newer}, %{seq: older}]
                when is_integer(newer) and is_integer(older) ->
                  newer == older + 1

                _ -> false
              end)

        if valid_markers?, do: {:ok, resolution, request, length(markers)},
          else: :not_applicable

      _ -> :not_applicable
    end
  end

  @doc """
  Compatibility entry point for the strictly activity-directed query.
  A location query still abstains here; use confirmed_fuzzy_event/2 for
  question-selected predicate or location projection.
  """
  @spec confirmed_fuzzy_activity(term(), String.t()) :: {:ok, map()} | :not_applicable
  def confirmed_fuzzy_activity(session, question) when is_binary(question) do
    case ParticipantQuestions.parse(question) do
      {:forward, _, :predicate, constraints} when constraints == %{} ->
        case entries(session) do
          [%{kind: :fuzzy_clarification_resolution} | _] ->
            confirmed_fuzzy_event(session, question)

          _ -> :not_applicable
        end

      _ -> :not_applicable
    end
  end

  def confirmed_fuzzy_activity(_, _), do: :not_applicable

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
      _ ->
        case clarified_query_record(si) do
          %{} = record -> record
          _ -> semantic_record_without_fuzzy(si)
        end
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

  # A successful question-directed projection creates a compact, factual-
  # authority-free marker. Only a *shown answer* with exactly matching frame,
  # role, and source provenance may extend the active clarification pair.
  # No raw question or answer text is retained.
  defp clarified_query_record(si) do
    meta = Map.get(si, :response_meta)
    session = Map.get(si, :session_id)
    text = Map.get(si, :sentence)

    with %{
           memory_source: :same_session_clarified_event,
           response_source: :symbolic_conversation,
           semantic_frame: frame,
           requested_role: role,
           factual_authority?: false
         } <- meta,
         true <- role in [:predicate, :location],
         true <- is_binary(text) and is_binary(Map.get(si, :response_text)),
         {:ok, answer, proof_meta} <- Core.Semantic.ClarifiedEvent.answer(session, text),
         true <- answer == Map.get(si, :response_text),
         true <- proof_meta.semantic_frame == frame,
         true <- proof_meta.requested_role == role,
         %{actor: %{surface: actor},
           source_request_seq: request_seq,
           source_turn_sha256: digest,
           evidence: %{question_role: ^role},
           factual_authority?: false} <- frame,
         true <- is_binary(actor) and is_integer(request_seq) and
                   is_binary(digest) do
      %{
        kind: :clarified_event_query,
        status: :role_projected,
        actor: actor,
        requested_role: role,
        source_request_seq: request_seq,
        source_turn_sha256: digest,
        provenance: :revalidated_same_session_role_answer,
        verification: :conversation_context_only,
        factual_authority?: false
      }
    else
      _ -> nil
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
                  case fuzzy_surface_clause(sentence, original) do
                    {:ok, %{actor: actor, tokens: tokens}} ->
                      %{
                        kind: :fuzzy_clarification_request,
                        status: :provisional,
                        original: original,
                        proposed: proposed,
                        actor: actor,
                        source_tokens: tokens,
                        source_syntax: observed_gap_syntax(si),
                        source_provisional: observed_source_provisional(si, original, tokens),
                        source_turn_sha256: :crypto.hash(:sha256, sentence)
                          |> Base.encode16(case: :lower),
                        asked_at_ms: System.system_time(:millisecond),
                        provenance: :actual_attention_fuzzy_question,
                        verification: :conversation_context_only,
                        factual_authority?: false
                      }

                    _ -> nil
                  end
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
              actor: pending.actor,
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

  # An independently observed pre-LIFG lexical gap can enrich the
  # clarification's role provenance. It never proves the missing word's
  # dictionary sense or bypasses the source fingerprint replay.
  defp observed_gap_syntax(si) do
    expected = %{
      subject_determiner: 0, subject_head: 1, auxiliary: 2,
      predicate: 3, preposition: 4, location_determiner: 5,
      location_head: 6
    }

    original = List.wrap(Map.get(si, :sentence_signatures))
    independent = Signatures.detect(si)

    Enum.find_value(original, fn candidate ->
      if candidate in independent and
           match?(%{id: :progressive_lexical_gap_candidate,
                    span: {0, 6}, source: :core_syntax}, candidate) and
           Map.get(candidate, :roles) == expected and
           is_list(Map.get(candidate, :evidence)) and
           :unresolved_progressive_predicate in candidate.evidence do
        Map.take(candidate, [:id, :span, :roles, :evidence, :source,
          :source_span_sha256])
      end
    end)
  end

  defp valid_gap_syntax?(%{
         id: :progressive_lexical_gap_candidate, span: {0, 6},
         source: :core_syntax, source_span_sha256: source_digest,
         roles: %{
           subject_determiner: 0, subject_head: 1, auxiliary: 2,
           predicate: 3, preposition: 4,
           location_determiner: 5, location_head: 6
         },
         evidence: evidence
       }, words) when is_list(evidence) and is_list(words) and length(words) == 7 and
                     is_binary(source_digest) do
    Enum.all?(
      [:contiguous_surface, :attested_nominal_subject,
       :unresolved_progressive_predicate, :attested_preposition,
       :attested_nominal_location],
      &(&1 in evidence)
    ) and Enum.all?(words, &is_binary/1) and
      source_digest == span_digest(words)
  end

  defp valid_gap_syntax?(_, _), do: false

  defp semantic_role_evidence(request, words) do
    case Map.get(request, :source_provisional) do
      nil ->
        if valid_gap_syntax?(Map.get(request, :source_syntax), words),
          do: :attested_core_syntax_gap, else: :surface_clause_only

      _ ->
        if valid_gap_syntax?(Map.get(request, :source_syntax), words),
          do: :validated_original_provisional_frame, else: :invalid_source_evidence
    end
  end

  # A nonnil witness must rebind to the *same* source lexemes, source syntax,
  # actor, predicate and locative roles. A merely plausible replacement is
  # insufficient, and corrupt evidence never falls back to the surface path.
  defp verified_source_provisional(request, words, clause) do
    case Map.get(request, :source_provisional) do
      %{
        kind: :unresolved_progressive_predicate,
        operation: :clarify_unresolved_predicate,
        source_span_sha256: digest,
        actor: actor, predicate: predicate,
        position: position, location: location,
        factual_authority?: false
      } = witness ->
        if is_binary(digest) and digest == span_digest(words) and
             valid_gap_syntax?(Map.get(request, :source_syntax), words) and
             actor == clause.actor and
             predicate == String.downcase(Enum.at(words, 3)) and
             position == clause.position and location == clause.location and
             Map.get(witness, :speaker_clarification_confirmed?) == false and
             Map.get(witness, :event_occurrence_verified?) == false do
          witness
        else
          :invalid_source_evidence
        end

      nil -> nil
      _ -> :invalid_source_evidence
    end
  end

  defp observed_source_provisional(si, original, words) do
    case ProvisionalInterpretation.verified(si) do
      {:ok, %{
        kind: :unresolved_progressive_predicate,
        operation: :clarify_unresolved_predicate,
        source_span_sha256: digest,
        original_sentence: sentence,
        target: %{lemma: ^original},
        participants: %{
          actor: %{surface: actor},
          predicate: %{surface: ^original, lexical_sense_verified?: false},
          location: %{relation: position, surface: location,
                      event_location_verified?: false}
        },
        speaker_clarification_confirmed?: false,
        event_occurrence_verified?: false,
        factual_authority?: false
      }} ->
        if sentence == Map.get(si, :sentence) and
             digest == span_digest(words) and
             valid_gap_syntax?(observed_gap_syntax(si), words) and
             is_binary(actor) and is_binary(position) and
             is_binary(location) do
          %{kind: :unresolved_progressive_predicate,
            operation: :clarify_unresolved_predicate,
            source_span_sha256: digest,
            actor: actor |> String.downcase() |> String.replace(~r/\A(?:a|an|the) /u, ""),
            predicate: original, position: position,
            location: location,
            speaker_clarification_confirmed?: false,
            event_occurrence_verified?: false,
            factual_authority?: false}
        else
          nil
        end

      _ -> nil
    end
  end

  defp span_digest(words) when is_list(words) do
    :crypto.hash(:sha256, Enum.map_join(words, " ", &String.downcase/1))
    |> Base.encode16(case: :lower)
  end

  # A source record retains bounded source tokens rather than replacing the
  # utterance. Replay the same strict structural grammar and exact original
  # SHA before the inferred subject/activity/location can be used as a frame.
  @fuzzy_clause ~r/\A(the|a|an) (\p{L}{2,32}) (is) (\p{L}{5,24}) (on|in|under|beside|above|below) (the|a|an) (\p{L}{2,32})\.\z/iu

  defp fuzzy_surface_clause(sentence, unknown) when is_binary(sentence) do
    case Regex.run(@fuzzy_clause, sentence) do
      [_, subject_det, actor, aux, activity, position, location_det, landmark] ->
        if String.downcase(activity) == unknown do
          {:ok, %{
            actor: String.downcase(actor),
            position: String.downcase(position),
            location: String.downcase(location_det <> " " <> landmark),
            tokens: [subject_det, actor, aux, activity,
              position, location_det, landmark]
          }}
        else
          :not_applicable
        end

      _ -> :not_applicable
    end
  end

  defp fuzzy_surface_clause(_, _), do: :not_applicable

  defp replay_fuzzy_source(tokens, original, digest)
       when is_list(tokens) and length(tokens) == 7 and
            is_binary(original) and is_binary(digest) do
    with true <- Enum.all?(tokens, &(is_binary(&1) and byte_size(&1) in 1..40)),
         sentence <- Enum.join(tokens, " ") <> ".",
         ^digest <- (:crypto.hash(:sha256, sentence)
                     |> Base.encode16(case: :lower)),
         {:ok, clause} <- fuzzy_surface_clause(sentence, original),
         true <- clause.tokens == tokens do
      {:ok, clause}
    else
      _ -> :not_applicable
    end
  end

  defp replay_fuzzy_source(_, _, _), do: :not_applicable

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
