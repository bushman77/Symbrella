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
  alias Core.Semantic.{Composition, DiscourseComposition, TemporalSentence}
  alias Core.Pipeline.Trace

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
    case TemporalSentence.verified(si) do
      {:ok, meaning} -> temporal_claim_record(meaning)
      :not_applicable -> ordinary_semantic_record(si)
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
