defmodule Core.Semantic.TemporalEventComposition do
  @moduledoc """
  Temporal Event Composition V1: verified order of two conversational reports.

  Two individually proof-qualified, typed entity/event property records from
  the protected same-session discourse ledger can be ordered by their *report*
  sequence numbers. This does not assert that either event occurred, that
  the events have the same real-world actor, or that they happened in report
  order. Temporal event chronology needs independent temporal evidence.

  Deliberately narrow V1 question:
    "What did I describe before it was sleeping under the tree?"

  The referent description must come from one unique earlier nominal
  composition; both event reports must carry independent typed-join evidence;
  repeated target reports or competing preceding reports are ambiguous.
  """

  alias Core.Discourse.Ledger

  @question ~r/\A\s*what did i describe before it was (sitting|sleeping) (on|in|under|beside|above|below) the (fence|tree|table)\?\s*\z/iu
  @actions %{"sitting" => :sit, "sleeping" => :sleep}
  @relations %{"on" => :on, "in" => :in, "under" => :under,
    "beside" => :beside, "above" => :above, "below" => :below}
  @landmarks %{"fence" => :fence, "tree" => :tree, "table" => :table}
  @event_type {:arrow, :entity, {:arrow, :event, :truth}}

  @doc "Interpret a narrowly attested earlier-report question, read-only."
  @spec question(map()) :: map() | nil
  def question(%{} = si) do
    session = Map.get(si, :session_id)
    sentence = Map.get(si, :sentence)

    with true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- is_binary(session) and byte_size(session) in 1..128,
         true <- is_binary(sentence) and byte_size(sentence) <= 160,
         [_, action_word, relation_word, landmark_word] <- Regex.run(@question, sentence),
         {:ok, action} <- Map.fetch(@actions, String.downcase(action_word)),
         {:ok, relation} <- Map.fetch(@relations, String.downcase(relation_word)),
         {:ok, landmark} <- Map.fetch(@landmarks, String.downcase(landmark_word)),
         {:ok, pair} <- pair(session, %{action: action, relation: relation,
           landmark: landmark}) do
      %{
        version: 1,
        kind: :reported_event_order_question,
        status: :provisional,
        sentence: sentence,
        definition: nil,
        speech_act: :request_information,
        source: :revalidated_discourse_report_order,
        operation: :recall_earlier_report,
        temporal_evidence: pair,
        grounding: %{basis: :session_report_sequence,
          actual_event_chronology: :not_established,
          factual_authority?: false},
        unresolved: [:real_world_event_occurrence, :actor_identity,
          :actual_event_chronology, :exact_dictionary_senses],
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end

  def question(_), do: nil

  @doc "The saved question meaning must equal an independent ledger re-query."
  @spec verified(map()) :: {:ok, map()} | :not_applicable
  def verified(%{} = si) do
    saved = Map.get(si, :sentence_meaning)
    if is_map(saved) and saved.kind == :reported_event_order_question and
       saved.factual_authority? == false and saved == question(si) do
      {:ok, saved}
    else
      :not_applicable
    end
  end

  def verified(_), do: :not_applicable

  @doc """
  A single earlier and single later report with matching provisional
  descriptive antecedent. Extra matches remain ambiguous, not guessed.
  """
  @spec pair(term(), map()) :: {:ok, map()} | :not_found | :ambiguous
  def pair(session, %{action: action, relation: relation, landmark: landmark})
      when is_binary(session) and action in [:sit, :sleep] and
           relation in [:on, :in, :under, :beside, :above, :below] and
           landmark in [:fence, :tree, :table] do
    records = Ledger.entries(session)
    reports = Enum.filter(records, &qualified_report?/1)

    targets =
      Enum.filter(reports, &(&1.action == action and &1.relation == relation and
        &1.landmark == landmark))

    case targets do
      [target] ->
        earlier = Enum.filter(reports, &(&1.seq < target.seq and
          &1.antecedent == target.antecedent and
          &1.antecedent_head == target.antecedent_head))

        nominals = Enum.filter(records, &(&1.kind == :nominal_description and
          &1.status == :provisional and
          &1.surface == target.antecedent and
          &1.head == target.antecedent_head and
          &1.seq < target.seq and &1.factual_authority? == false))

        case {earlier, nominals} do
          {[previous], [nominal]} when nominal.seq < previous.seq ->
            {:ok, %{
              kind: :report_order_relation,
              status: :verified_from_ledger,
              order: :reported_before,
              earlier: project(previous),
              later: project(target),
              antecedent: nominal.surface,
              antecedent_proof_sha256: nominal.proof_sha256,
              same_actor_identity: :candidate_only,
              event_occurrence: :not_established,
              actual_event_order: :not_established,
              causal_relation: :not_established,
              relation_type: @event_type,
              scope: :same_session,
              factual_authority?: false
            }}

          {[], _} -> :not_found
          {_, []} -> :not_found
          _ -> :ambiguous
        end

      [] -> :not_found
      _ -> :ambiguous
    end
  end

  def pair(_, _), do: :not_found

  defp project(event) do
    Map.take(event, [:seq, :antecedent, :antecedent_head, :action, :relation,
      :landmark, :typed_join_proof_sha256, :event_existence,
      :referent_verified?, :event_occurrence_verified?, :factual_authority?])
  end

  defp qualified_report?(%{
         kind: :reported_spatial_description,
         status: :provisional,
         seq: seq,
         antecedent: antecedent,
         antecedent_head: head,
         action: action,
         relation: relation,
         landmark: landmark,
         typed_join_verified?: true,
         typed_join_type: @event_type,
         typed_join_proof_sha256: sha,
         event_existence: :not_asserted,
         verification: :conversation_context_only,
         provenance: :revalidated_cross_turn_composition,
         referent_verified?: false,
         event_occurrence_verified?: false,
         factual_authority?: false
       })
       when is_integer(seq) and seq > 0 and
            is_binary(antecedent) and is_binary(head) and
            is_binary(sha) and byte_size(sha) == 64 and
            action in [:sit, :sleep] and
            relation in [:on, :in, :under, :beside, :above, :below] and
            landmark in [:fence, :tree, :table], do: true

  defp qualified_report?(_), do: false
end
