defmodule Core.Semantic.TemporalActivityRecall do
  @moduledoc """
  Read-only, same-session retrieval of *report order* for independently proved
  progressive event descriptions.

  Past questions select the single earlier report when exactly two qualified
  actor-kind reports exist; present questions select the latest qualified
  report. Both activity and location questions are resolved by requested role.
  Neither form proves real event chronology, present activity, arrival, or that
  repeated descriptions of an actor kind identify the same individual.

  Older one-event activity questions continue through the existing Hippocampal
  participant reader. No LLM, transcript guessing, or durable memory write.
  """

  alias Core.Discourse.Ledger
  alias Core.Semantic.Composition.Lexical

  # Bounded grammatical paraphrases. These select report roles, not facts.
  @progressive_question ~r/\A\s*what\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously))?\s*[?.]?\s*\z/iu
  @contracted_question ~r/\A\s*what['’]s\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently))?\s*[?.]?\s*\z/iu
  @earlier_do_question ~r/\A\s*what\s+did\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+do\s+(?:earlier|before|previously)\s*[?.]?\s*\z/iu
  @where_question ~r/\A\s*where\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously))?\s*[?.]?\s*\z/iu
  @where_contracted ~r/\A\s*where['’]s\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently))?\s*[?.]?\s*\z/iu
  @activity_report_question ~r/\A\s*(?:tell\s+me|can\s+you\s+tell\s+me)\s+what\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously))?\s*[?.]?\s*\z/iu
  @actions %{sit: "sitting", sleep: "sleeping", fly: "flying"}
  @relations %{on: "on", in: "in", under: "under", beside: "beside",
               above: "above", below: "below", into: "into"}
  @landmarks %{fence: "fence", tree: "tree", table: "table"}
  @static_relations [:on, :in, :under, :beside, :above, :below]
  @max_records 128

  @spec reply(term(), String.t()) :: {:ok, String.t(), map()} | :not_applicable
  def reply(session, text)
      when is_binary(session) and byte_size(session) in 1..128 and
           is_binary(text) and byte_size(text) <= 160 do
    with {:ok, role, mode, actor} <- parse(text),
         # The latest unknown activity for this actor blocks stale recall.
         :not_applicable <- Ledger.pending_unresolved_activity(
           session, "What is the #{Atom.to_string(actor)} doing?"),
         reports <- Ledger.entries(session),
         # Do not mistake a truncated search window for a complete timeline.
         true <- length(reports) <= @max_records,
         qualified <- Enum.filter(reports, &qualified?(&1, actor)),
         {:ok, chosen} <- select(mode, qualified, actor, role),
         {:ok, wording} <- realize(chosen, role, mode) do
      {:ok, wording, metadata(mode, chosen.seq, role)}
    else
      {:ambiguous, :earlier, actor, requested_slot} ->
        {:ok,
         "You gave several earlier descriptions of a #{Atom.to_string(actor)}. Which earlier description do you mean?",
         metadata(:ambiguous_earlier, nil, requested_slot)}

      _ ->
        :not_applicable
    end
  end

  def reply(_, _), do: :not_applicable

  defp parse(text) do
    cond do
      match = Regex.named_captures(@progressive_question, text) ->
        parse_aux(match, :activity)

      match = Regex.named_captures(@activity_report_question, text) ->
        parse_aux(match, :activity)

      match = Regex.named_captures(@contracted_question, text) ->
        parse_actor(match, :activity, :latest)

      match = Regex.named_captures(@earlier_do_question, text) ->
        parse_actor(match, :activity, :earlier)

      match = Regex.named_captures(@where_question, text) ->
        parse_aux(match, :location)

      match = Regex.named_captures(@where_contracted, text) ->
        parse_actor(match, :location, :latest)

      true ->
        :not_applicable
    end
  end

  defp parse_aux(%{"aux" => aux, "actor" => surface, "time" => raw}, role) do
    time = normalize_time(raw)

    case {actor_kind(surface), String.downcase(aux), time} do
      {{:ok, actor}, "was", nil} -> {:ok, role, :earlier, actor}
      {{:ok, actor}, "was", earlier} when earlier in ["earlier", "before", "previously"] ->
        {:ok, role, :earlier, actor}
      {{:ok, actor}, "is", latest}
          when latest in [nil, "now", "right now", "currently", "at the moment"] ->
        {:ok, role, :latest, actor}
      _ -> :not_applicable
    end
  end

  defp parse_actor(%{"actor" => surface}, role, mode) do
    case actor_kind(surface) do
      {:ok, actor} -> {:ok, role, mode, actor}
      _ -> :not_applicable
    end
  end

  defp actor_kind(surface) do
    case Lexical.actor(String.downcase(surface)) do
      {:ok, %{symbol: kind}} when is_atom(kind) -> {:ok, kind}
      _ -> :unsupported
    end
  end

  defp normalize_time(nil), do: nil
  defp normalize_time(time) when is_binary(time) do
    case time |> String.trim() |> String.downcase() |> String.replace(~r/\s+/u, " ") do
      "" -> nil
      normalized -> normalized
    end
  end

  defp select(:latest, [latest | _], _actor, _slot), do: {:ok, latest}
  defp select(:earlier, [_latest, previous], _actor, _slot), do: {:ok, previous}
  defp select(:earlier, [_latest, _previous | _], actor, slot),
    do: {:ambiguous, :earlier, actor, slot}
  defp select(_, _, _, _), do: :not_applicable

  # The ledger is append-only and server-owned. Still validate the complete
  # compact witness before using it for a temporal answer.
  defp qualified?(%{
         kind: :reported_progressive_event, status: :provisional,
         seq: seq, actor_kind: actor, action: action,
         relation: relation, landmark: landmark, reading: reading,
         arrival: :not_established, typed_proof_sha256: proof,
         event_existence: :not_asserted,
         event_occurrence_verified?: false, referent_verified?: false,
         provenance: :revalidated_event_composition,
         verification: :conversation_context_only,
         factual_authority?: false
       }, requested_actor)
       when is_integer(seq) and seq > 0 and actor == requested_actor and
            is_binary(proof) and byte_size(proof) == 64 do
    Regex.match?(~r/\A[0-9a-f]{64}\z/, proof) and
      valid_property?(action, relation, landmark, reading)
  end

  defp qualified?(_, _), do: false

  defp valid_property?(action, relation, landmark, reading) do
    Map.has_key?(@actions, action) and Map.has_key?(@relations, relation) and
      Map.has_key?(@landmarks, landmark) and
      case {action, relation, reading} do
        {:fly, :into, :directional} -> true
        {verb, spatial, :locative} when verb in [:sit, :sleep] ->
          spatial in @static_relations
        _ -> false
      end
  end

  defp describe(%{actor_kind: actor, action: action, relation: relation,
                  landmark: landmark}) do
    with true <- is_atom(actor),
         {:ok, activity} <- Map.fetch(@actions, action),
         {:ok, prep} <- Map.fetch(@relations, relation),
         {:ok, place} <- Map.fetch(@landmarks, landmark) do
      {:ok, "a #{Atom.to_string(actor)} #{activity} #{prep} a #{place}"}
    else
      _ -> :not_applicable
    end
  end

  # Answers describe what the speaker said, never that the reported action
  # occurred. In particular, "into" is a direction, not a confirmed location.
  defp realize(event, :activity, :earlier) do
    with {:ok, description} <- describe(event) do
      {:ok, "Earlier, you described #{description}. This is report order, not verified event chronology."}
    end
  end

  defp realize(event, :activity, :latest) do
    with {:ok, description} <- describe(event) do
      {:ok, "Among the reports I can interpret, the latest describes #{description}. This does not verify current activity, arrival, or that it is the same individual."}
    end
  end

  defp realize(%{reading: :locative} = event, :location, mode) do
    with {:ok, prep} <- Map.fetch(@relations, event.relation),
         {:ok, place} <- Map.fetch(@landmarks, event.landmark),
         true <- event.relation in @static_relations do
      actor = Atom.to_string(event.actor_kind)

      message =
        if mode == :latest do
          "The latest interpretable description places a #{actor} #{prep} a #{place}. Its actual current location is not verified."
        else
          "An earlier description places a #{actor} #{prep} a #{place}. This does not establish actual event chronology."
        end

      {:ok, message}
    else
      _ -> :not_applicable
    end
  end

  defp realize(%{reading: :directional} = event, :location, mode) do
    with {:ok, description} <- describe(event) do
      qualifier =
        if mode == :latest,
          do: "Arrival and current location are not established.",
          else: "Arrival and historical location are not established."

      label = if mode == :latest, do: "latest interpretable", else: "earlier"
      {:ok, "The #{label} description has #{description}. " <> qualifier}
    end
  end

  defp realize(_, _, _), do: :not_applicable

  defp metadata(role, seq, requested_slot) do
    %{
      action: :conversation_recall,
      response_source: :symbolic_conversation,
      memory_source: :same_session_progressive_report,
      provenance: :revalidated_report_sequence,
      requested_temporal_role: role,
      requested_slot: requested_slot,
      report_seq: seq,
      retrieval_found?: role != :ambiguous_earlier,
      scope: :same_session,
      semantic_verified?: false,
      factual_authority?: false,
      event_occurrence_verified?: false,
      actual_event_chronology_verified?: false,
      same_actor_identity_verified?: false,
      llm_called?: false
    }
  end
end
