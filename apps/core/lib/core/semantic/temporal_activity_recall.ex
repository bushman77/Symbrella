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
  @progressive_question ~r/\A\s*what\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  @contracted_question ~r/\A\s*what['’]s\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently))?\s*[?.]?\s*\z/iu
  @earlier_do_question ~r/\A\s*what\s+did\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+do\s+(?<time>earlier|before|previously|first|last)\s*[?.]?\s*\z/iu
  @where_question ~r/\A\s*where\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  @where_contracted ~r/\A\s*where['’]s\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently))?\s*[?.]?\s*\z/iu
  @activity_report_question ~r/\A\s*(?:tell\s+me|can\s+you\s+tell\s+me)\s+what\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  # In embedded questions English normally uses subject-auxiliary order:
  # "Tell me what the bird is doing", not "tell me what is the bird doing".
  @embedded_activity_question ~r/\A\s*(?:tell\s+me|can\s+you\s+tell\s+me)\s+what\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+(?<aux>is|was)\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  # Anaphoric "it" is never assigned to an individual from mere recency.
  # It can refer to one unique attested actor KIND in the retained session.
  # Mixed actor kinds require clarification, not a guessed antecedent.
  @pronoun_activity ~r/\A\s*what\s+(?<aux>is|was)\s+it\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  @pronoun_where ~r/\A\s*where\s+(?<aux>is|was)\s+it(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  @pronoun_contract_activity ~r/\A\s*what['’]s\s+it\s+doing(?:\s+(?:right\s+now|at\s+the\s+moment|now|currently))?\s*[?.]?\s*\z/iu
  @pronoun_contract_where ~r/\A\s*where['’]s\s+it(?:\s+(?:right\s+now|at\s+the\s+moment|now|currently))?\s*[?.]?\s*\z/iu
  @pronoun_embedded ~r/\A\s*(?:tell\s+me|can\s+you\s+tell\s+me)\s+what\s+it\s+(?<aux>is|was)\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously|first|last))?\s*[?.]?\s*\z/iu
  # Destination questions distinguish a directional property from
  # verified arrival. A matching reported destination is required.
  @direction_question ~r/\A\s*where\s+is\s+(?:(?:the|a|an)\s+)?(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+flying\s*[?.]?\s*\z/iu
  @arrived_did ~r/\A\s*did\s+(?:(?:the|a|an)\s+)?(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+(?:reach|arrive\s+at)\s+(?:the|a|an)\s+(?<landmark>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[?.]?\s*\z/iu
  @arrived_has ~r/\A\s*has\s+(?:(?:the|a|an)\s+)?(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+(?:reached|arrived\s+at)\s+(?:the|a|an)\s+(?<landmark>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[?.]?\s*\z/iu
  # Count only evidence-qualified event descriptions, never word mentions.
  @count_described ~r/\A\s*how\s+many\s+times\s+(?:have\s+i\s+described|did\s+i\s+describe)\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[?.]?\s*\z/iu
  @count_reports ~r/\A\s*how\s+many\s+(?:descriptions|reports)\s+of\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+(?:have\s+i\s+given|did\s+i\s+give)\s*[?.]?\s*\z/iu
  # Compare the descriptions themselves; a difference in attributed
  # predicates cannot establish a real behavioral transition or identity.
  @compare_different ~r/\A\s*did\s+i\s+describe\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing\s+something\s+different\s*[?.]?\s*\z/iu
  @compare_same ~r/\A\s*was\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s+doing\s+the\s+same\s+thing\s*[?.]?\s*\z/iu
  @compare_what ~r/\A\s*what\s+changed\s+in\s+my\s+descriptions\s+of\s+(?:the|a|an)\s+(?<actor>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[?.]?\s*\z/iu
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
    with {:ok, role, mode, subject, target} <- parse(text),
         true <- is_pid(Process.whereis(Core.Discourse.Ledger.Owner)),
         reports <- Ledger.entries(session),
         # Counts and ordinal retrieval require a complete retained session,
         # not merely a limited slice of a longer or truncated timeline.
         true <- complete_report_window?(reports),
         {:ok, actor} <- resolve_actor(subject, reports, role),
         # The latest unknown activity for this actor blocks stale recall.
         :not_applicable <- Ledger.pending_unresolved_activity(
           session, "What is the #{Atom.to_string(actor)} doing?"),
         qualified <- Enum.filter(reports, &qualified?(&1, actor)),
         {:ok, chosen} <- select(mode, qualified, actor, role),
         true <- target_matches?(chosen, role, target),
         {:ok, wording} <- realize(chosen, role, mode) do
      {:ok, wording, metadata(mode, chosen.seq, role) |> enrich_metadata(chosen, role)}
    else
      {:ambiguous, :referent, actors, slot} ->
        labels = Enum.map_join(actors, " or ", &("the " <> Atom.to_string(&1)))
        {:ok, "Do you mean #{labels}?", metadata(:ambiguous_referent, nil, slot)}

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
      match = Regex.named_captures(@pronoun_activity, text) ->
        parse_aux(Map.put(match, "actor", "it"), :activity)

      match = Regex.named_captures(@pronoun_where, text) ->
        parse_aux(Map.put(match, "actor", "it"), :location)

      Regex.match?(@pronoun_contract_activity, text) ->
        {:ok, :activity, :latest, :it, nil}

      Regex.match?(@pronoun_contract_where, text) ->
        {:ok, :location, :latest, :it, nil}

      match = Regex.named_captures(@pronoun_embedded, text) ->
        parse_aux(Map.put(match, "actor", "it"), :activity)

      match = Regex.named_captures(@direction_question, text) ->
        case actor_kind(match["actor"]) do
          {:ok, actor} -> {:ok, :direction, :latest, actor, nil}
          _ -> :not_applicable
        end

      match = Regex.named_captures(@arrived_did, text) ->
        parse_arrival(match)

      match = Regex.named_captures(@arrived_has, text) ->
        parse_arrival(match)

      match = Regex.named_captures(@count_described, text) ->
        parse_actor(match, :count, :count)

      match = Regex.named_captures(@count_reports, text) ->
        parse_actor(match, :count, :count)

      match = Regex.named_captures(@compare_different, text) ->
        parse_actor(match, :comparison, :compare)

      match = Regex.named_captures(@compare_same, text) ->
        parse_actor(match, :comparison, :compare)

      match = Regex.named_captures(@compare_what, text) ->
        parse_actor(match, :comparison, :compare)






      match = Regex.named_captures(@progressive_question, text) ->
        parse_aux(match, :activity)

      match = Regex.named_captures(@activity_report_question, text) ->
        parse_aux(match, :activity)

      match = Regex.named_captures(@embedded_activity_question, text) ->
        parse_aux(match, :activity)

      match = Regex.named_captures(@contracted_question, text) ->
        parse_actor(match, :activity, :latest)

      match = Regex.named_captures(@earlier_do_question, text) ->
        parse_do_order(match)

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
      {{:ok, actor}, "was", "first"} -> {:ok, role, :first, actor, nil}
      {{:ok, actor}, "was", "last"} -> {:ok, role, :latest, actor, nil}
      {{:ok, actor}, "was", nil} -> {:ok, role, :earlier, actor, nil}
      {{:ok, actor}, "was", earlier} when earlier in ["earlier", "before", "previously"] ->
        {:ok, role, :earlier, actor, nil}
      {{:ok, actor}, "is", latest}
          when latest in [nil, "now", "right now", "currently", "at the moment"] ->
        {:ok, role, :latest, actor, nil}
      _ -> :not_applicable
    end
  end

  defp parse_actor(%{"actor" => surface}, role, mode) do
    case actor_kind(surface) do
      {:ok, actor} -> {:ok, role, mode, actor, nil}
      _ -> :not_applicable
    end
  end

  defp parse_do_order(%{"actor" => subject, "time" => time}) do
    with {:ok, actor} <- actor_kind(subject) do
      mode =
        case String.downcase(time) do
          "first" -> :first
          "last" -> :latest
          _ -> :earlier
        end

      {:ok, :activity, mode, actor, nil}
    else
      _ -> :not_applicable
    end
  end

  defp parse_arrival(%{"actor" => subject, "landmark" => place}) do
    with {:ok, actor} <- actor_kind(subject),
         {:ok, %{symbol: landmark}} <- Lexical.landmark(String.downcase(place)) do
      {:ok, :arrival, :latest, actor, landmark}
    else
      _ -> :not_applicable
    end
  end

  defp actor_kind("it"), do: {:ok, :it}

  defp actor_kind(surface) do
    case Lexical.actor(String.downcase(surface)) do
      {:ok, %{symbol: kind}} when is_atom(kind) -> {:ok, kind}
      _ -> :unsupported
    end
  end

  defp resolve_actor(:it, reports, slot) do
    actor_kinds =
      reports
      |> Enum.flat_map(fn
        %{actor_kind: kind} = report when is_atom(kind) ->
          if qualified?(report, kind) and
               match?({:ok, %{symbol: ^kind}},
                 Lexical.actor(Atom.to_string(kind))), do: [kind], else: []
        _ -> []
      end)
      |> Enum.uniq()
      |> Enum.sort()

    case actor_kinds do
      [unique] -> {:ok, unique}
      [] -> :not_applicable
      kinds -> {:ambiguous, :referent, kinds, slot}
    end
  end

  defp resolve_actor(actor, _reports, _slot) when is_atom(actor) do
    case Lexical.actor(Atom.to_string(actor)) do
      {:ok, %{symbol: ^actor}} -> {:ok, actor}
      _ -> :not_applicable
    end
  end

  defp resolve_actor(_, _, _), do: :not_applicable

  defp normalize_time(nil), do: nil
  defp normalize_time(time) when is_binary(time) do
    case time |> String.trim() |> String.downcase() |> String.replace(~r/\s+/u, " ") do
      "" -> nil
      normalized -> normalized
    end
  end

  defp complete_report_window?([]), do: true
  defp complete_report_window?(reports) when is_list(reports) do
    length(reports) <= @max_records and
      case List.last(reports) do
        %{seq: 1} -> true
        _ -> false
      end
  end

  defp select(:count, [], actor, :count),
    do: {:ok, %{seq: nil, actor_kind: actor, report_count: 0}}
  defp select(:count, [latest | _] = qualified, _actor, :count),
    do: {:ok, Map.put(latest, :report_count, length(qualified))}

  defp select(:compare, [latest, earlier], _actor, :comparison),
    do: {:ok, %{seq: latest.seq, latest_report: latest, earlier_report: earlier}}
  defp select(:compare, [_latest, _earlier | _], actor, :comparison),
    do: {:ambiguous, :earlier, actor, :comparison}

  defp select(:first, [_ | _] = qualified, _actor, _slot),
    do: {:ok, List.last(qualified)}
  defp select(:latest, [latest | _], _actor, _slot), do: {:ok, latest}
  defp select(:earlier, [_latest, previous], _actor, _slot), do: {:ok, previous}
  defp select(:earlier, [_latest, _previous | _], actor, slot),
    do: {:ambiguous, :earlier, actor, slot}
  defp select(_, _, _, _), do: :not_applicable

  defp target_matches?(event, :arrival, target)
       when is_atom(target) do
    event.reading == :directional and event.action == :fly and
      event.relation == :into and event.landmark == target
  end
  defp target_matches?(event, :direction, nil) do
    event.reading == :directional and event.action == :fly and
      event.relation == :into
  end
  defp target_matches?(_event, _role, nil), do: true
  defp target_matches?(_, _, _), do: false

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
  defp realize(event, :activity, :first) do
    with {:ok, description} <- describe(event) do
      {:ok, "Your first interpretable report described #{description}. This is report order, not verified event chronology."}
    end
  end

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
        case mode do
          :latest ->
            "The latest interpretable description places a #{actor} #{prep} a #{place}. Its actual current location is not verified."

          :first ->
            "Your first interpretable description places a #{actor} #{prep} a #{place}. This does not establish actual event chronology."

          _ ->
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

      label =
        case mode do
          :latest -> "latest interpretable"
          :first -> "first interpretable"
          _ -> "earlier"
        end
      {:ok, "The #{label} description has #{description}. " <> qualifier}
    end
  end

  defp realize(%{reading: :directional} = event, :direction, :latest) do
    with {:ok, description} <- describe(event) do
      {:ok, "You described #{description}. This expresses direction, not a verified destination or arrival."}
    end
  end

  defp realize(%{reading: :directional} = event, :arrival, :latest) do
    with {:ok, description} <- describe(event) do
      {:ok, "You described #{description}, but the description does not establish that it arrived."}
    end
  end

  defp realize(%{report_count: count, actor_kind: actor}, :count, :count)
       when is_integer(count) and count >= 0 and is_atom(actor) do
    noun = if count == 1, do: "description", else: "descriptions"

    {:ok, "I have #{count} interpretable #{noun} of a #{Atom.to_string(actor)} in this session. They do not establish event occurrence or that multiple reports describe the same individual."}
  end

  defp realize(%{earlier_report: old, latest_report: new},
               :comparison, :compare) do
    with {:ok, before} <- describe(old),
         {:ok, after_description} <- describe(new) do
      relation =
        if old.action == new.action,
          do: "The reported activity is the same in both descriptions.",
          else: "The reported activities differ."

      {:ok, "You first described #{before}, then #{after_description}. " <>
        relation <> " This does not establish that the same individual changed behavior or that either event occurred."}
    end
  end

  defp realize(_, _, _), do: :not_applicable

  defp enrich_metadata(meta, %{report_count: 0}, :count),
    do: Map.put(meta, :retrieval_found?, false)

  defp enrich_metadata(meta, %{earlier_report: %{seq: seq}}, :comparison),
    do: Map.put(meta, :earlier_report_seq, seq)

  defp enrich_metadata(meta, _, _), do: meta

  defp metadata(role, seq, requested_slot) do
    %{
      action: :conversation_recall,
      response_source: :symbolic_conversation,
      memory_source: :same_session_progressive_report,
      provenance: :revalidated_report_sequence,
      requested_temporal_role: role,
      requested_slot: requested_slot,
      report_seq: seq,
      retrieval_found?: role not in [:ambiguous_earlier, :ambiguous_referent],
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
