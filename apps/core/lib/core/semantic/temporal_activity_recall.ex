defmodule Core.Semantic.TemporalActivityRecall do
  @moduledoc """
  Read-only, same-session retrieval of *report order* for independently proved
  progressive event descriptions.

  "Was doing" selects the single earlier report when exactly two qualified
  actor-kind reports exist; "doing now" selects the latest qualified report.
  Neither form proves real event chronology, present activity, arrival, or that
  repeated descriptions of an actor kind identify the same individual.

  Older one-event activity questions continue through the existing Hippocampal
  participant reader. No LLM, transcript guessing, or durable memory write.
  """

  alias Core.Discourse.Ledger

  # Bounded grammatical paraphrases. These select report roles, not facts.
  @progressive_question ~r/\A\s*what\s+(?<aux>is|was)\s+(?:the|a|an)\s+(?<actor>bird|cat|dog)\s+doing(?:\s+(?<time>right\s+now|at\s+the\s+moment|now|currently|earlier|before|previously))?\s*\??\s*\z/iu
  @contracted_question ~r/\A\s*what['’]s\s+(?:the|a|an)\s+(?<actor>bird|cat|dog)\s+doing\s+(?<time>right\s+now|at\s+the\s+moment|now|currently)\s*\??\s*\z/iu
  @earlier_do_question ~r/\A\s*what\s+did\s+(?:the|a|an)\s+(?<actor>bird|cat|dog)\s+do\s+(?:earlier|before|previously)\s*\??\s*\z/iu
  @actors %{"bird" => :bird, "cat" => :cat, "dog" => :dog}
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
    with {:ok, mode, actor} <- parse(text),
         # The latest unknown activity for this actor blocks stale recall.
         :not_applicable <- Ledger.pending_unresolved_activity(
           session, "What is the #{Atom.to_string(actor)} doing?"),
         reports <- Ledger.entries(session),
         # Do not mistake a truncated search window for a complete timeline.
         true <- length(reports) <= @max_records,
         qualified <- Enum.filter(reports, &qualified?(&1, actor)),
         {:ok, chosen} <- select(mode, qualified, actor),
         {:ok, description} <- describe(chosen) do
      wording =
        case mode do
          :earlier ->
            "Earlier, you described #{description}. This is report order, not verified event chronology."

          :latest ->
            "You most recently described #{description}. This does not verify current activity, arrival, or that it is the same individual."
        end

      {:ok, wording, metadata(mode, chosen.seq)}
    else
      {:ambiguous, :earlier, actor} ->
        {:ok,
         "You gave several earlier descriptions of a #{Atom.to_string(actor)}. Which earlier description do you mean?",
         metadata(:ambiguous_earlier, nil)}

      _ ->
        :not_applicable
    end
  end

  def reply(_, _), do: :not_applicable

  defp parse(text) do
    cond do
      match = Regex.named_captures(@progressive_question, text) ->
        actor = actor_kind(match["actor"])
        time = normalize_time(match["time"])

        case {String.downcase(match["aux"]), time} do
          {"was", nil} -> {:ok, :earlier, actor}
          {"was", earlier} when earlier in ["earlier", "before", "previously"] ->
            {:ok, :earlier, actor}
          {"is", latest} when latest in ["now", "right now", "currently", "at the moment"] ->
            {:ok, :latest, actor}
          _ -> :not_applicable
        end

      match = Regex.named_captures(@contracted_question, text) ->
        {:ok, :latest, actor_kind(match["actor"])}

      match = Regex.named_captures(@earlier_do_question, text) ->
        {:ok, :earlier, actor_kind(match["actor"])}

      true ->
        :not_applicable
    end
  end

  defp actor_kind(surface), do: Map.fetch!(@actors, String.downcase(surface))

  defp normalize_time(nil), do: nil
  defp normalize_time(time),
    do: time |> String.downcase() |> String.replace(~r/\s+/u, " ")

  defp select(:latest, [latest | _], _actor), do: {:ok, latest}
  defp select(:earlier, [_latest, previous], _actor), do: {:ok, previous}
  defp select(:earlier, [_latest, _previous | _], actor),
    do: {:ambiguous, :earlier, actor}
  defp select(_, _, _), do: :not_applicable

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
    with true <- actor in Map.values(@actors),
         {:ok, activity} <- Map.fetch(@actions, action),
         {:ok, prep} <- Map.fetch(@relations, relation),
         {:ok, place} <- Map.fetch(@landmarks, landmark) do
      {:ok, "a #{Atom.to_string(actor)} #{activity} #{prep} a #{place}"}
    else
      _ -> :not_applicable
    end
  end

  defp metadata(role, seq) do
    %{
      action: :conversation_recall,
      response_source: :symbolic_conversation,
      memory_source: :same_session_progressive_report,
      provenance: :revalidated_report_sequence,
      requested_temporal_role: role,
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
