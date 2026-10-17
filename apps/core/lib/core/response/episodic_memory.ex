defmodule Core.Response.EpisodicMemory do
  @moduledoc """
  Owner-scoped semantic event memory, independent of chat session.

  A user-provided assertion is a recollection, not external verification.
  Events use a generic entity/relation/value contract. Language parsing is a
  conservative adapter; other symbolic regions can call remember_event/2.
  """

  alias Core.Semantic.EpisodicEvents

  @compile {:no_warn_undefined, Brain.Hippocampus}
  @type result :: {String.t(), map()} | nil

  @spec remember_user_turn(String.t(), term()) :: :ok | {:error, atom()}
  def remember_user_turn(text, session_id) when is_binary(text) do
    text
    |> EpisodicEvents.extract()
    |> Enum.reduce(:ok, fn event, acc ->
      case remember_event(event, session_id, text) do
        :ok -> acc
        error -> error
      end
    end)
  end

  def remember_user_turn(_, _), do: :ok

  @doc "Accept any normalized typed event from a user-grounded symbolic parser."
  @spec remember_event(map(), term(), String.t()) :: :ok | {:error, atom()}
  def remember_event(event, session_id, sentence \\ "") do
    with {:ok, event} <- normalize_event(event),
         owner when is_binary(owner) <- memory_owner(),
         true <- hippocampus_available?() do
      slate = %{
        sentence: sentence,
        tokens: Enum.uniq([event.entity_key, event.relation, event.value]),
        winners: [%{id: "#{event.entity_key}|event|0", lemma: event.entity_key}]
      }

      metadata = %{
        kind: :user_reported_event, event_version: 1, subject: :user,
        provenance: :user_statement, owner: owner,
        session_id: to_string(session_id), entity: event.entity,
        entity_key: event.entity_key, relation: event.relation,
        value: event.value, status: Atom.to_string(event.status),
        tags: ["episodic", "user_reported_event"]
      }

      try do
        Brain.Hippocampus.encode_user_event(slate, metadata)
      rescue
        _ -> {:error, :memory_unavailable}
      catch
        :exit, _ -> {:error, :memory_unavailable}
      end
    else
      :error -> {:error, :invalid_event}
      _ -> {:error, :memory_unavailable}
    end
  end

  @spec reply(String.t()) :: result()
  def reply(text) when is_binary(text) do
    with owner when is_binary(owner) <- memory_owner(),
         true <- hippocampus_available?() do
      case EpisodicEvents.question(text) do
        {:relation, key, relation} when key != "" ->
          case Brain.Hippocampus.user_event(owner, key, relation) do
            %{status: "asserted"} = event -> respond_event(event)
            %{status: "retracted"} = event ->
              {"You withdrew what you previously told me about #{event.entity} (#{relation}). I don't have a current value for it.",
               metadata(:hippocampal_event, event)}
            _ -> nil
          end

        {:history, key} when key != "" ->
          owner
          |> Brain.Hippocampus.user_event_history(key, 12)
          |> respond_history()

        _ -> nil
      end
    else
      _ -> nil
    end
  end

  def reply(_), do: nil

  @doc "Get the most recent event for an entity/relation, including retractions."
  @spec recall(String.t(), String.t()) :: map() | nil
  def recall(entity, relation) when is_binary(entity) and is_binary(relation) do
    if memory_owner() && hippocampus_available?() do
      Brain.Hippocampus.user_event(memory_owner(), EpisodicEvents.entity_key(entity), relation)
    end
  end

  def recall(_, _), do: nil

  defp respond_event(%{entity: entity, relation: relation, value: value} = event) do
    sentence =
      case relation do
        "deadline" ->
          subject = if String.starts_with?(String.downcase(entity), "the "), do: entity, else: "the " <> entity
          "According to what you told me, #{subject} deadline is #{value}."
        "lives_in" -> "You told me #{entity} lives in #{value}."
        "met_at" -> "You told me you met #{entity} at #{value}."
        "named" -> "You told me you named #{entity} #{value}."
        "scheduled_for" -> "You told me #{entity} is scheduled for #{value}."
        "likes" -> "You told me #{entity} likes #{value}."
        "prefers" -> "You told me you prefer #{value}."
        "located_at" -> "You told me #{entity} is located #{value}."
        "relationship" -> "You told me #{entity} is #{value}."
        _ -> "You told me: #{entity} — #{relation} — #{value}."
      end

    {sentence, metadata(:hippocampal_event, event)}
  end

  defp respond_history([]), do: nil

  defp respond_history(events) do
    # The newest event for a relation is the current state, including a
    # retraction. Historical superseded claims are accessible in the DB but
    # must not be presented as simultaneously current.
    latest =
      Enum.uniq_by(events, & &1.relation)
      |> Enum.take(6)

    entity = hd(latest).entity
    details =
      Enum.map_join(latest, "; ", fn event ->
        if event.status == "retracted" do
          "#{event.relation}: withdrawn"
        else
          "#{event.relation}: #{event.value}"
        end
      end)

    {"From what you've told me about #{entity}: #{details}.",
     metadata(:hippocampal_event_history, hd(events))}
  end

  defp metadata(source, event) do
    %{action: :conversation_recall,
      memory_source: source,
      response_source: :symbolic_conversation,
      source: :user_reported_event,
      provenance: :user_reported,
      memory_event_id: event.id,
      retrieval_found?: true}
  end

  defp normalize_event(event), do: EpisodicEvents.normalize(event)

  defp hippocampus_available? do
    Code.ensure_loaded?(Brain.Hippocampus) and
      is_pid(Process.whereis(Brain.Hippocampus)) and
      function_exported?(Brain.Hippocampus, :encode_user_event, 2) and
      function_exported?(Brain.Hippocampus, :user_event, 3)
  end

  # Mirrors SpatialDialogue owner scoping; production multi-user systems must
  # explicitly set a unique stable owner ID for each authenticated user.
  defp memory_owner do
    case Application.get_env(:symbrella, :memory_owner_id, "local-primary") do
      id when is_binary(id) and byte_size(id) in 1..128 -> id
      _ -> nil
    end
  end
end
