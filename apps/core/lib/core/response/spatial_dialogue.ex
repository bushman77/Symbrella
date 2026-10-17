defmodule Core.Response.SpatialDialogue do
  @moduledoc """
  Attribution of user-reported object placements from the active session
  and owner-scoped, durable hippocampal observation episodes.

  This is conversational evidence, NOT independent verification of a
  physical location. Only unambiguous first-person placement statements
  may update a discourse object. Assistant answers never establish state.
  No unscoped general chat transcript is promoted into user memory.
  If no matching statement or owner-scoped episode exists, defer to Core.
  """

  alias Core.Response.LlmChatHistory

  @compile {:no_warn_undefined, Brain.Hippocampus}

  @turn_pairs 32
  @type answer :: {String.t(), map()} | nil

  @spec reply(String.t(), term()) :: answer()
  def reply(text, session_id) when is_binary(text) do
    with {:ok, target} <- location_question(text) do
      turns =
        session_id
        |> LlmChatHistory.messages(@turn_pairs)
        |> Enum.filter(&(Map.get(&1, "role") == "user"))
        |> Enum.map(&Map.get(&1, "content", ""))
        |> Kernel.++([text])

      states = reduce_placements(turns)

      object =
        case target do
          :it -> states.last
          name -> normalize_noun(name)
        end

      local = Map.get(states.objects, head_noun(object))
      cond do
        is_map(local) and (target == :it or matches_name?(target, local.object)) ->
          reply_from(local, :same_session_user_statement)

        is_binary(object) and object != "" ->
          case recall_observation(head_noun(object)) do
            %{object: named} = recalled when is_binary(named) ->
              if matches_name?(object, named),
                do: reply_from(recalled, :hippocampal_user_observation),
                else: nil

            _ -> nil
          end

        true -> nil
      end
    else
      _ -> nil
    end
  end

  def reply(_, _), do: nil

  @doc """
  Encode a concrete placement stated by the user, even when this turn did
  not ask a question. The previous same-session user turns may resolve 'it'.
  Fail closed on absent objects, ambiguous pronouns, or unavailable storage.
  """
  @spec remember_user_turn(String.t(), term()) :: :ok | {:error, atom()}
  def remember_user_turn(text, session_id) when is_binary(text) do
    prior =
      session_id
      |> LlmChatHistory.messages(@turn_pairs)
      |> Enum.filter(&(Map.get(&1, "role") == "user"))
      |> Enum.map(&Map.get(&1, "content", ""))

    previous_state = reduce_placements(prior)

    resolved =
      case placement(text) do
        %{object: :it} = event when is_binary(previous_state.last) ->
          %{event | object: previous_state.last}
        %{object: object} = event when is_binary(object) ->
          event
        _ ->
          nil
      end

    case resolved do
      %{object: object, position: position, location: location} ->
        owner = memory_owner()
        key = head_noun(object)

        if owner != nil and key != "" and hippocampus_available?() do
          slate = %{
            sentence: text,
            tokens: [key, object, location],
            winners: [%{id: "#{key}|conversation_observation|0", lemma: key}]
          }
          meta = %{
            kind: :user_reported_spatial,
            subject: :user,
            provenance: :user_statement,
            owner: owner,
            session_id: to_string(session_id),
            object_key: key,
            object: object,
            position: position,
            location: location,
            tags: ["user_reported_spatial", "episodic"]
          }

          try do
            Brain.Hippocampus.encode_user_observation(slate, meta)
          rescue
            _ -> {:error, :memory_unavailable}
          catch
            :exit, _ -> {:error, :memory_unavailable}
          end
        else
          {:error, :memory_unavailable}
        end

      _ ->
        :ok
    end
  end

  def remember_user_turn(_, _), do: :ok

  defp reduce_placements(turns) do
    Enum.reduce(turns, %{objects: %{}, last: nil}, fn turn, state ->
      case placement(turn) do
        %{object: :it} = event when is_binary(state.last) ->
          insert(state, %{event | object: state.last})

        %{object: object} = event when is_binary(object) ->
          insert(state, event)

        _ ->
          state
      end
    end)
  end

  defp reply_from(%{object: named, position: position, location: place}, source) do
    {"According to what you told me, the #{named} is #{position} #{place}.",
     %{action: :conversation_recall,
       source: :user_reported_spatial_event,
       response_source: :symbolic_conversation,
       memory_source: source,
       retrieval_found?: true}}
  end

  defp recall_observation(key) do
    if memory_owner() != nil and hippocampus_available?() do
      try do
        Brain.Hippocampus.user_observation(memory_owner(), key)
      rescue
        _ -> nil
      catch
        :exit, _ -> nil
      end
    end
  end

  defp hippocampus_available? do
    Code.ensure_loaded?(Brain.Hippocampus) and
      is_pid(Process.whereis(Brain.Hippocampus)) and
      function_exported?(Brain.Hippocampus, :user_observation, 2) and
      function_exported?(Brain.Hippocampus, :encode_user_observation, 2)
  end

  # A local, single-user deployment has one stable owner; multi-user
  # deployments must configure a distinct value for each memory owner.
  # nil/false disables cross-session observation persistence.
  defp memory_owner do
    case Application.get_env(:symbrella, :memory_owner_id, "local-primary") do
      owner when is_binary(owner) and byte_size(owner) in 1..128 -> owner
      _ -> nil
    end
  end

  defp location_question(text) do
    case Regex.run(
           ~r/\bwhere\s+is\s+(?:(?:the|my|a)\s+)?([\p{L}]+(?:\s+(?!now\b|currently\b)[\p{L}]+){0,3})(?:\s+(?:now|currently))?\s*\?/iu,
           text
         ) do
      [_, "it"] -> {:ok, :it}
      [_, name] -> {:ok, normalize_noun(name)}
      _ -> :no_query
    end
  end

  defp placement(text) do
    # The direct correction tested in the live conversation.
    moved_out =
      Regex.named_captures(
        ~r/\bI\s+(?:took|removed|pulled)\s+(?<object>(?:the|my|a)\s+[\p{L}\s-]{2,70}?)\s+out\s+of\s+(?:the|my|a)\s+[\p{L}\s-]{2,70}?\s+and\s+(?:I\s+)?(?:placed|put|set)\s+it\s+(?<position>on|onto|in|into|under|beside)\s+(?<location>(?:a|the|my)\s+[\p{L}\s-]{2,70}?)(?=[.!?]|$)/iu,
        text
      )

    moved =
      Regex.named_captures(
        ~r/\bI\s+(?:moved|placed|put|set)\s+(?<object>it|(?:the|my|a)\s+[\p{L}\s-]{2,70}?)\s+(?<position>on|onto|in|into|under|beside)\s+(?<location>(?:a|the|my)\s+[\p{L}\s-]{2,70}?)(?=[.!?]|$)/iu,
        text
      )

    located =
      Regex.named_captures(
        ~r/\bI\s+have\s+(?<object>(?:a|the|my)\s+[\p{L}\s-]{2,70}?)\s+(?:sitting|resting)\s+(?<position>on|in|under)\s+(?<location>(?:a|the|my)\s+[\p{L}\s-]{2,70}?)(?=[.!?]|$)/iu,
        text
      )

    case moved_out || moved || located do
      %{"object" => object, "position" => position, "location" => location} ->
        object = normalize_noun(object)
        place = String.trim(location)
        if object != "" and String.length(place) in 3..70 do
          %{object: if(object == "it", do: :it, else: object),
            position: String.downcase(position), location: place}
        end

      _ -> nil
    end
  end

  defp insert(state, %{object: object} = event) when is_binary(object) do
    key = head_noun(object)
    if key == "" do
      state
    else
      %{state | objects: Map.put(state.objects, key, event), last: object}
    end
  end

  defp normalize_noun(name) when is_binary(name) do
    name
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\A(?:the|a|my)\s+/iu, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize_noun(_), do: ""

  defp head_noun(name) when is_binary(name) do
    name |> String.split(" ", trim: true) |> List.last() || ""
  end

  defp head_noun(_), do: ""

  defp matches_name?(target, named) do
    target = normalize_noun(target)
    named = normalize_noun(named)

    head_noun(target) == head_noun(named) and
      Enum.all?(String.split(target, " ", trim: true),
        &(&1 in String.split(named, " ", trim: true)))
  end
end
