defmodule Core.Response.SpatialDialogue do
  @moduledoc """
  Bounded, session-local attribution of user-reported object placements.

  This is conversational evidence, NOT independent verification of a
  physical location. Only unambiguous first-person placement statements
  may update a discourse object. Assistant answers never establish state.
  If no matching user statement exists, return nil and defer to Core.
  """

  alias Core.Response.LlmChatHistory

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

      states =
        Enum.reduce(turns, %{objects: %{}, last: nil}, fn turn, state ->
          case placement(turn) do
            %{object: :it} = event ->
              case state.last do
                nil -> state
                object -> insert(state, %{event | object: object})
              end

            %{object: _} = event ->
              insert(state, event)

            nil ->
              state
          end
        end)

      object =
        case target do
          :it -> states.last
          name -> normalize_noun(name)
        end

      case Map.get(states.objects, head_noun(object)) do
        %{object: named, position: position, location: place} ->
          if target == :it or matches_name?(target, named) do
            {"According to what you told me, the #{named} is #{position} #{place}.",
             %{action: :conversation_recall,
               source: :user_reported_spatial_event,
               response_source: :symbolic_conversation,
               memory_source: :same_session_user_statement,
               retrieval_found?: true}}
          end

        _ ->
          nil
      end
    else
      _ -> nil
    end
  end

  def reply(_, _), do: nil

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
