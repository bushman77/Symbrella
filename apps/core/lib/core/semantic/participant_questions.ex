defmodule Core.Semantic.ParticipantQuestions do
  @moduledoc """
  Bounded question-to-role binding for a stored progressive spatial event.
  Question grammar selects a requested *slot*; event participants supply the
  answer. Never extract the answer from the original sentence or LLM output.
  """

  alias Core.Semantic.EpisodicEvents

  @positions ~w(on in under beside above below)

  @spec parse(String.t()) ::
          {:forward, String.t(), atom(), map()} |
          {:reverse, String.t(), String.t(), String.t()} | nil
  def parse(text) when is_binary(text) do
    t = String.trim(text)

    cond do
      m = Regex.named_captures(
            ~r/^what is (?<predicate>[\p{L}]+) (?<position>on|in|under|beside|above|below) (?<location>(?:the|a|an|my) [\p{L} '-]+)\?$/iu, t
          ) ->
        {:reverse, norm(m["predicate"]), norm(m["position"]), norm(m["location"])}

      m = Regex.named_captures(
            ~r/^what is (?<entity>(?:the|a|an|my) [\p{L} '-]+?) (?<predicate>[\p{L}]+) (?<position>on|in|under|beside|above|below)\?$/iu, t
          ) ->
        {:forward, EpisodicEvents.entity_key(m["entity"]), :location,
         %{predicate: norm(m["predicate"]), position: norm(m["position"])}}

      m = Regex.named_captures(
            ~r/^what (?:is|was) (?<entity>(?:the|a|an|my) [\p{L} '-]+?) doing\??$/iu, t
          ) ->
        {:forward, EpisodicEvents.entity_key(m["entity"]), :predicate, %{}}

      m = Regex.named_captures(
            ~r/^where is (?<entity>(?:the|a|an|my) [\p{L} '-]+?)\?$/iu, t
          ) ->
        {:forward, EpisodicEvents.entity_key(m["entity"]), :location, %{}}

      true ->
        nil
    end
  end

  def parse(_), do: nil

  @spec answer(map(), atom(), map()) :: String.t() | nil
  def answer(%{status: "asserted", relation: "spatial_state",
               participants: %{"actor" => actor, "predicate" => predicate,
                               "position" => position, "location" => location}},
             role, constraints)
      when is_binary(actor) and is_binary(predicate) and
           is_binary(position) and is_binary(location) and is_map(constraints) do
    if Enum.all?(constraints, fn {key, val} ->
         case key do
           :predicate -> predicate == val
           :position -> position == val
           _ -> false
         end
       end) and position in @positions do
      case role do
        :location -> sentence(position <> " " <> location)
        :predicate -> sentence(predicate)
        :actor -> sentence(actor)
        _ -> nil
      end
    end
  end

  def answer(_, _, _), do: nil

  defp sentence(text) do
    text = String.trim(text)
    if text == "" do
      nil
    else
      String.upcase(String.first(text)) <> String.slice(text, 1, String.length(text)) <> "."
    end
  end

  defp norm(text), do: text |> String.downcase() |> String.trim()
end
