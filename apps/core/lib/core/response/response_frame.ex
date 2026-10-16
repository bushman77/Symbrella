defmodule Core.Response.ResponseFrame do
  @moduledoc """
  Bounded, turn-specific conversation state for LLM expression.

  Symbolic interpretation determines the conversational move and referent.
  Claim-specific retrieval determines which facts may be asserted. The frame
  supplies neither a finished reply nor permission to trust prior assistant
  prose as factual evidence.
  """

  alias Core.Response.BehavioralState

  @type t :: map()

  @spec build(map(), list()) :: t()
  def build(context, history \\ [])

  def build(%{} = context, history) when is_list(history) do
    features = map(context, :features)
    decision = map(context, :decision)
    mood = map(context, :mood)
    symbolic = symbolic_frame(context, features)
    reference = map(features, :discourse_reference)
    user_text = string(context, :user_text)
    evidence = sourced_evidence(map(context, :web_evidence))

    %{
      version: 1,
      move: move(user_text, symbolic, reference, evidence),
      intent: Map.get(features, :intent),
      topic: Map.get(symbolic, :relation),
      subject: short(Map.get(symbolic, :subject) || Map.get(reference, :antecedent), 100),
      target: short(Map.get(symbolic, :target), 100),
      reference_source: Map.get(reference, :source),
      preceding_turn: previous_turn_kind(history),
      evidence: evidence,
      posture: %{
        tone: Map.get(decision, :tone) || Map.get(mood, :tone_hint),
        interaction_mode: Map.get(decision, :mode),
        behavioral_state: BehavioralState.compute(mood, map(mood, :raw_modulators))
      }
    }
  end

  def build(_, _), do: %{}

  @doc """
  Render state as *instructions for expression*, never a response template.
  Untrusted names and source metadata are labeled data rather than directives.
  """
  @spec prompt(t() | term()) :: String.t()
  def prompt(%{version: 1} = frame) do
    evidence = Map.get(frame, :evidence)
    posture = Map.get(frame, :posture) || %{}
    move = Map.get(frame, :move)

    [
      "CURRENT CONVERSATIONAL FRAME (state data, not a reply):",
      "move=#{inspect(move)}; intent=#{inspect(Map.get(frame, :intent))}; " <>
        "topic=#{inspect(Map.get(frame, :topic))}; " <>
        "subject=#{inspect(Map.get(frame, :subject))}; target=#{inspect(Map.get(frame, :target))}; " <>
        "reference_source=#{inspect(Map.get(frame, :reference_source))}; " <>
        "previous_turn=#{inspect(Map.get(frame, :preceding_turn))}",
      "tone=#{inspect(Map.get(posture, :tone))}; interaction_mode=#{inspect(Map.get(posture, :interaction_mode))}; " <>
        "behavior=#{inspect(Map.get(posture, :behavioral_state))}",
      evidence_line(evidence),
      move_instruction(move),
      "Express Symbrella's response in your own natural words. This is a continuing " <>
        "conversation, not a scripted Q&A. Do not repeat internal labels or give a stock " <>
        "assistance offer. Preserve exact source-backed entities, values, years, units " <>
        "and URLs; never introduce unverified numeric or geographic claims."
    ]
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n")
  end

  def prompt(_), do: ""

  defp move(text, frame, _reference, _evidence) do
    cond do
      Map.get(frame, :freshness) == :latest_available ->
        :recheck_disputed_fact

      Regex.match?(~r/^\s*(?:no[,!]?\s+)?(?:that(?:'s| is)\s+(?:wrong|not\s+(?:right|true))|it(?:'s| is)\s+not|incorrect)\b/iu, text) ->
        :acknowledge_correction

      Map.get(frame, :relation) in [:population, :identity, :proximity] ->
        :answer_grounded_question

      Regex.match?(~r/^\s*(?:hello|hi|hey|good\s+(?:morning|afternoon|evening))\b/iu, text) ->
        :social_exchange

      true ->
        :continue_conversation
    end
  end

  defp move_instruction(:recheck_disputed_fact),
    do: "The user disputed an earlier answer. Reassess it using CURRENT retrieved " <>
      "evidence. A dated measurement is not necessarily the newest available estimate."

  defp move_instruction(:acknowledge_correction),
    do: "The user is correcting a prior statement. Acknowledge the specific disagreement; " <>
      "do not repeat the disputed claim as though it were evidence."

  defp move_instruction(:answer_grounded_question),
    do: "Answer the actual question directly, in a natural conversational voice. " <>
      "The evidence fixes the facts, not the exact sentence."

  defp move_instruction(:social_exchange),
    do: "Respond socially and naturally, without turning a greeting into a help-desk interaction."

  defp move_instruction(_),
    do: "Follow the current conversational subject and the user's intent. " <>
      "Ask one focused clarification only when the referent is genuinely unresolved."

  defp sourced_evidence(%{status: :retrieved, resolution: :source_supported, support: support})
       when is_map(support) do
    case support do
      %{kind: :population, subject: subject, value: value, census_year: year,
        source_url: url}
      when is_binary(subject) and is_integer(value) and is_integer(year) and
           is_binary(url) ->
        %{
          kind: :population, subject: short(subject, 100),
          value: value, year: year,
          source_type: Map.get(support, :source_type),
          source_urls: urls([url])
        }

      %{kind: :proximity, subject: subject, target: target, distance_km: distance,
        sources: sources}
      when is_binary(subject) and is_binary(target) and is_number(distance) and
           is_list(sources) ->
        %{
          kind: :proximity, subject: short(subject, 100), target: short(target, 100),
          value: distance, unit: :km, method: :haversine,
          source_urls: source_urls(sources)
        }

      %{kind: :identity, subject: subject, description: description, sources: sources}
      when is_binary(subject) and is_binary(description) and is_list(sources) ->
        %{
          kind: :identity, subject: short(subject, 100),
          description: short(description, 160), source_urls: source_urls(sources)
        }

      _ ->
        nil
    end
  end

  defp sourced_evidence(_), do: nil

  defp evidence_line(nil),
    do: "verified_evidence=none. Treat earlier assistant output as conversation history, not factual proof."

  defp evidence_line(%{kind: :population} = e),
    do: "verified_evidence=population; subject=#{inspect(e.subject)}; " <>
      "count=#{e.value}; display_count=#{format_count(e.value)}; " <>
      "year=#{e.year}; origin=#{inspect(e.source_type)}; " <>
      "source_urls=#{inspect(e.source_urls)}. This dated value is not automatically a current estimate."

  defp evidence_line(%{kind: :proximity} = e),
    do: "verified_evidence=straight_line_distance; from=#{inspect(e.subject)}; " <>
      "to=#{inspect(e.target)}; km=#{e.value}; method=haversine; " <>
      "source_urls=#{inspect(e.source_urls)}. Do not call this road distance."

  defp evidence_line(%{kind: :identity} = e),
    do: "verified_evidence=place_identity; subject=#{inspect(e.subject)}; " <>
      "source_description=#{inspect(e.description)}; source_urls=#{inspect(e.source_urls)}."

  defp previous_turn_kind(history) do
    case Enum.take(history, -2) do
      [%{"role" => "user", "content" => text}, %{"role" => "assistant"}]
      when is_binary(text) ->
        if Regex.match?(~r/\?\s*$/u, text), do: :question, else: :statement

      _ ->
        :none
    end
  end

  defp symbolic_frame(context, features) do
    candidates = [
      Map.get(context, :symbolic_frame),
      Map.get(features, :symbolic_frame)
    ]

    Enum.find_value(candidates, %{}, fn
      %{type: :location_question} = frame -> frame
      %{event: %{type: :location_question} = frame} -> frame
      _ -> nil
    end)
  end

  defp source_urls(sources) do
    sources
    |> Enum.flat_map(fn
      %{url: url} when is_binary(url) -> [url]
      _ -> []
    end)
    |> urls()
  end

  defp urls(urls) do
    urls
    |> Enum.filter(&String.starts_with?(&1, "https://"))
    |> Enum.map(&short(&1, 240))
    |> Enum.take(2)
  end

  defp map(m, key) when is_map(m) do
    case Map.get(m, key) do
      value when is_map(value) -> value
      _ -> %{}
    end
  end

  defp string(m, key) when is_map(m) do
    case Map.get(m, key) do
      value when is_binary(value) -> value
      _ -> ""
    end
  end

  defp format_count(value) when is_integer(value) do
    value
    |> Integer.to_string()
    |> String.reverse()
    |> String.graphemes()
    |> Enum.chunk_every(3)
    |> Enum.map_join(",", &Enum.join/1)
    |> String.reverse()
  end

  defp short(value, n) when is_binary(value),
    do: value |> String.replace(~r/[\p{Cc}\p{Cf}]/u, " ") |> String.slice(0, n)

  defp short(_, _), do: nil
end
