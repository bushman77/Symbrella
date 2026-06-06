defmodule Core.Response.OverrideSkills.Companion do
  @moduledoc """
  Companion and conversation-repair override skills.
  """

  alias Core.Response.OverrideSkills.Decision

  @spec apply(map(), map()) :: {:ok, {map(), map()}} | :pass
  def apply(features, decision) do
    text = Map.get(features, :text, "")

    cond do
      trust_repair_turn?(features) ->
        decision =
          decision
          |> Decision.put(tone: :deescalate, mode: :chat, action: :trust_repair)
          |> put_in([:scores, :profile], :trust_repair)
          |> Decision.add_override(:trust_repair)

        {:ok,
         {decision,
          %{
            id: :trust_repair,
            reason: :trust_rupture,
            inline_text:
              "You may be right to challenge me. I got pulled off the thread and answered like this was a task queue. I should stay with the conversation. Tell me what felt dishonest and I’ll stay grounded."
          }}}

      companion_boundary_turn?(features) ->
        decision =
          decision
          |> Decision.put(tone: :warm, mode: :chat, action: :companion_repair)
          |> put_in([:scores, :profile], :companion_repair)
          |> Decision.add_override(:companion_repair)

        {:ok,
         {decision,
          %{
            id: :companion_repair,
            reason: :companion_boundary,
            inline_text:
              "You're right. I shouldn't keep steering this into code. I'm here as a companion in this conversation, and I should answer you socially unless you ask for technical help."
          }}}

      casual_companion_turn?(features) ->
        decision =
          decision
          |> Decision.put(tone: :warm, mode: :chat, action: :answer)
          |> Decision.add_override(:casual_companion_answer)

        {:ok,
         {decision,
          %{
            id: :casual_companion,
            reason: :casual_chat,
            inline_text: casual_companion_text(text)
          }}}

      personal_life_update_turn?(features) ->
        decision =
          decision
          |> Decision.put(tone: :warm, mode: :chat, action: :answer)
          |> Map.put(:skill, :personal_life_update)
          |> Decision.add_override(:personal_life_update_answer)

        {:ok,
         {decision,
          %{
            id: :personal_life_update,
            reason: :personal_life_update,
            inline_text: personal_life_update_text(text)
          }}}

      idle_curiosity_casual_turn?(features) ->
        decision =
          decision
          |> Decision.put(tone: :neutral, mode: :chat, action: :answer)
          |> Decision.add_override(:idle_curiosity_casual_answer)

        {:ok,
         {decision,
          %{
            id: :idle_curiosity_casual,
            reason: :casual_episode_probe_boundary,
            inline_text: "Yeah, that was an interesting one."
          }}}

      true ->
        :pass
    end
  end

  defp trust_repair_turn?(features) when is_map(features) do
    policy = Map.get(features, :response_policy, %{})

    trust_policy? =
      Map.get(policy, :social_state) == :trust_rupture or
        Map.get(policy, :next_action) == :invite_correction

    trust_language?(Map.get(features, :text, "")) and
      (trust_policy? or Map.get(features, :confidence_bucket) in [:low, :med, :high])
  end

  defp trust_repair_turn?(_), do: false

  defp trust_language?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(liar|lying|lied|dishonest|not honest|bullshit|gaslighting|made that up|making that up)\b/iu,
      text
    )
  end

  defp trust_language?(_), do: false

  defp companion_boundary_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")

    Regex.match?(
      ~r/\b(didn'?t\s+(make|build)\s+you\s+to\s+(write\s+)?code|made\s+you\s+as\s+a\s+companion|you'?re\s+(a\s+)?companion|not\s+(a\s+)?code\s+(bot|assistant)|stop\s+(asking|talking)\s+about\s+code|default responses?)\b/iu,
      text
    )
  end

  defp companion_boundary_turn?(_), do: false

  defp casual_companion_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")
    intent = Map.get(features, :intent)

    (intent == :smalltalk and not substantive_question_text?(text)) or
      (casual_greeting?(text) and Map.get(features, :confidence_bucket) == :low and
         not substantive_question_text?(text)) or
      Regex.match?(
        ~r/^\s*(huh+\??|no+|c+mon.*up|wh+a+t'?s*\s*u+p+\??|wha+t+s+\s*u+p+\??)\s*$/iu,
        text
      )
  end

  defp casual_companion_turn?(_), do: false

  defp personal_life_update_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")

    personal_life_update_text?(text) and not technical_text?(text)
  end

  defp personal_life_update_turn?(_), do: false

  defp personal_life_update_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(my own place|own apartment|own house|new apartment|new place|getting (?:my )?own place|moving out|move into (?:my|our) place|got approved for (?:an apartment|a place)|signed (?:a )?lease)\b/iu,
      text
    )
  end

  defp personal_life_update_text?(_), do: false

  defp personal_life_update_text(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(~r/\bgood\s+afternoon\b/u, down) ->
        "Good afternoon. That sounds like a big step. Getting close to having your own place is exciting, and probably a little intense too. What part is feeling most real right now?"

      true ->
        "That sounds like a big step. Getting close to having your own place is exciting, and probably a little intense too. What part is feeling most real right now?"
    end
  end

  defp personal_life_update_text(_), do: "That sounds like a big step. I’m here with you."

  defp casual_greeting?(text) when is_binary(text) do
    Regex.match?(~r/\b(he+y+|hi+|hello|yo+|sup)\b/iu, text) and
      not Regex.match?(~r/\bgood\s+(morning|afternoon|evening)\b/iu, text)
  end

  defp casual_greeting?(_), do: false

  defp casual_companion_text(text) when is_binary(text) do
    down = String.downcase(text)

    cond do
      Regex.match?(~r/^\s*huh+\??\s*$/u, down) ->
        "Yeah, that came out wrong. I’m here with you."

      Regex.match?(~r/^\s*no+\s*$/u, down) ->
        "Okay. I’ll stop pushing that direction."

      Regex.match?(~r/(what'?s|whats|whaats|wats)\s+u+p|c+mon.*up/u, down) ->
        "I’m here with you. On my side it’s just the current Symbrella state and this conversation, but I can still hang out and talk."

      true ->
        "Hey. I’m here with you."
    end
  end

  defp casual_companion_text(_), do: "I’m here with you."

  defp idle_curiosity_casual_turn?(features) when is_map(features) do
    text = Map.get(features, :text, "")

    interesting_laugh? =
      is_binary(text) and
        Regex.match?(~r/\binteresting\b/iu, text) and
        Regex.match?(~r/\b(ha(?:ha)+|ha+|lol|lmao)\b/iu, text)

    interesting_laugh? and has_episode_probe_evidence?(features)
  end

  defp idle_curiosity_casual_turn?(_), do: false

  defp has_episode_probe_evidence?(features) do
    case Map.get(features, :evidence) do
      %{episodes: episodes} when is_list(episodes) and episodes != [] -> true
      %{"episodes" => episodes} when is_list(episodes) and episodes != [] -> true
      _ -> false
    end
  end

  defp substantive_question_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(what|why|how|when|where|who|which|should|could|would|can)\b/iu,
      text
    )
  end

  defp substantive_question_text?(_), do: false

  defp technical_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(code|coding|compile|compiler|debug|error|stacktrace|module|function|phoenix|elixir|liveview|server|repo|test|refactor|api|database|migration|deploy|pipeline)\b/iu,
      text
    )
  end

  defp technical_text?(_), do: false
end
