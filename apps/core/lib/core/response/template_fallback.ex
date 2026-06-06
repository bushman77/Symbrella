defmodule Core.Response.TemplateFallback do
  @moduledoc """
  Template fallback option shaping for response planning.
  """

  alias Core.Response.LlmSynthesis

  @spec opts(String.t(), map(), map()) :: map()
  def opts(text_in, features, decision) do
    %{
      text: text_in,
      variant_seed:
        :erlang.phash2(
          {text_in, Map.get(features, :intent), Map.get(features, :confidence_bucket),
           Map.get(decision, :tone), Map.get(decision, :mode), Map.get(decision, :action)}
        ),
      next_step: next_step(features, decision),
      file_hint: file_hint(text_in, features),
      context_status: LlmSynthesis.history_status(Map.get(features, :session_id))
    }
  end

  @spec normalize_reason(term()) :: atom()
  def normalize_reason(reason) when is_atom(reason), do: reason
  def normalize_reason({reason, _}) when is_atom(reason), do: reason
  def normalize_reason(%{reason: reason}) when is_atom(reason), do: reason
  def normalize_reason(%{"reason" => reason}) when is_atom(reason), do: reason
  def normalize_reason(_), do: :unknown

  defp next_step(features, decision) do
    cond do
      Map.get(features, :intent) == :health_support ->
        "Use safe health-support posture: acknowledge, avoid dose instructions, and suggest pharmacist or prescriber guidance."

      personal_finance_text?(Map.get(features, :text)) ->
        "Name the debts, balances, interest rates, payment status, and deadlines before choosing a repayment or consolidation plan."

      peace_or_war_question_text?(Map.get(features, :text)) ->
        nil

      casual_or_companion_text?(Map.get(features, :text)) ->
        nil

      personal_life_update_text?(Map.get(features, :text)) ->
        nil

      Map.get(features, :guardrail?) or Map.get(features, :risk_bucket) == :high ->
        "Give a brief safe redirect."

      low_confidence_task?(features) or comprehension_degraded_task?(features) ->
        "State what is understood, then ask one targeted question only if necessary."

      Map.get(decision, :action) == :act_first and technical_request?(features) ->
        "Make the next concrete engineering move."

      Map.get(decision, :mode) == :explainer ->
        "Explain from the available Symbrella evidence without implying sentience."

      true ->
        nil
    end
  end

  defp technical_request?(features) when is_map(features) do
    Map.get(features, :intent) in [
      :code,
      :command,
      :debug,
      :refactor,
      :review,
      :plan,
      :diagram,
      :bug,
      :optimize,
      :benchmark
    ] or technical_text?(Map.get(features, :text))
  end

  defp technical_request?(_), do: false

  defp low_confidence_task?(features) when is_map(features) do
    Map.get(features, :confidence_bucket) == :low and technical_request?(features)
  end

  defp low_confidence_task?(_), do: false

  defp comprehension_degraded_task?(features) when is_map(features) do
    comprehension_degraded?(features) and technical_request?(features)
  end

  defp comprehension_degraded_task?(_), do: false

  defp technical_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(code|coding|compile|compiler|debug|error|stacktrace|module|function|phoenix|elixir|liveview|server|repo|test|refactor|api|database|migration|deploy|pipeline)\b/iu,
      text
    )
  end

  defp technical_text?(_), do: false

  defp personal_finance_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(credit|credit karma|debt|debts|consolidat(?:e|ion|ing)|collections?|collector|loan|loans|interest rate|apr|minimum payment|bankruptcy|charge[-\s]?off|delinquen|late payment)\b/iu,
      text
    )
  end

  defp personal_finance_text?(_), do: false

  defp casual_or_companion_text?(text) when is_binary(text) do
    not substantive_question_text?(text) and
      Regex.match?(
        ~r/\b(he+y+|hi+|hello|yo+|sup|wh+a+t'?s*\s*u+p+|wha+t+s+\s*u+p+|huh+\??|c+mon|companion|friend|talk|chat|made you|default responses?|not code|write code)\b/iu,
        text
      )
  end

  defp casual_or_companion_text?(_), do: false

  defp peace_or_war_question_text?(text) when is_binary(text) do
    Regex.match?(~r/\bwhat\s+do\s+you\s+think\s+(of|about)\b/iu, text) or
      (Regex.match?(
         ~r/\b(altern+a+tives?|options?|instead|other\s+ways?|peace|diplomacy|negotiation|de[-\s]?escalation|ceasefire|sanctions?|mediation|war)\b/iu,
         text
       ) and Regex.match?(~r/\b(war|conflict|fighting|violence)\b/iu, text))
  end

  defp peace_or_war_question_text?(_), do: false

  defp substantive_question_text?(text) when is_binary(text) do
    peace_or_war_question_text?(text) or
      Regex.match?(
        ~r/\b(what|why|how|when|where|who|which|should|could|would|can)\b/iu,
        text
      )
  end

  defp substantive_question_text?(_), do: false

  defp personal_life_update_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(my own place|own apartment|own house|new apartment|new place|getting (?:my )?own place|moving out|move into (?:my|our) place|got approved for (?:an apartment|a place)|signed (?:a )?lease)\b/iu,
      text
    )
  end

  defp personal_life_update_text?(_), do: false

  defp comprehension_degraded?(features) do
    comprehension = Map.get(features, :comprehension)

    lifg_degraded? =
      case Map.get(features, :symbolic_frame) do
        %{lifg: %{degraded?: true}} -> true
        %{"lifg" => %{"degraded?" => true}} -> true
        _ -> false
      end

    (is_map(comprehension) and Map.get(comprehension, :degraded?) == true) or lifg_degraded?
  end

  defp file_hint(_text_in, %{file_hint: hint}) when is_binary(hint), do: hint

  defp file_hint(text_in, _features) when is_binary(text_in) do
    case Regex.run(~r/(?:apps|lib|test|config)\/[A-Za-z0-9_\.\/-]+/u, text_in) do
      [hint | _] -> hint
      _ -> nil
    end
  end

  defp file_hint(_text_in, _features), do: nil
end
