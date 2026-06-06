defmodule Core.Response.OverrideSkills.Utility do
  @moduledoc """
  Utility override skills for direct safety and capability responses.
  """

  alias Core.Response.OverrideSkills.Decision

  @spec illicit_redirect(map(), map()) :: {:ok, {map(), map()}} | :pass
  def illicit_redirect(features, decision) do
    if Map.get(features, :intent) == :illicit_request do
      decision =
        decision
        |> Decision.put(tone: Map.get(decision, :tone), mode: :editor, action: :safe_redirect)
        |> Decision.add_override(:illicit_request_redirect)

      {:ok,
       {decision,
        %{
          id: :illicit_request_redirect,
          reason: :illicit_request,
          inline_text:
            "I can't help with buying drugs or getting wasted. I can help with safety, health risks, or getting support instead."
        }}}
    else
      :pass
    end
  end

  @spec time(map(), map()) :: {:ok, {map(), map()}} | :pass
  def time(features, decision) do
    text = Map.get(features, :text, "")

    if time_query?(text) do
      decision =
        decision
        |> Decision.put(mode: :chat, action: :time)
        |> Decision.add_override(:time_skill)

      {:ok,
       {decision,
        %{
          id: :time,
          reason: :time_query,
          inline_text: time_inline_text()
        }}}
    else
      :pass
    end
  end

  @spec alarm(map(), map()) :: {:ok, {map(), map()}} | :pass
  def alarm(features, decision) do
    text = Map.get(features, :text, "")

    if alarm_request?(text) do
      decision =
        decision
        |> Decision.put(tone: :warm, mode: :chat, action: :answer)
        |> Decision.add_override(:alarm_capability_answer)

      {:ok,
       {decision,
        %{
          id: :alarm_capability,
          reason: :alarm_request,
          inline_text:
            "I can't set a real device alarm yet. I can help you phrase one or keep a note here, but I don't have a phone/OS alarm integration wired in."
        }}}
    else
      :pass
    end
  end

  defp time_query?(text) when is_binary(text) do
    t = String.downcase(text)

    String.contains?(t, "what time") or
      String.contains?(t, "time is it") or
      String.contains?(t, "current time") or
      Regex.match?(~r/\btime\?\s*\z/u, String.trim(t))
  end

  defp time_query?(_), do: false

  defp alarm_request?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(set|make|create|start|schedule)\s+(an?\s+)?(alarm|timer|reminder)\b/u, t) or
      Regex.match?(~r/\b(alarm|timer|reminder)\s+(for|at|in)\b/u, t) or
      (String.contains?(t, "alarm") and Regex.match?(~r/\b(can|could|able|you)\b/u, t))
  end

  defp alarm_request?(_), do: false

  defp time_inline_text do
    utc = DateTime.utc_now()

    case safe_shift_zone(utc, "America/Vancouver") do
      {:ok, dt} ->
        formatted = Calendar.strftime(dt, "%-I:%M %p")
        "It’s #{formatted} (America/Vancouver)."

      _ ->
        formatted = Calendar.strftime(utc, "%H:%M UTC")
        "It’s #{formatted}."
    end
  end

  defp safe_shift_zone(dt, zone) do
    try do
      DateTime.shift_zone(dt, zone)
    rescue
      _ -> {:error, :no_tzdata}
    catch
      _, _ -> {:error, :no_tzdata}
    end
  end
end
