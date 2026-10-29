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

  @spec current_events(map(), map()) :: {:ok, {map(), map()}} | :pass
  def current_events(features, decision) do
    text = Map.get(features, :text, "")

    cond do
      news_retrieval_routable?(text, features) ->
        # EvidenceResolver is invoked by LlmSynthesis, not here. An inline
        # capability override bypasses it entirely, so let the response
        # planner continue when the news pipeline is configured.
        :pass

      current_events_query?(text) ->
        decision =
          decision
          |> Decision.put(tone: :neutral, mode: :chat, action: :capability_disclosure)
          |> Decision.add_override(:current_events_capability_answer)

        {:ok,
         {decision,
          %{
            id: :current_events_capability,
            reason: :no_live_news_source,
            inline_text:
              "I don't have a live news source available in this runtime right now, so I can't truthfully list today's current events. If you give me a region or topic, I can help frame what to look for or summarize background from what I already know."
          }}}

      true ->
        :pass
    end
  end

  defp news_retrieval_routable?(text, features) do
    cfg = Application.get_env(:core, :web_retrieval, [])
    enabled? =
      Map.get(
        features,
        :web_retrieval_enabled?,
        Keyword.get(cfg, :enabled?, false)
      )

    enabled? == true and
      is_binary(text) and
      match?(
        %{request_type: :current_events},
        Core.Tools.WebRetrieval.route_for(%{user_text: text})
      )
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

  defp current_events_query?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(~r/\b(?:current events|news|headlines)\b/u, t) and
      Regex.match?(~r/\b(?:today|current|latest|happening|now)\b/u, t)
  end

  defp current_events_query?(_), do: false

  defp time_inline_text do
    utc = DateTime.utc_now()

    case DateTime.shift_zone(utc, "America/Vancouver") do
      {:ok, dt} ->
        formatted = Calendar.strftime(dt, "%-I:%M %p")
        "It’s #{formatted} Pacific Time (#{dt.zone_abbr}, America/Vancouver)."

      _ ->
        {local, abbr} = pacific_wall_time(utc)
        formatted = Calendar.strftime(local, "%-I:%M %p")
        "It’s #{formatted} Pacific Time (#{abbr}, America/Vancouver)."
    end
  end

  defp pacific_wall_time(%DateTime{} = utc) do
    if pacific_daylight_time?(utc) do
      {utc |> DateTime.add(-7 * 60 * 60, :second) |> DateTime.to_naive(), "PDT"}
    else
      {utc |> DateTime.add(-8 * 60 * 60, :second) |> DateTime.to_naive(), "PST"}
    end
  end

  defp pacific_daylight_time?(%DateTime{} = utc) do
    year = utc.year
    dst_start_utc = pacific_dst_start_utc(year)
    dst_end_utc = pacific_dst_end_utc(year)

    DateTime.compare(utc, dst_start_utc) != :lt and DateTime.compare(utc, dst_end_utc) == :lt
  end

  defp pacific_dst_start_utc(year) do
    year
    |> nth_sunday(3, 2)
    |> DateTime.new!(~T[10:00:00], "Etc/UTC")
  end

  defp pacific_dst_end_utc(year) do
    year
    |> nth_sunday(11, 1)
    |> DateTime.new!(~T[09:00:00], "Etc/UTC")
  end

  defp nth_sunday(year, month, n) do
    first = Date.new!(year, month, 1)
    days_until_sunday = rem(7 - Date.day_of_week(first), 7)

    first
    |> Date.add(days_until_sunday)
    |> Date.add((n - 1) * 7)
  end
end
