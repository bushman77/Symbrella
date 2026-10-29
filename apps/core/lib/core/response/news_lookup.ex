defmodule Core.Response.NewsLookup do
  @moduledoc """
  No-LLM, evidence-gated current-events discovery. Reuses the existing
  WebRetrieval -> EvidenceResolver -> NewsRss route and provider validations.

  Published headlines and source links are reported claims, never verified
  article content or an assessment of what actually happened. Retrieval must
  be explicitly enabled. The topic never becomes a factual premise.
  """

  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.NewsRss

  @max_headlines 3
  @max_user_bytes 320
  @news_host "news.google.com"
  @valid_actions [:answer, :offer_options, :act_first, :ask_first]

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(text, %{risk_bucket: :low} = features,
               %{action: action}, si)
      when is_binary(text) and byte_size(text) <= @max_user_bytes and
           action in @valid_actions and is_map(si) do
    with ^text <- Map.get(si, :sentence),
         true <- Map.get(si, :source) in [:user, :prod, :test],
         true <- Application.get_env(:core, :llm_inference_enabled?, true) == false,
         true <- retrieval_enabled?(),
         %{query: query, relation: :current_events,
           request_type: :current_events} = route <-
           WebRetrieval.route_for(%{user_text: text}),
         true <- is_binary(query) and byte_size(query) <= 160,
         evidence <- WebRetrieval.enrich(
           %{user_text: text, session_id: Map.get(si, :session_id),
             features: %{symbolic_frame: Map.get(features, :symbolic_frame)}},
           enabled?: true
         ),
         %{web_evidence: %{relation: :current_events,
                            request_type: :current_events,
                            support: nil} = news} <- evidence,
         true <- news.query == route.query do
      realize(news)
    else
      _ -> :not_applicable
    end
  end

  def generate(_, _, _, _), do: :not_applicable

  defp retrieval_enabled? do
    case Application.get_env(:core, :web_retrieval, []) do
      cfg when is_list(cfg) -> Keyword.get(cfg, :enabled?, false) == true
      _ -> false
    end
  end

  defp realize(%{status: :retrieved, resolution: :headlines_available,
                  sources: sources} = news) when is_list(sources) do
    now = DateTime.utc_now()

    headlines =
      sources
      |> Enum.filter(&safe_headline?(&1, now))
      |> Enum.take(@max_headlines)

    case headlines do
      [] -> unavailable(news)
      _ ->
        lines =
          Enum.map(headlines, fn item ->
            date = String.slice(item.published_at, 0, 10)
            publisher = clean(item.publisher, 70)
            title = clean(item.title, 180)
            "- #{date} — #{publisher}: #{title} (#{item.url})"
          end)

        message =
          "I found dated reporting on this subject, but have not read or " <>
            "independently verified the full articles. Recent headlines:\n" <>
            Enum.join(lines, "\n") <>
            "\nThese are source-attributed headlines, not verified facts " <>
            "about the conflict or a basis for a firm conclusion."

        {:ok, message, metadata(:headlines_available, length(headlines), news)}
    end
  end

  defp realize(%{status: status} = news)
       when status in [:unavailable, :not_found, :retrieved],
       do: unavailable(news)

  defp realize(_), do: :not_applicable

  defp unavailable(news) do
    {:ok,
     "I tried the current-events lookup, but it returned no usable, dated " <>
       "headlines for this question. I can't make a reliable current assessment " <>
       "without independently supported reporting.",
     metadata(:no_usable_headlines, 0, news)}
  end

  defp safe_headline?(%{provider: :news_rss, title: title,
                        publisher: publisher, url: url,
                        published_at: stamp} = item, now)
       when is_binary(title) and is_binary(publisher) and
            is_binary(url) and is_binary(stamp) do
    uri = URI.parse(url)
    uri.scheme == "https" and uri.host == @news_host and
      is_nil(uri.userinfo) and is_nil(uri.fragment) and
      String.trim(title) != "" and String.trim(publisher) != "" and
      NewsRss.recent_source?(item, now, NewsRss.default_age_hours())
  end

  defp safe_headline?(_, _), do: false

  defp clean(text, max) do
    text
    |> String.replace(~r/[\p{Cc}\p{Cf}\[\]<>*]/u, " ")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
    |> String.slice(0, max)
  end

  defp metadata(outcome, count, news) do
    %{
      status: :symbolic_news_discovery,
      outcome: outcome,
      requested_operation: :current_events_evidence_lookup,
      provenance: :dated_headline_discovery,
      retrieval_status: Map.get(news, :status),
      retrieval_resolution: Map.get(news, :resolution),
      source_count: count,
      content_scope: :headlines_only,
      article_contents_verified?: false,
      external_facts_verified?: false,
      factual_claims?: false,
      llm_called?: false
    }
  end
end
