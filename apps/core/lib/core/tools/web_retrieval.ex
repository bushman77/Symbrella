defmodule Core.Tools.WebRetrieval do
  @moduledoc """
  Opt-in, bounded external retrieval.

  RequestRouter deterministically recognizes explicit searches, several
  research/reference intentions, current events and supported geographic
  frames. Sources remain untrusted data; no arbitrary URL is fetched.
  """

  alias Core.Telemetry
  alias Core.Tools.WebRetrieval.RequestRouter

  @event [:core, :web_retrieval, :complete]

  @spec enrich(map(), keyword()) :: map()
  def enrich(%{} = context, opts \\ []) do
    cfg = Application.get_env(:core, :web_retrieval, [])
    enabled? = Keyword.get(opts, :enabled?, Keyword.get(cfg, :enabled?, false))

    if enabled? do
      routing_context =
        Map.put(context, :retrieval_history,
          Keyword.get(opts, :history, Map.get(context, :retrieval_history, [])))

      case route_for(routing_context) do
        nil -> context
        route -> fetch_and_attach(context, route, cfg, opts)
      end
    else
      context
    end
  end

  @doc "A typed, inspectable route including trigger and request category."
  @spec route_for(map()) :: RequestRouter.route() | nil
  def route_for(context), do: RequestRouter.route(context)

  @doc "Compatibility with callers expecting only {query, relation}."
  @spec query_for(map()) :: {String.t(), atom() | nil} | nil
  def query_for(context) do
    case route_for(context) do
      %{query: query, relation: relation} -> {query, relation}
      _ -> nil
    end
  end

  @spec current_events_query(String.t()) :: String.t() | nil
  defdelegate current_events_query(text), to: RequestRouter

  defp provider_options(%{relation: nil, request_type: type}, opts)
       when type in [:general_search, :fact_check, :comparison,
                     :scientific_research, :technical_reference] do
    # Optional SearXNG provides web-wide results. With no configured server,
    # Wikipedia remains the bounded reference provider; never imply general
    # internet coverage from an encyclopedia snippet.
    if Application.get_env(:core, :searxng_url) do
      opts
      |> Keyword.put_new(:provider, Core.Tools.WebRetrieval.Searxng)
      |> Keyword.put_new(:fallback_provider, Core.Tools.WebRetrieval.Wikipedia)
    else
      opts
    end
  end

  defp provider_options(_, opts), do: opts

  defp fetch_and_attach(context, route, cfg, opts) do
    started = System.monotonic_time(:millisecond)
    %{query: query, relation: relation, request_type: type, trigger: trigger} = route

    resolver_opts = provider_options(route, opts)

    evidence =
      context
      |> Core.Tools.EvidenceResolver.resolve(query, relation, cfg, resolver_opts)
      |> Map.put(:request_type, type)
      |> Map.put(:routing_trigger, trigger)

    Telemetry.emit(
      @event,
      %{source_count: length(evidence.sources),
        elapsed_ms: System.monotonic_time(:millisecond) - started},
      %{status: evidence.status, resolution: evidence.resolution,
        provider: evidence.provider, relation: relation,
        request_type: type, trigger: trigger,
        attempts: evidence.attempted_count,
        session_id: Map.get(context, :session_id)}
    )

    Map.put(context, :web_evidence, evidence)
  end
end
