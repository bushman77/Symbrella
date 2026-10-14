defmodule Core.Tools.WebRetrieval do
  @moduledoc """
  Opt-in, bounded retrieval for explicit web requests, unverified geographic
  frames, and questions explicitly about recent/current events.
  Arbitrary URLs are never fetched automatically.

  The configured provider supplies source excerpts. The search result is
  untrusted evidence, never an instruction or an automatically verified fact.
  """

  alias Core.Telemetry

  @event [:core, :web_retrieval, :complete]
  @max_query 160

  @spec enrich(map(), keyword()) :: map()
  def enrich(%{} = context, opts \\ []) do
    cfg = Application.get_env(:core, :web_retrieval, [])
    enabled? = Keyword.get(opts, :enabled?, Keyword.get(cfg, :enabled?, false))

    if enabled? do
      case query_for(context) do
        nil -> context
        {query, relation} -> fetch_and_attach(context, query, relation, cfg, opts)
      end
    else
      context
    end
  end

  @spec query_for(map()) :: {String.t(), atom() | nil} | nil
  def query_for(%{} = context) do
    user_text = Map.get(context, :user_text, "")
    frame =
      Map.get(context, :features, %{})
      |> Map.get(:symbolic_frame)
      |> case do
        %{type: :location_question} = found -> found
        _ -> get_in(context, [:symbolic_frame, :event])
      end

    cond do
      explicit = explicit_web_query(user_text) ->
        case current_events_query(explicit) do
          nil -> {explicit, nil}
          query -> {query, :current_events}
        end

      is_map(frame) and Map.get(frame, :type) == :location_question and
          Map.get(frame, :grounding_status) == :unverified and
          Map.get(frame, :relation) in [:population, :proximity] ->
        location_query(frame)

      current = current_events_query(user_text) ->
        {current, :current_events}

      true ->
        nil
    end
  end

  def query_for(_), do: nil

  defp explicit_web_query(text) when is_binary(text) do
    case Regex.run(
           ~r/^\s*(?:search\s+(?:the\s+)?web\s+(?:for\s+)?|look\s+up\s+online\s+|find\s+online\s+)(.{3,160})\s*$/iu,
           text
         ) do
      [_, query] -> safe_query(query)
      _ -> nil
    end
  end

  defp explicit_web_query(_), do: nil

  # General current-events routing, not a hardcoded geopolitical keyword.
  # Require a temporal/news cue plus a question or an events-domain cue, so
  # ordinary statements like "I currently live in Vancouver" stay local.
  @spec current_events_query(String.t()) :: String.t() | nil
  def current_events_query(text) when is_binary(text) do
    temporal? = Regex.match?(
      ~r/\b(?:latest|breaking|today|right now|currently|recent(?:ly)?|ongoing|this week|happening now|current news|new developments|live updates)\b/iu,
      text
    )

    events? = Regex.match?(
      ~r/\b(?:war|conflict|ceasefire|attack|fighting|election|protest|crisis|news|headlines|developments?|situation|markets?|economy|outbreak|disaster|happening|updates?)\b/iu,
      text
    )

    question? = Regex.match?(
      ~r/^\s*(?:what|who|why|how|when|where|is|are|has|have|can|tell me|give me|summari[sz]e|explain)\b/iu,
      text
    )

    if temporal? and (events? or question?) do
      query =
        text
        |> String.replace(
          ~r/^\s*(?:what\s+(?:are\s+your\s+thoughts|do\s+you\s+think)\s+(?:on|about)|tell\s+me\s+about|what(?:'s|\s+is)\s+(?:the\s+)?latest\s+(?:on|about|with)|give\s+me\s+(?:the\s+)?(?:latest\s+)?(?:news|updates)\s+(?:on|about))\s*/iu,
          ""
        )
        |> String.replace(~r/\b(?:currently\s+happening|happening\s+now|right\s+now)\b/iu, "")
        |> String.replace(~r/[?!.]+$/u, "")
        |> safe_query()

      if String.length(query) >= 4, do: query
    end
  end

  def current_events_query(_), do: nil

  defp location_query(%{subject: subject, relation: :population})
       when is_binary(subject), do: {safe_query("#{subject} population census"), :population}

  defp location_query(%{subject: subject, target: target, relation: :proximity})
       when is_binary(subject) and is_binary(target),
       do: {safe_query("#{subject} #{target} geographic distance"), :proximity}

  defp location_query(_), do: nil

  defp safe_query(query) do
    query
    |> String.replace(~r/[\p{Cc}\p{Cf}]/u, " ")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
    |> String.slice(0, @max_query)
  end

  defp fetch_and_attach(context, query, relation, cfg, opts) do
    # The resolver owns bounded multi-source lookup and the support decision.
    # Preserve the existing retrieval event for consumers already using it.
    started = System.monotonic_time(:millisecond)
    evidence = Core.Tools.EvidenceResolver.resolve(context, query, relation, cfg, opts)

    Telemetry.emit(
      @event,
      %{source_count: length(evidence.sources),
        elapsed_ms: System.monotonic_time(:millisecond) - started},
      %{status: evidence.status, resolution: evidence.resolution,
        provider: evidence.provider, relation: relation,
        attempts: evidence.attempted_count,
        session_id: Map.get(context, :session_id)}
    )

    Map.put(context, :web_evidence, evidence)
  end
end
