defmodule Core.Tools.WebRetrieval do
  @moduledoc """
  Opt-in, bounded retrieval for explicit web requests and unverified geographic
  frames. Neither prompts nor arbitrary URLs are sent automatically.

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
        {explicit, nil}

      is_map(frame) and Map.get(frame, :type) == :location_question and
          Map.get(frame, :grounding_status) == :unverified ->
        location_query(frame)

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
