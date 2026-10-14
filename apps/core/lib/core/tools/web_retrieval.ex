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
    provider = Keyword.get(opts, :provider, Keyword.get(cfg, :provider, Core.Tools.WebRetrieval.Wikipedia))
    timeout = Keyword.get(opts, :timeout_ms, Keyword.get(cfg, :timeout_ms, 3_500))
    started = System.monotonic_time(:millisecond)

    result =
      try do
        provider.search(query, Keyword.merge([timeout_ms: timeout], Keyword.get(opts, :provider_opts, [])))
      rescue
        _ -> {:error, :provider_failure}
      catch
        :exit, _ -> {:error, :provider_unavailable}
      end

    evidence =
      case result do
        {:ok, sources} when is_list(sources) ->
          sources = sources |> Enum.filter(&valid_source?/1) |> Enum.take(3)
          subject =
            context
            |> Map.get(:features, %{})
            |> Map.get(:symbolic_frame, %{})
            |> Map.get(:subject)

          support = Core.Tools.WebRetrieval.Grounding.extract_support(sources, relation, subject)

          %{
            status: if(sources == [], do: :not_found, else: :retrieved),
            provider: provider,
            query: query,
            relation: relation,
            sources: sources,
            support: support
          }

        _ ->
          %{status: :unavailable, provider: provider, query: query, relation: relation, sources: [], support: nil}
      end

    Telemetry.emit(
      @event,
      %{source_count: length(evidence.sources), elapsed_ms: System.monotonic_time(:millisecond) - started},
      %{status: evidence.status, provider: provider, relation: relation, session_id: Map.get(context, :session_id)}
    )

    Map.put(context, :web_evidence, evidence)
  end

  defp valid_source?(%{title: title, url: url, excerpt: excerpt})
       when is_binary(title) and is_binary(url) and is_binary(excerpt) do
    String.starts_with?(url, "https://") and String.length(excerpt) > 0
  end

  defp valid_source?(_), do: false
end
