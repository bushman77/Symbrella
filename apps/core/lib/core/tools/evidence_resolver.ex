defmodule Core.Tools.EvidenceResolver do
  @moduledoc """
  Bounded evidence acquisition and claim support assessment.

  Retrieval success is not verification. The resolver stops on a claim-specific,
  source-supported fact, otherwise tries up to two additional strategies.
  All attempts and provenance are retained. It never promotes snippets or
  repeated Wikimedia data to independent corroboration.

  Current claim validators: population (sourced article excerpt) and geographic
  proximity (Wikidata entity coordinates + deterministic haversine distance).
  Unsupported claim types remain unverified, ready for future validators.
  """

  alias Core.Tools.WebRetrieval.Grounding
  alias Core.Tools.WebRetrieval.Wikidata
  alias Core.Tools.WebRetrieval.Searxng
  alias Core.Telemetry

  @max_attempts 3
  @max_sources 3
  @event [:core, :evidence_resolver, :complete]

  @spec resolve(map(), String.t(), atom() | nil, keyword(), keyword()) :: map()
  def resolve(context, query, relation, cfg, opts \\ []) do
    frame = frame(context)
    subject = Map.get(frame, :subject)
    target = Map.get(frame, :target)
    provider = Keyword.get(opts, :provider, Keyword.get(cfg, :provider, Core.Tools.WebRetrieval.Wikipedia))
    timeout = Keyword.get(opts, :timeout_ms, Keyword.get(cfg, :timeout_ms, 3_500))
    configured_budget = Keyword.get(opts, :max_attempts, Keyword.get(cfg, :max_attempts, @max_attempts))
    max_attempts = max(1, min(@max_attempts, configured_budget))
    provider_opts = Keyword.get(opts, :provider_opts, [])
    coords_provider = Keyword.get(opts, :coordinate_provider, Wikidata)
    coords_opts = Keyword.get(opts, :coordinate_opts, [])
    start = System.monotonic_time(:millisecond)

    plans =
      [{:search, provider, query, provider_opts}] ++
        (if relation == :proximity and is_binary(subject) and is_binary(target),
           do: [{:coordinates, coords_provider, nil, coords_opts}],
           else: []) ++
        (if relation == :population and is_binary(subject),
           do: [{:search, provider, "#{subject} 2021 census population", provider_opts}],
           else: []) ++
        optional_search(cfg, provider, query, opts)

    {attempts, sources, support, resolution} =
      plans
      |> Enum.take(max_attempts)
      |> Enum.reduce_while({[], [], nil, :unverified}, fn plan, {traces, gathered, _, _} ->
        {trace, found, candidate} = attempt(plan, subject, target, relation, timeout)
        # When a fallback finally supports the claim, put its cited sources
        # first instead of losing them behind three irrelevant search hits.
        ordered =
          if is_map(candidate) and Map.get(candidate, :status) == :source_supported,
            do: found ++ gathered,
            else: gathered ++ found

        gathered = ordered |> Enum.uniq_by(&Map.get(&1, :url)) |> Enum.take(@max_sources)
        traces = traces ++ [trace]

        if is_map(candidate) and Map.get(candidate, :status) == :source_supported do
          {:halt, {traces, gathered, candidate, :source_supported}}
        else
          {:cont, {traces, gathered, nil, :unverified}}
        end
      end)

    status =
      cond do
        sources != [] -> :retrieved
        Enum.any?(attempts, &(&1.status == :unavailable)) -> :unavailable
        true -> :not_found
      end

    resolution = if resolution == :unverified, do: :exhausted, else: resolution

    result = %{
      status: status,
      resolution: resolution,
      provider: provider,
      query: query,
      relation: relation,
      support: support,
      sources: sources,
      attempts: attempts,
      attempted_count: length(attempts),
      exhausted?: resolution == :exhausted
    }

    Telemetry.emit(@event,
      %{
        attempts: length(attempts),
        source_count: length(sources),
        elapsed_ms: System.monotonic_time(:millisecond) - start
      },
      %{
        status: status, resolution: resolution, relation: relation,
        session_id: Map.get(context, :session_id)
      }
    )

    result
  end

  defp optional_search(cfg, provider, query, opts) do
    configured = Keyword.get(opts, :fallback_provider, Keyword.get(cfg, :fallback_provider))
    configured = configured || if(Application.get_env(:core, :searxng_url), do: Searxng)

    if is_atom(configured) and configured != provider and not is_nil(configured),
      do: [{:search, configured, query, Keyword.get(opts, :fallback_opts, [])}],
      else: []
  end

  defp attempt({:search, provider, query, provider_opts}, subject, _target, relation, timeout) do
    reply = safe_call(fn ->
      provider.search(query, Keyword.merge([timeout_ms: timeout], provider_opts))
    end)

    case reply do
      {:ok, found} when is_list(found) ->
        sources = found |> Enum.filter(&valid_source?/1) |> Enum.take(@max_sources)
        support = Grounding.extract_support(sources, relation, subject)
        state =
          cond do
            is_map(support) -> :source_supported
            sources == [] -> :not_found
            true -> :retrieved_unsupported
          end

        {%{kind: :search, provider: provider, query: query,
           status: state, source_count: length(sources)},
         sources, support}

      _ ->
        {%{kind: :search, provider: provider, query: query,
           status: :unavailable, source_count: 0}, [], nil}
    end
  end

  defp attempt({:coordinates, provider, _query, provider_opts}, subject, target, _relation, timeout) do
    reply = safe_call(fn ->
      provider.proximity(subject, target,
        Keyword.merge([timeout_ms: timeout], provider_opts))
    end)

    case reply do
      {:ok, %{status: :source_supported, sources: sources} = support}
      when is_list(sources) ->
        # The source records represent structured Wikidata entity claims,
        # not independently corroborating documents.
        {%{kind: :coordinates, provider: provider, query: "#{subject} / #{target}",
           status: :source_supported, source_count: length(sources)},
         Enum.map(sources, fn source ->
           Map.put(source, :excerpt,
             "Coordinate location (Wikidata P625) for #{Map.get(source, :title)}")
         end), support}

      _ ->
        {%{kind: :coordinates, provider: provider, query: "#{subject} / #{target}",
           status: :unavailable, source_count: 0}, [], nil}
    end
  end

  defp safe_call(fun) do
    try do
      fun.()
    rescue
      _ -> {:error, :provider_failure}
    catch
      :exit, _ -> {:error, :provider_unavailable}
      :throw, _ -> {:error, :provider_failure}
    end
  end

  defp valid_source?(%{title: title, url: url, excerpt: excerpt})
       when is_binary(title) and is_binary(url) and is_binary(excerpt) do
    String.starts_with?(url, "https://") and byte_size(excerpt) > 0
  end

  defp valid_source?(_), do: false

  defp frame(context) do
    case get_in(context, [:features, :symbolic_frame]) do
      %{type: :location_question} = found -> found
      _ ->
        case get_in(context, [:symbolic_frame, :event]) do
          %{type: :location_question} = found -> found
          _ -> %{}
        end
    end
  end
end
