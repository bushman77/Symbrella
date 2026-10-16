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
  alias Core.Tools.WebRetrieval.StatisticsCanadaPopulation
  alias Core.Tools.WebRetrieval.CanadianGeoNames
  alias Core.Tools.WebRetrieval.Searxng
  alias Core.Tools.WebRetrieval.NewsRss
  alias Core.Telemetry

  @max_attempts 3
  @max_sources 3
  @event [:core, :evidence_resolver, :complete]

  @spec resolve(map(), String.t(), atom() | nil, keyword(), keyword()) :: map()
  def resolve(context, query, relation, cfg, opts \\ [])

  def resolve(context, query, :population, cfg, opts) do
    subjects =
      context
      |> frame()
      |> Map.get(:subjects, [])
      |> List.wrap()
      |> Enum.filter(&(is_binary(&1) and String.trim(&1) != ""))
      |> Enum.uniq()
      |> Enum.take(4)

    if length(subjects) >= 2 do
      resolve_population_group(context, query, subjects, cfg, opts)
    else
      resolve_one(context, query, :population, cfg, opts)
    end
  end

  def resolve(context, query, relation, cfg, opts),
    do: resolve_one(context, query, relation, cfg, opts)

  defp resolve_one(context, query, relation, cfg, opts) do
    frame = frame(context)
    subject = Map.get(frame, :subject)
    target = Map.get(frame, :target)
    provider = Keyword.get(opts, :provider, Keyword.get(cfg, :provider, Core.Tools.WebRetrieval.Wikipedia))
    timeout = Keyword.get(opts, :timeout_ms, Keyword.get(cfg, :timeout_ms, 3_500))
    configured_budget = Keyword.get(opts, :max_attempts, Keyword.get(cfg, :max_attempts, @max_attempts))
    max_attempts = max(1, min(@max_attempts, configured_budget))
    provider_opts = Keyword.get(opts, :provider_opts, [])
    coords_provider =
      Keyword.get(opts, :coordinate_provider, Keyword.get(cfg, :coordinate_provider, Wikidata))
    coords_opts = Keyword.get(opts, :coordinate_opts, Keyword.get(cfg, :coordinate_opts, []))
    population_opts =
      Keyword.get(opts, :statistics_canada_opts,
        Keyword.get(cfg, :statistics_canada_opts, []))
      |> Keyword.put_new(:timeout_ms, 12_000)
    custom_coordinates? =
      Keyword.has_key?(opts, :coordinate_provider) or
        Keyword.has_key?(cfg, :coordinate_provider)

    population_plan =
      if custom_coordinates? do
        [{:population, coords_provider, nil, coords_opts}]
      else
        [{:population, StatisticsCanadaPopulation, nil, population_opts},
         {:population, Wikidata, nil, coords_opts}]
      end

    coordinate_plan =
      if custom_coordinates? do
        [{:coordinates, coords_provider, nil, coords_opts}]
      else
        [{:coordinates, CanadianGeoNames, nil, coords_opts},
         {:coordinates, Wikidata, nil, coords_opts}]
      end

    news_provider = Keyword.get(opts, :news_provider, Keyword.get(cfg, :news_provider, NewsRss))
    news_opts = Keyword.get(opts, :news_opts, [])
    request_type = Keyword.get(opts, :request_type)
    start = System.monotonic_time(:millisecond)

    plans =
      if relation == :current_events do
        # Fresh, dated news headlines are discovery evidence, never verified
        # article text. News searches do not route through Wikipedia.
        [{:news, news_provider, query, news_opts},
         {:news, news_provider, "#{query} latest", news_opts}] ++
          optional_search(cfg, news_provider, query, opts)
      else
        (cond do
           relation == :identity and is_binary(subject) ->
             [{:identity, coords_provider, nil, coords_opts}]

           relation == :population and is_binary(subject) ->
             # Dated entity statements outrank arbitrary numbers in search
             # excerpts; every claim must have a date and exact identity.
             population_plan ++
               [{:search, provider, "#{subject} official population estimate", provider_opts}]

           true ->
             [{:search, provider, query, provider_opts}]
         end) ++
          (if relation == :proximity and is_binary(subject) and is_binary(target),
             do: coordinate_plan,
             else: []) ++
          optional_search(cfg, provider, query, opts)
      end

    {attempts, sources, support, resolution} =
      plans
      |> Enum.take(max_attempts)
      |> Enum.reduce_while({[], [], nil, :unverified}, fn plan, {traces, gathered, _, _} ->
        {trace, found, candidate} = attempt(plan, subject, target, relation, timeout, request_type)
        # When a fallback finally supports the claim, put its cited sources
        # first instead of losing them behind three irrelevant search hits.
        ordered =
          if is_map(candidate) and Map.get(candidate, :status) == :source_supported,
            do: found ++ gathered,
            else: gathered ++ found

        gathered = ordered |> Enum.uniq_by(&Map.get(&1, :url)) |> Enum.take(@max_sources)
        traces = traces ++ [trace]

        case candidate do
          %{status: :source_supported} ->
            {:halt, {traces, gathered, candidate, :source_supported}}

          %{status: :headlines_available} ->
            # Headlines have dates and provenance, but are not independently
            # verified facts; do not put them in :support.
            {:halt, {traces, gathered, nil, :headlines_available}}

          %{status: :reference_sources} ->
            # General reference snippets are evidence candidates, not
            # claim-specific verification (unlike population/proximity).
            {:halt, {traces, gathered, nil, :reference_sources}}

          _ ->
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
      provider:
        cond do
          relation == :current_events -> news_provider
          is_map(support) and is_atom(Map.get(support, :source_type)) ->
            Map.get(support, :source_type)
          is_map(support) and is_atom(Map.get(support, :coordinate_provider)) ->
            Map.get(support, :coordinate_provider)
          true -> provider
        end,
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

  # Resolve a bounded set of *independently dated* population claims.
  # No combined count, inferred year, or copied source attribution is allowed.
  # One failing place never prevents returning the others.
  defp resolve_population_group(context, query, subjects, cfg, opts) do
    started = System.monotonic_time(:millisecond)

    results =
      Enum.map(subjects, fn subject ->
        single_frame =
          context
          |> frame()
          |> Map.put(:subject, subject)
          |> Map.delete(:subjects)

        single_context =
          context
          |> Map.update(:features, %{symbolic_frame: single_frame}, fn
            %{} = features -> Map.put(features, :symbolic_frame, single_frame)
            _ -> %{symbolic_frame: single_frame}
          end)
          |> Map.put(:symbolic_frame, %{event: single_frame})

        result =
          resolve_one(single_context, "#{subject} population census", :population, cfg, opts)

        support = result.support

        verified? =
          result.resolution == :source_supported and
            is_map(support) and support[:kind] == :population and
            support[:status] == :source_supported and
            (is_nil(support[:subject]) or support[:subject] == subject) and
            is_integer(support[:value]) and support[:value] > 0 and
            is_integer(support[:census_year]) and
            is_binary(support[:source_url]) and
            String.starts_with?(support[:source_url], "https://")

        entry =
          if verified? do
            %{
              subject: subject,
              status: :source_supported,
              value: support.value,
              census_year: support.census_year,
              source_url: support.source_url,
              source_type: Map.get(support, :source_type, :wikipedia),
              measurement_type: Map.get(support, :measurement_type, :dated_population_statement),
              reference_year: Map.get(support, :reference_year, support.census_year),
              source_table: Map.get(support, :source_table)
            }
          else
            %{subject: subject, status: :unavailable}
          end

        {entry, result}
      end)

    entries = Enum.map(results, &elem(&1, 0))
    verified = Enum.filter(entries, &(&1.status == :source_supported))

    resolution =
      cond do
        length(verified) == length(entries) -> :source_supported
        verified != [] -> :partial_support
        true -> :exhausted
      end

    sources =
      Enum.map(verified, fn entry ->
        %{
          title: entry.subject,
          url: entry.source_url,
          provider: entry.source_type,
          excerpt: "Dated population statement (#{entry.census_year}): #{entry.value}"
        }
      end)

    attempts =
      Enum.flat_map(results, fn {entry, result} ->
        Enum.map(result.attempts, &Map.put(&1, :subject, entry.subject))
      end)

    verified_providers = verified |> Enum.map(& &1.source_type) |> Enum.uniq()

    source_provider =
      case verified_providers do
        [single] -> single
        [] -> :none
        _ -> :multiple
      end

    result = %{
      status: if(verified == [], do: :unavailable, else: :retrieved),
      resolution: resolution,
      provider: source_provider,
      query: query,
      relation: :population,
      support: %{
        kind: :population_group,
        status: resolution,
        subjects: subjects,
        results: entries
      },
      sources: sources,
      attempts: attempts,
      attempted_count: length(attempts),
      exhausted?: resolution == :exhausted
    }

    Telemetry.emit(@event,
      %{
        attempts: length(attempts),
        source_count: length(sources),
        elapsed_ms: System.monotonic_time(:millisecond) - started
      },
      %{
        status: result.status, resolution: resolution, relation: :population,
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

  defp attempt({:news, provider, query, provider_opts}, _subject, _target, _relation, timeout, _request_type) do
    reply = safe_call(fn ->
      provider.search(query, Keyword.merge([timeout_ms: timeout], provider_opts))
    end)

    case reply do
      {:ok, found} when is_list(found) ->
        # Recheck timestamps even if a custom provider returned the items.
        # A provider returning stale data must not imply current evidence.
        max_age_hours = Keyword.get(provider_opts, :max_age_hours, NewsRss.default_age_hours())
        now = Keyword.get(provider_opts, :now, DateTime.utc_now())

        items =
          found
          |> Enum.filter(&valid_news_source?/1)
          |> Enum.filter(&NewsRss.recent_source?(&1, now, max_age_hours))
          |> Enum.take(@max_sources)
        status = if items == [], do: :not_found, else: :headlines_available
        candidate = if items == [], do: nil, else: %{status: :headlines_available}

        {%{kind: :news, provider: provider, query: query,
           status: status, source_count: length(items)},
         items, candidate}

      _ ->
        {%{kind: :news, provider: provider, query: query,
           status: :unavailable, source_count: 0}, [], nil}
    end
  end

  defp attempt({:search, provider, query, provider_opts}, subject, _target, relation, timeout, request_type) do
    reply = safe_call(fn ->
      provider.search(query, Keyword.merge([timeout_ms: timeout], provider_opts))
    end)

    case reply do
      {:ok, found} when is_list(found) ->
        sources =
          found
          |> Enum.filter(&valid_source?/1)
          |> require_named_focus(query, relation, request_type)
          |> Enum.take(@max_sources)

        support = Grounding.extract_support(sources, relation, subject)
        state =
          cond do
            is_map(support) -> :source_supported
            sources == [] -> :not_found
            is_nil(relation) -> :reference_sources
            true -> :retrieved_unsupported
          end

        candidate = if state == :reference_sources, do: %{status: :reference_sources}, else: support

        {%{kind: :search, provider: provider, query: query,
           status: state, source_count: length(sources)},
         sources, candidate}

      _ ->
        {%{kind: :search, provider: provider, query: query,
           status: :unavailable, source_count: 0}, [], nil}
    end
  end

  defp attempt({:population, provider, _query, provider_opts}, subject, _target, _relation, timeout, _request_type) do
    reply = safe_call(fn ->
      provider.population(subject, Keyword.merge([timeout_ms: timeout], provider_opts))
    end)

    case reply do
      {:ok, %{kind: :population, status: :source_supported,
              value: value, census_year: year, source_url: url,
              sources: sources} = support}
      when is_integer(value) and value > 0 and is_integer(year) and
           is_binary(url) and is_list(sources) and length(sources) == 1 ->
        found = Enum.map(sources, fn source ->
          Map.put(source, :excerpt, "Dated population statement (#{year}): #{value}")
        end)
        {%{kind: :population, provider: provider, query: subject,
           status: :source_supported, source_count: length(found)},
         found, support}

      _ ->
        {%{kind: :population, provider: provider, query: subject,
           status: :unavailable, source_count: 0}, [], nil}
    end
  end

  defp attempt({:identity, provider, _query, provider_opts}, subject, _target, _relation, timeout, _request_type) do
    reply = safe_call(fn ->
      provider.place(subject, nil, Keyword.merge([timeout_ms: timeout], provider_opts))
    end)

    case reply do
      {:ok, %{title: title, url: url, provider: :wikidata, description: description}}
      when is_binary(title) and is_binary(url) and is_binary(description) and
           byte_size(description) > 5 ->
        citation = %{title: title, provider: :wikidata, url: url}
        support = %{
          kind: :identity,
          status: :source_supported,
          subject: subject,
          title: title,
          description: description,
          sources: [citation]
        }
        {%{kind: :identity, provider: provider, query: subject,
           status: :source_supported, source_count: 1},
         [Map.put(citation, :excerpt, description)], support}

      _ ->
        {%{kind: :identity, provider: provider, query: subject,
           status: :unavailable, source_count: 0}, [], nil}
    end
  end

  defp attempt({:coordinates, provider, _query, provider_opts}, subject, target, _relation, timeout, _request_type) do
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
             "Verified representative place coordinate from #{inspect(Map.get(source, :provider))} " <>
               "for #{Map.get(source, :title)}")
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

  # Named-subject searches need both the named entity and actual topic
  # terms. A partial substring ("Japan" in "Japanese") is not sufficient,
  # and entertainment results are not evidence for a technical programme.
  # This is conservative *relevance*, not independent fact verification.
  defp require_named_focus(sources, query, nil, :general_search) do
    case Regex.run(
           ~r/\b([A-Z][\p{L}-]{2,})['’]s\b/u,
           query,
           capture: :all_but_first
         ) do
      [name] ->
        focus = String.downcase(name)

        terms =
          query
          |> String.downcase()
          |> then(&Regex.scan(~r/[\p{L}\p{N}]+/u, &1))
          |> List.flatten()
          |> Enum.reject(&(&1 in [focus, "s", "the", "a", "an", "of", "for", "on", "about"]))
          |> Enum.filter(&(String.length(&1) >= 4))
          |> Enum.uniq()

        Enum.filter(sources, fn source ->
          text =
            Map.get(source, :title, "") <> " " <> Map.get(source, :excerpt, "")

          named_word?(text, focus) and
            (terms == [] or Enum.any?(terms, &whole_word?(text, &1))) and
            not unrelated_media?(text, query)
        end)

      _ ->
        sources
    end
  end

  defp require_named_focus(sources, _, _, _), do: sources

  defp named_word?(text, focus) do
    word_forms =
      # Adjectival forms such as Japanese/Taiwanese can name a country,
      # but their mere substring is not sufficient proof of relevance.
      [focus, focus <> "ese", focus <> "an", focus <> "ian"]
      |> Enum.uniq()

    Enum.any?(word_forms, &whole_word?(text, &1))
  end

  defp whole_word?(text, word) do
    Regex.match?(
      Regex.compile!("(?<![\\p{L}\\p{N}])" <> Regex.escape(word) <> "(?![\\p{L}\\p{N}])", "iu"),
      text
    )
  end

  defp unrelated_media?(text, query) do
    media =
      ~r/\b(?:anime|animated television|television series|tv series|manga|fictional|video game|comic book|television season|season \d+)\b/iu

    Regex.match?(media, text) and not Regex.match?(media, query)
  end

  defp valid_news_source?(%{provider: :news_rss, title: title, url: url,
                                   published_at: stamp, excerpt: excerpt})
       when is_binary(title) and is_binary(url) and is_binary(stamp) and
              is_binary(excerpt) do
    valid_source?(%{title: title, url: url, excerpt: excerpt}) and
      case DateTime.from_iso8601(stamp) do
        {:ok, published, _offset} ->
          age_seconds = DateTime.diff(DateTime.utc_now(), published, :second)
          age_seconds >= -3_600 and age_seconds <= NewsRss.max_age_hours() * 3_600

        _ ->
          false
      end
  end

  defp valid_news_source?(_), do: false

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
