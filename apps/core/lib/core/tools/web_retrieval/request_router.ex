defmodule Core.Tools.WebRetrieval.RequestRouter do
  @moduledoc """
  Deterministic, inspectable retrieval intent classification.

  Routes are *requests for evidence*, not claims that any facts were verified.
  A news route uses dated RSS headlines; all other categories use the bounded
  reference/search resolver (Wikipedia, optionally SearXNG). No LLM is asked
  to guess whether a retrieval call should happen.

  Anaphoric retrieval requests ("search for it", "any updates on that")
  require a recent substantive USER turn from this session. Assistant
  responses are never used as search instructions.
  """

  @max_query 160
  @max_prior_turns 6

  @type route :: %{
          query: String.t(),
          relation: atom() | nil,
          request_type: atom(),
          trigger: atom()
        }

  @spec route(map()) :: route() | nil
  def route(%{} = context) do
    text = Map.get(context, :user_text, "")
    frame = get_in(context, [:features, :symbolic_frame])
    frame =
      if is_map(frame) and Map.get(frame, :type) == :location_question,
        do: frame,
        else: get_in(context, [:symbolic_frame, :event])

    cond do
      not is_binary(text) or String.trim(text) == "" ->
        nil

      # Explicit user search requests outrank automatic topic detection.
      request = explicit_request(text) ->
        explicit_route(request, context)

      is_map(frame) and Map.get(frame, :type) == :location_question and
          Map.get(frame, :grounding_status) == :unverified and
          Map.get(frame, :relation) in [:population, :proximity] ->
        location_route(frame)

      research = research_query(text) ->
        {query, category} = research
        build(query, nil, category, :structured_question)

      # Resolve deictic references BEFORE the generic current-events test.
      # "those developments" is not itself a legitimate news subject.
      anaphora?(text) ->
        previous_route(context, text)

      current = current_events_query(text) ->
        build(current, :current_events, :current_events, :temporal_question)

      true ->
        nil
    end
  end

  def route(_), do: nil

  @doc "Extract a bounded current-events query without requiring an LLM."
  @spec current_events_query(String.t()) :: String.t() | nil
  def current_events_query(text) when is_binary(text) do
    temporal? =
      Regex.match?(
        ~r/\b(?:latest|breaking|today|tonight|yesterday|now|right now|currently|current|recent(?:ly)?|ongoing|this week|last week|this month|happening now|new developments|live updates|up[- ]to[- ]date|newest)\b/iu,
        text
      ) or Regex.match?(~r/\bas\s+of\s+(?:now|today|current(?:ly)?|this\s+(?:week|month|year))\b/iu, text)

    events? = Regex.match?(
      ~r/\b(?:war|conflict|ceasefire|attacks?|fighting|elections?|protests?|crisis|news|headlines|developments?|situation|markets?|economy|outbreak|disaster|happening|updates?|breakthrough|release|announcements?|railguns?)\b/iu,
      text
    )

    question? = Regex.match?(
      ~r/^\s*(?:what|who|why|how|when|where|is|are|has|have|can|tell me|give me|summari[sz]e|explain|any updates)\b/iu,
      text
    )

    strong_news_question? =
      question? and
        Regex.match?(~r/\b(?:latest|breaking|new developments|live updates|up[- ]to[- ]date)\b/iu, text)

    if temporal? and (events? or strong_news_question?) do
      text
      |> String.replace(
        ~r/^\s*(?:what\s+(?:are\s+your\s+thoughts|do\s+you\s+think)\s+(?:on|about)|what(?:'s|\s+is)\s+your\s+(?:thoughts|take)\s+(?:on|about)|tell\s+me\s+about|what(?:'s|\s+is)\s+(?:the\s+)?latest\s+(?:on|about|with)|give\s+me\s+(?:the\s+)?(?:latest\s+)?(?:news|updates)\s+(?:on|about))\s*/iu,
        ""
      )
      |> String.replace(~r/\b(?:currently\s+happening|happening\s+now|right\s+now|as\s+of\s+(?:now|today|current(?:ly)?))\b/iu, "")
      |> String.replace(~r/[?!.]+$/u, "")
      |> nonempty()
    end
  end

  def current_events_query(_), do: nil

  defp explicit_request(text) do
    # A narrowly bounded directive anywhere in a correction ("No, search for
    # it") is still explicit. Do not reinterpret unrelated mentions of a
    # search engine as an instruction.
    prefix =
      ~r/^\s*(?:(?:please|can you|could you|would you|hey symbrella[,!]?|no[,!]?|actually[,!]?)\s+)*\s*(?:do\s+(?:a\s+)?(?:web\s+)?search\s+(?:for|on|about)\s+|search\s+(?:(?:the\s+)?(?:web|internet|online)\s+)?(?:(?:for|about|on)\s+)?|look\s+up\s+(?:online\s+)?|look\s+into\s+|check\s+online\s+(?:for\s+)?|browse\s+(?:the\s+web\s+)?(?:for\s+)?|find\s+(?:online\s+)?(?:information|sources|articles)\s+(?:on|about|for)\s+|find\s+out\s+about\s+|research\s+|fact[- ]check\s+|verify\s+|compare\s+(?:sources\s+(?:on|about|for)\s+)?)(.+)$/iu

    case Regex.run(prefix, text, capture: :all_but_first) do
      [query] -> {:explicit, clean_query(query), text}
      _ ->
        # "Look it up" and conversational corrections cannot be parsed as
        # a standalone topic without a same-session antecedent.
        if Regex.match?(
             ~r/^\s*(?:(?:please|no[,!]?|actually[,!]?|can you|could you)\s+)*(?:look\s+(?:it|that|this)\s+up|(?:do\s+a\s+)?search\s+for\s+(?:it|that|this)|check\s+(?:it|that|this)\s+(?:online|on\s+the\s+web)|find\s+out\s+more\s+about\s+(?:it|that|this))\s*[.!?]*$/iu,
             text
           ) do
          {:explicit, "it", text}
        end
    end
  end

  defp explicit_route({:explicit, query, original}, context) do
    # A direction such as "search for the latest developments" describes a
    # retrieval *operation*, not the topic. Reuse an earlier user subject or
    # decline routing; never send a subject-free generic RSS request.
    contextual? = unresolved_reference?(query) or topicless_news?(query)

    resolved =
      if contextual?, do: prior_topic(context), else: nonempty(query)

    if resolved do
      cond do
        contextual? and news_intent?(original) ->
          build(resolved, :current_events, :current_events, :explicit_search)

        current_events_query(resolved) ->
          build(current_events_query(resolved), :current_events,
            :current_events, :explicit_search)

        news_intent?(original) ->
          build(resolved, :current_events, :current_events, :explicit_search)

        true ->
          build(resolved, nil, explicit_type(original, resolved), :explicit_search)
      end
    end
  end

  defp news_intent?(text) do
    Regex.match?(
      ~r/\b(?:latest|breaking|recent|current|today|new)\b/iu, text
    ) and
      Regex.match?(
        ~r/\b(?:news|headlines|updates?|developments?|reports?)\b/iu, text
      )
  end

  defp topicless_news?(text) when is_binary(text) do
    Regex.match?(
      ~r/^\s*(?:(?:the|any|some)\s+)?(?:(?:latest|recent|new|current|breaking|today'?s?)\s+)*(?:news|headlines|updates?|developments?|reports?)(?:\s+(?:about|on|regarding)\s+(?:it|this|that|those|them))?\s*[.!?]*$/iu,
      text
    )
  end

  defp topicless_news?(_), do: false

  defp explicit_type(original, query) do
    cond do
      Regex.match?(~r/\b(?:compare|versus|vs\.?|differences?|alternatives?)\b/iu, original) ->
        :comparison
      Regex.match?(~r/\b(?:fact[- ]check|verify|confirm|is\s+it\s+true|sources)\b/iu, original) ->
        :fact_check
      Regex.match?(~r/\b(?:research|study|papers?|clinical\s+trials?|scientific)\b/iu, original) ->
        :scientific_research
      Regex.match?(~r/\b(?:api|documentation|docs|github|programming|elixir|phoenix|linux|version|release notes)\b/iu, query) ->
        :technical_reference
      Regex.match?(~r/\b(?:history|historical|invented|founded)\b/iu, query) ->
        :historical_reference
      Regex.match?(~r/\b(?:near me|nearby|local|in my area|opening hours|open now|stores?|shops?|restaurants?|clinics?)\b/iu, query) ->
        :local_information
      true -> :general_search
    end
  end

  defp research_query(text) do
    cond do
      Regex.match?(~r/^\s*(?:what\s+does\s+.{3,80}\s+mean\s+in\s+(?:physics|biology|neuroscience|medicine|science)|what\s+is\s+(?:the\s+)?(?:scientific\s+)?research\s+(?:on|about)\s+.+|what\s+do\s+(?:studies|researchers)\s+say\s+about\s+.+)\??\s*$/iu, text) ->
        {text, :scientific_research}

      Regex.match?(~r/^\s*(?:who\s+invented\s+.+|when\s+(?:was|were|did)\s+.+\s+(?:invented|founded|discovered|built|begin|end)\??|(?:tell\s+me\s+about\s+)?the\s+history\s+of\s+.+)\s*$/iu, text) ->
        {text, :historical_reference}

      Regex.match?(~r/^\s*(?:what\s+(?:is|are)\s+(?:the\s+)?(?:differences?\s+between|advantages?\s+of)\s+.+|compare\s+.{3,}\s+(?:and|with|vs\.?)\s+.{3,})\??\s*$/iu, text) ->
        {text, :comparison}

      Regex.match?(~r/^\s*(?:where\s+can\s+i\s+find\s+(?:the\s+)?(?:official\s+)?(?:docs|documentation|api\s+reference)\s+.+|what\s+(?:changed|is\s+new)\s+in\s+.{3,}\s+version\s+[\w.]+|what\s+is\s+the\s+(?:latest|current)\s+(?:stable\s+)?version\s+of\s+.+)\??\s*$/iu, text) ->
        {text, :technical_reference}

      true ->
        public_topic_question(text)
    end
  end

  # Named public subjects often deserve reference lookup even when not
  # phrased as explicit web searches. Personal memory questions stay local.
  defp public_topic_question(text) do
    case Regex.run(
           ~r/^\s*(?:what\s+do\s+you\s+know\s+about|tell\s+me\s+about)\s+(.+?)\s*[?!.]*$/iu,
           text,
           capture: :all_but_first
         ) do
      [topic] ->
        topic = clean_query(topic)

        if Regex.match?(
             ~r/^(?:my|our|your|me|us|you|yourself|symbrella|this|that|it|these|those)\b/iu,
             topic
           ) or unresolved_reference?(topic) or topicless_news?(topic) do
          nil
        else
          case nonempty(topic) do
            nil -> nil
            value -> {value, :general_search}
          end
        end

      _ ->
        nil
    end
  end

  defp location_route(%{subject: subject, relation: :population}) when is_binary(subject),
    do: build("#{subject} population census", :population, :geographic_population, :symbolic_frame)

  defp location_route(%{subject: subject, target: target, relation: :proximity})
       when is_binary(subject) and is_binary(target),
       do: build("#{subject} #{target} geographic distance", :proximity,
         :geographic_proximity, :symbolic_frame)

  defp location_route(_), do: nil

  defp anaphora?(text) do
    Regex.match?(
      ~r/^\s*(?:any\s+(?:new\s+)?updates?\s+(?:on|about)\s+(?:it|that|this)|what(?:'s|\s+is)\s+(?:the\s+)?latest\s+(?:on|about)\s+(?:it|that|this)|can\s+you\s+(?:check|search|look\s+up)\s+(?:it|that|this)(?:\s+online)?|what\s+do\s+you\s+think\s+about\s+(?:those|these|the)\s+(?:developments|updates|reports)|what\s+are\s+your\s+thoughts\s+(?:on|about)\s+(?:those|these|the)\s+(?:developments|updates|reports)|what\s+do\s+you\s+make\s+of\s+(?:those|these|the)\s+(?:developments|updates|reports)|how\s+do\s+you\s+view\s+(?:those|these|the)\s+(?:developments|updates|reports))\s*[.!?]*$/iu,
      text
    )
  end

  defp previous_route(context, text) do
    case prior_topic(context) do
      nil -> nil
      prior ->
        if Regex.match?(
             ~r/\b(?:updates?|latest|new|developments|reports)\b/iu,
             text
           ) do
          build(prior, :current_events, :current_events, :followup)
        else
          build(prior, nil, :general_search, :followup)
        end
    end
  end

  defp prior_topic(context) do
    context
    |> Map.get(:retrieval_history, [])
    |> List.wrap()
    |> Enum.reverse()
    |> Enum.filter(fn message ->
      is_map(message) and
        (Map.get(message, "role") || Map.get(message, :role)) == "user"
    end)
    |> Enum.take(@max_prior_turns)
    |> Enum.map(fn message -> Map.get(message, "content") || Map.get(message, :content) end)
    |> Enum.find_value(fn prior ->
      if is_binary(prior) and substantive_topic?(prior) and
           not anaphora?(prior) do
        case explicit_request(prior) do
          {:explicit, query, _} when is_binary(query) and byte_size(query) >= 4 ->
            if unresolved_reference?(query) or topicless_news?(query),
              do: nil, else: nonempty(query)
          _ ->
            case current_events_query(prior) do
              nil -> clean_prior(prior)
              query -> query
            end
        end
      end
    end)
  end

  defp substantive_topic?(text) do
    words = Regex.scan(~r/[\p{L}\p{N}]+/u, text)

    length(words) >= 4 and
      String.length(text) <= 500 and
      not conversational_correction?(text) and
      not acknowledgement?(text) and
      not topicless_news?(text) and
      not Regex.match?(
        ~r/^\s*(?:hello|hi|yes|no|okay|ok|thanks|thank\s+you|you\s+are\s+wrong|try\s+again|search\s+(?:for\s+)?(?:it|that|this))\s*[.!?]*$/iu,
        text
      ) and
      not unresolved_reference?(text)
  end

  # A correction is conversational feedback, not a search subject.
  # Match the complete utterance so "It's not a typo: Japanese railgun ..."
  # can still supply an actual referent rather than being discarded.
  defp conversational_correction?(text) do
    Regex.match?(
      ~r/^\s*(?:(?:no|actually)[,!]?\s+)?(?:it|that|this)(?:(?:'s|\s+is)\s+not|\s+isn't)\s+(?:(?:a|an)\s+)?(?:typo|mistake|error)\s*[.!?]*$/iu,
      text
    ) or
      Regex.match?(
        ~r/^\s*(?:(?:no|actually)[,!]?\s+)?(?:you\s+(?:misunderstood|misheard|misread)\s+(?:me|it|that)|that(?:'s|\s+is)\s+not\s+what\s+i\s+meant|i\s+(?:already\s+)?said\s+that)\s*[.!?]*$/iu,
        text
      )
  end

  # Short social acknowledgements must not displace a substantive topic.
  defp acknowledgement?(text) do
    Regex.match?(
      ~r/^\s*(?:thanks(?:[,!]\s*)?(?:good\s+to\s+know|for\s+(?:that|the\s+info))?|thank\s+you(?:[,!]\s*)?(?:good\s+to\s+know)?|(?:ok|okay)[,!]?\s+(?:thanks|got\s+it)|good\s+to\s+know)\s*[.!?]*$/iu,
      text
    )
  end

  defp clean_prior(text) do
    text
    |> String.replace(~r/^\s*(?:(?:no|actually)[,!]\s*)?i\s+(?:mean|meant)\s+/iu, "")
    |> String.replace(~r/^\s*(?:(?:can|could|would)\s+you\s+)?(?:tell\s+me\s+about|what\s+do\s+you\s+(?:think|know)\s+about|what\s+are\s+your\s+thoughts\s+(?:on|about)|what\s+do\s+you\s+know\s+about)\s+/iu, "")
    |> String.replace(~r/[?!.]+$/u, "")
    |> nonempty()
  end

  defp unresolved_reference?(query) do
    Regex.match?(~r/^\s*(?:(?:for|about|on)\s+)?(?:it|this|that|them|those|the\s+same\s+thing)\s*[?!.]*$/iu, query)
  end

  defp build(query, relation, request_type, trigger) do
    case nonempty(query) do
      nil -> nil
      text -> %{query: text, relation: relation, request_type: request_type, trigger: trigger}
    end
  end

  defp clean_query(text) do
    text
    |> String.replace(~r/\s+(?:please|for\s+me)\s*[.!?]*$/iu, "")
    |> String.replace(~r/[?!.]+$/u, "")
    |> String.trim()
  end

  defp nonempty(query) when is_binary(query) do
    query =
      query
      |> String.replace(~r/[\p{Cc}\p{Cf}]/u, " ")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()
      |> String.slice(0, @max_query)

    if String.length(query) >= 4, do: query
  end

  defp nonempty(_), do: nil
end
