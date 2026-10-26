defmodule Core.Brain.WM.Comprehension do
  @moduledoc """
  Session-scoped, temporary conversational focus.

  A completed comprehension is eligible for WM as an *active context frame*,
  not a verified fact. This uses the canonical Brain.focus/2 admission path
  with permissive thresholds for this single candidate class only. Lexical
  candidates, grounded facts and recalled episodes keep their existing gates.

  Read the last same-session frame before discourse resolution; write the
  current frame after Core.Comprehension.Summary. No raw sentence is retained
  in the frame, and no cross-session fallback is permitted.
  """

  alias Core.Brain.Runtime
  alias Core.Pipeline.Trace
  alias Core.Semantic.EpisodicProjection
  alias Core.Semantic.ProvisionalInterpretation

  @max_age_ms 20 * 60 * 1_000
  @max_entities 4
  @max_terms 6

  @doc """
  Return only the currently admitted, unexpired comprehension context for
  the given session. This is an attention frame, not factual authority.
  """
  @spec admitted_context(term()) :: map() | nil
  def admitted_context(session) when is_binary(session) and session != "" do
    case find_frame(session) do
      %{context: %{} = context} -> context
      _ -> nil
    end
  end

  def admitted_context(_), do: nil

  @spec recall(map(), keyword()) :: map()
  def recall(si, opts \\ [])

  def recall(%{} = si, opts) when is_list(opts) do
    with true <- enabled?(opts),
         session when not is_nil(session) <- field(si, :session_id),
         %{} = item <- find_frame(session),
         %{} = context <- field(item, :context) do
      si
      |> Map.put(:wm_context, context)
      |> maybe_bind_plural_reference(context)
      |> Trace.append(:wm_context,
        decision: :recalled,
        reason: :recent_same_session_comprehension,
        meta: %{entity_count: length(List.wrap(context[:entities])), source: :comprehension}
      )
    else
      _ -> si
    end
  end

  def recall(si, _opts), do: si

  @spec store(map(), keyword()) :: map()
  def store(si, opts \\ [])

  def store(%{} = si, opts) when is_list(opts) do
    with true <- enabled?(opts),
         session when not is_nil(session) <- field(si, :session_id),
         %{} = comprehension <- field(si, :comprehension),
         sentence when is_binary(sentence) and byte_size(sentence) > 0 <- field(si, :sentence) do
      context = context_from(si, comprehension)
      id = frame_id(session)

      # The already implemented salience/BasalGanglia path receives a bounded,
      # same-session attention context for this admission only.
      goal_terms = attention_terms(context)
      attention = %{goal_terms: goal_terms}
      focus_label = List.first(goal_terms) || "current comprehension"

      salience =
        Brain.Attention.salience(
          %{id: id, lemma: focus_label, source: :comprehension},
          attention
        )

      context =
        Map.put(context, :attention, %{
          scope: :session,
          goal_terms: goal_terms,
          salience: salience,
          transition: context.focus_transition,
          verification: :conversation_context_only
        })

      # :score here is focus priority, NEVER factual confidence. Keep lexical
      # :lifg_min_score and generic evidence floors unchanged. Only this
      # session's temporary comprehension frame gets a permissive gate.
      candidate = %{
        id: id,
        lemma: focus_label,
        source: :comprehension,
        score: 0.6,
        reason: :current_conversation,
        context: context,
        kind: :comprehension_frame
      }

      result =
        safe_brain_call(:focus, [
          [candidate],
          [
            attention: attention,
            gate_threshold: 0.0,
            boost_threshold: 0.0,
            boost_threshold_pref: 0.0,
            block_threshold: 0.0,
            evidence_floor: 0.0
          ]
        ])

      admitted? =
        is_list(result) and
          Enum.any?(result, &(field(&1, :id) == id))

      :telemetry.execute(
        [:core, :wm, :attention],
        %{salience: salience, goals: length(goal_terms)},
        %{
          scope: :session,
          transition: context.focus_transition,
          decision: if(admitted?, do: :admitted, else: :unavailable),
          factual_authority?: false
        }
      )

      :telemetry.execute(
        [:core, :wm, :comprehension],
        %{candidates: 1, admitted: if(admitted?, do: 1, else: 0)},
        %{decision: if(admitted?, do: :admitted, else: :unavailable), source: :comprehension}
      )

      si
      |> Map.put(:wm_context, context)
      |> Trace.append(:wm_comprehension,
        decision: if(admitted?, do: :admitted, else: :unavailable),
        reason: :completed_comprehension_focus,
        meta: %{
          candidates: 1,
          admitted: if(admitted?, do: 1, else: 0),
          entity_count: length(context.entities),
          attention_salience: salience,
          focus_transition: context.focus_transition,
          degraded?: context.degraded?,
          fact_verified?: false
        }
      )
    else
      _ -> si
    end
  end

  def store(si, _opts), do: si

  @doc "Build a small frame from current comprehension and independently identified event roles."
  @spec context_from(map(), map()) :: map()
  def context_from(si, comprehension) when is_map(si) and is_map(comprehension) do
    event = map_or_empty(field(si, :symbolic_frame))
    projected = EpisodicProjection.validated_participant(si)
    roles = if is_map(projected), do: field(projected, :participants), else: nil
    event_roles = bounded_roles(roles)
    prev = map_or_empty(field(si, :wm_context))
    reference = map_or_empty(field(si, :discourse_reference))
    sentence = field(si, :sentence) || ""
    provisional =
      case ProvisionalInterpretation.verified(si) do
        {:ok, frame} -> frame
        _ -> nil
      end
    provisional_topic = get_in(provisional || %{}, [:target, :lemma])
    provisional_reference = provisional_context(provisional, prev, field(si, :session_id))
    year = followup_year(sentence)
    previous_topic = field(prev, :topic)
    previous_subtopic = field(prev, :subtopic)
    continuation? =
      is_integer(year) or conversational_continuation?(sentence, previous_subtopic)
    explicit_topic = if continuation?, do: nil, else: public_topic(sentence)
    inherited_topic = if continuation?, do: previous_topic, else: nil
    topic = explicit_topic || inherited_topic || provisional_topic

    # A conversational subtopic refines, but does not replace, the established
    # same-session subject. Explicit switches clear both inherited focus and
    # news recency; neither is promoted to factual evidence.
    subtopic =
      if is_binary(inherited_topic) do
        followup_subtopic(sentence) || previous_subtopic
      end

    current_events? =
      cond do
        is_binary(explicit_topic) -> current_event_request?(sentence)
        is_binary(inherited_topic) -> field(prev, :current_events?) == true
        true -> false
      end

    focus_transition =
      cond do
        is_binary(explicit_topic) and is_binary(field(prev, :topic)) and
            explicit_topic != field(prev, :topic) -> :shift
        is_binary(explicit_topic) -> :select
        is_binary(inherited_topic) -> :sustain
        is_binary(field(prev, :topic)) -> :release
        true -> :none
      end

    explicit_entities =
      ([field(event, :subject), field(event, :target)] ++
         List.wrap(field(event, :subjects)))
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&clean_entity/1)
      |> Enum.reject(&(&1 == "" or pronoun?(&1)))
      |> Enum.uniq()
      |> Enum.take(@max_entities)

    # Grammatical event projection already ran before the comprehension/WM
    # stages. Preserve its *validated roles* as active conversational context
    # when the independent symbolic frame has no explicit participants.
    # This is not a promotion to independently verified factual knowledge.
    projected_entities =
      ["actor", "patient", "location"]
      |> Enum.map(&Map.get(event_roles, &1))
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&clean_entity/1)
      |> Enum.reject(&(&1 == "" or pronoun?(&1)))
      |> Enum.uniq()
      |> Enum.take(@max_entities)

    current_entities =
      if explicit_entities == [], do: projected_entities, else: explicit_entities

    inherited =
      if contextual_reference?(sentence, reference) and current_entities == [] do
        List.wrap(field(prev, :entities))
        |> Enum.filter(&is_binary/1)
        |> Enum.take(@max_entities)
      else
        []
      end

    entities = if current_entities == [], do: inherited, else: current_entities

    relation =
      field(event, :relation) || field(projected, :relation) ||
        if(inherited != [] and Regex.match?(~r/\bpopulations?\b/iu, sentence),
          do: :population, else: nil)

    %{
      schema: 1,
      topic: topic,
      subtopic: subtopic,
      current_events?: current_events?,
      focus_transition: focus_transition,
      topic_source:
        cond do
          is_binary(explicit_topic) -> :current_user_request
          is_binary(inherited_topic) -> :same_session_wm
          is_binary(provisional_topic) -> :provisional_construction
          true -> nil
        end,
      temporal_reference:
        if(is_integer(year), do: %{year: year, relation: year_relation(year)}, else: nil),
      intent: field(si, :intent),
      entities: entities,
      relation: relation,
      requested_relation: field(provisional_reference, :requested_relation),
      provisional_interpretation: provisional_reference,
      event_roles: event_roles,
      event_source_frame: field(projected, :source_frame),
      event_role_status: if(is_map(projected), do: :validated_projection, else: :none),
      recognized_topics: terms(field(comprehension, :recognized_topics)),
      understood: terms(field(comprehension, :understood)),
      uncertain: terms(field(comprehension, :uncertain)),
      degraded?: field(comprehension, :degraded?) == true,
      interpretation_status:
        case field(map_or_empty(field(si, :sentence_meaning)), :status) do
          status when status in [nil, :not_yet_interpreted] and is_map(provisional) ->
            :provisional
          status -> status
        end,
      verification: :conversation_context_only,
      updated_at_ms: System.system_time(:millisecond)
    }
  end

  # These prior-turn matches remain candidates, not resolved references or facts.
  # Real pipeline recall is session-keyed before this method runs.
  defp provisional_context(%{status: :provisional} = frame, prev, session)
       when is_binary(session) and session != "" do
    lemma = frame.target.lemma
    candidates =
      prev
      |> field(:entities)
      |> List.wrap()
      |> Enum.filter(&is_binary/1)
      |> Enum.filter(&(referent_head(&1) == lemma))
      |> Enum.uniq()
      |> Enum.take(@max_entities)

    %{
      status: :provisional, operation: frame.operation,
      speech_act: frame.speech_act, target: frame.target,
      requested_relation: frame.requested_relation,
      referent_status: if(candidates == [], do: :unresolved, else: :candidate_same_session),
      antecedent_candidates: candidates, evidence: frame.evidence,
      missing: frame.missing, factual_authority?: false,
      verification: :conversation_context_only
    }
  end

  defp provisional_context(%{status: :provisional} = frame, _prev, _session) do
    %{
      status: :provisional, operation: frame.operation,
      speech_act: frame.speech_act, target: frame.target,
      requested_relation: frame.requested_relation,
      referent_status: :unresolved, antecedent_candidates: [],
      evidence: frame.evidence, missing: frame.missing,
      factual_authority?: false, verification: :conversation_context_only
    }
  end
  defp provisional_context(_, _, _), do: nil

  defp referent_head(entity) do
    entity
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\A(?:a|an|the)\s+/u, "")
  end

  # Conversational subject only, not independently verified evidence.
  defp public_topic(sentence) when is_binary(sentence) do
    switch =
      ~r/^\s*(?:let(?:'s|\s+us)|can\s+we|we\s+should)\s+(?:talk|speak|chat|discuss)\s+(?:about\s+)?(.+?)\s+(?:instead|now)\s*[?!.]*\s*$/iu

    pattern =
      ~r/^\s*(?:(?:can|could|would)\s+you\s+(?:please\s+)?give\s+me\s+(?:(?:the|te|some)\s+)?(?:details?|detils|information|info)\s+(?:of|on|about)|(?:what\s+)?(?:can|could|would)\s+you\s+(?:please\s+)?tell\s+me{1,3}\s+about|tell\s+me{1,3}\s+about|what\s+do\s+you\s+know\s+about|what\s+(?:are|is)\s+your\s+(?:thoughts?|take)\s+(?:on|about))\s+(.+?)\s*[?!.]*$/iu

    matched =
      Regex.run(switch, sentence, capture: :all_but_first) ||
        Regex.run(pattern, sentence, capture: :all_but_first)

    case matched do
      [subject] ->
        subject =
          subject
          |> String.trim()
          |> String.replace(
            ~r/\s+(?:as\s+of\s+(?:current(?:ly)?|now|today|this\s+(?:week|month|year))|currently|right\s+now)\s*$/iu,
            ""
          )
          |> String.trim()
          |> String.slice(0, 120)
        if subject == "" or
             Regex.match?(~r/^(?:it|that|this|them|those|my|our|your)\b/iu, subject),
          do: nil, else: subject
      _ -> nil
    end
  end

  defp public_topic(_), do: nil

  # Follow-ups are only allowed to retain a subject explicitly stored in
  # the same session. A definite plural reference additionally requires a
  # matching, previously introduced subtopic.
  defp conversational_continuation?(sentence, previous_subtopic)
       when is_binary(sentence) do
    Regex.match?(
      ~r/^\s*(?:(?:and|so)\s+)?(?:how|what)\s+about\b|^\s*(?:tell me more|go on|continue|why\??|okay|ok|yes|right|i see|interesting)\s*[.!?]*\s*$/iu,
      sentence
    ) or referenced_subtopic?(sentence, previous_subtopic)
  end

  defp conversational_continuation?(_, _), do: false

  defp referenced_subtopic?(sentence, previous_subtopic)
       when is_binary(sentence) and is_binary(previous_subtopic) do
    case Regex.run(
           ~r/^\s*(?:tell\s+me\s+more\s+about|more\s+(?:about|on))\s+(?:those|these|the)\s+([\p{L}\p{N}][\p{L}\p{N}\s-]*?)\s*[?!.]*\s*$/iu,
           sentence, capture: :all_but_first
         ) do
      [referent] ->
        previous = String.downcase(previous_subtopic)
        referent = String.downcase(String.trim(referent))
        referent != "" and
          Regex.match?(~r/(?:^|\W)#{Regex.escape(referent)}(?:\W|$)/iu, previous)

      _ ->
        false
    end
  end

  defp referenced_subtopic?(_, _), do: false

  defp followup_subtopic(sentence) when is_binary(sentence) do
    case Regex.run(
           ~r/^\s*(?:(?:and|so)\s+)?(?:how|what)\s+about\s+(.+?)\s*[?!.]*\s*$/iu,
           sentence, capture: :all_but_first
         ) do
      [subject] ->
        subject = String.trim(subject)

        if String.length(subject) <= 96 and
             not Regex.match?(~r/^(?:in\s+)?(?:19|20)\d{2}$|^(?:it|that|this|them|those|my|our|your)\b/iu, subject),
          do: subject,
          else: nil

      _ ->
        nil
    end
  end

  defp followup_subtopic(_), do: nil

  defp current_event_request?(sentence) when is_binary(sentence) do
    is_binary(Core.Tools.WebRetrieval.RequestRouter.public_polling_query(sentence)) or
      (Regex.match?(
         ~r/\b(?:war|conflict|ceasefire|attacks?|fighting|elections?|news|developments?|negotiations?)\b/iu,
         sentence
       ) and
         Regex.match?(
           ~r/\b(?:current(?:ly)?|latest|recent(?:ly)?|today|tonight|ongoing|now|this\s+week|as\s+of\s+(?:current|now|today))\b/iu,
           sentence
         ))
  end

  defp current_event_request?(_), do: false

  defp attention_terms(context) do
    terms =
      case context.topic do
        topic when is_binary(topic) and topic != "" -> [topic, context.subtopic]
        _ -> context.entities
      end

    terms
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.take(@max_entities)
  end

  defp followup_year(sentence) when is_binary(sentence) do
    case Regex.run(
           ~r/^\s*(?:(?:and|so)\s+)?(?:how|what)\s+about\s+(?:in\s+)?((?:19|20)\d{2})\s*[?.!]*\s*$/iu,
           sentence, capture: :all_but_first) do
      [year] -> String.to_integer(year)
      _ -> nil
    end
  end

  defp followup_year(_), do: nil

  defp year_relation(year) do
    cond do
      year < Date.utc_today().year -> :past
      year > Date.utc_today().year -> :future
      true -> :current
    end
  end

  @event_role_keys ~w(actor predicate surface_predicate patient position location aspect tense)

  defp bounded_roles(%{} = participants) do
    participants
    |> Map.take(@event_role_keys)
    |> Enum.flat_map(fn {key, value} ->
      if is_binary(value) and byte_size(value) <= 96 and
           String.trim(value) != "" do
        [{key, String.trim(value)}]
      else
        []
      end
    end)
    |> Map.new()
  end

  defp bounded_roles(_), do: %{}

  defp maybe_bind_plural_reference(si, %{entities: entities} = _context) when is_list(entities) do
    sentence = field(si, :sentence)

    cond do
      not is_binary(sentence) or field(si, :discourse_reference) != nil ->
        si

      length(entities) >= 2 and
          contextual_plural_cue?(sentence) ->
        reference_phrase =
          cond do
            Regex.match?(~r/\bboth\b/iu, sentence) -> "both"
            Regex.match?(~r/\beach\b/iu, sentence) -> "each"
            Regex.match?(~r/\b(?:those|these)\s+(?:cities|places)\b/iu, sentence) ->
              "those cities"
            true -> "which one"
          end

        Map.put(si, :discourse_reference, %{
          pronoun: reference_phrase,
          antecedents: Enum.take(entities, @max_entities),
          source: :wm_comprehension,
          evidence: :prior_same_session_comprehension,
          verification: :conversation_context_only
        })

      true -> si
    end
  end

  defp maybe_bind_plural_reference(si, _), do: si

  defp contextual_reference?(sentence, reference) do
    is_binary(sentence) and contextual_plural_cue?(sentence) and
      field(reference, :source) == :wm_comprehension
  end

  # Only unambiguous references to a previously active group are inherited.
  # Unrelated turns must not keep the prior topic alive indefinitely.
  defp contextual_plural_cue?(sentence) when is_binary(sentence) do
    Regex.match?(~r/\b(?:each|both)\b/iu, sentence) or
      Regex.match?(~r/\b(?:those|these)\s+(?:cities|places)\b/iu, sentence) or
      Regex.match?(
        ~r/^\s*which\s+(?:one|city)\s+has\s+the\s+(?:larger|greater|higher)\s+population\b/iu,
        sentence
      )
  end

  defp contextual_plural_cue?(_), do: false

  defp find_frame(session) do
    case safe_brain_call(:snapshot_wm, []) do
      %{wm: wm} when is_list(wm) ->
        id = frame_id(session)
        now = System.system_time(:millisecond)

        Enum.find_value(wm, fn item ->
          ts = field(item, :last_bump) || field(item, :ts)
          payload = field(item, :payload)

          if field(item, :id) == id and is_map(payload) and
               field(payload, :kind) == :comprehension_frame and
               is_integer(ts) and now >= ts and now - ts <= @max_age_ms do
            payload
          end
        end)

      _ ->
        nil
    end
  end

  @doc "Deterministic opaque WM identity for one conversation session."
  def frame_id(session) when not is_nil(session) do
    digest =
      :crypto.hash(:sha256, :erlang.term_to_binary({:symbrella_wm_session, session}))
      |> Base.encode16(case: :lower)
      |> binary_part(0, 32)

    "comprehension:#{digest}"
  end

  defp safe_brain_call(function, args) do
    if Runtime.pid_alive?(Brain) do
      Runtime.apply_if_exported(Brain, function, args, nil)
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  defp enabled?(opts) do
    config = Application.get_env(:core, __MODULE__, [])
    configured? = if is_list(config), do: Keyword.get(config, :enabled?, true), else: true
    Keyword.get(opts, :comprehension_wm?, configured?) == true
  end

  defp terms(value) do
    value
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.slice(String.trim(&1), 0, 96))
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.take(@max_terms)
  end

  defp clean_entity(text) do
    text
    |> String.trim()
    |> String.slice(0, 96)
    |> String.replace(~r/\s+/u, " ")
  end

  defp pronoun?(text),
    do: String.downcase(text) in ~w(it this that each both there they them its)

  defp map_or_empty(%{} = value), do: value
  defp map_or_empty(_), do: %{}

  defp field(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp field(_, _), do: nil
end
