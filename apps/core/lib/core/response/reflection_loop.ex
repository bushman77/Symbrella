defmodule Core.Response.ReflectionLoop do
  @moduledoc """
  Bounded post-draft response review.

  This module treats reflection as an inspectable control pass, not hidden
  chain-of-thought. It reviews an LLM draft against Core's symbolic context and
  returns final text plus a compact reflection record that can be stored on the
  turn.
  """

  alias Core.Telemetry
  alias Core.Response.Topics
  alias Core.Tools.WebRetrieval.Grounding

  @event [:core, :response, :reflection]
  @v 1
  @max_text_chars 1_200
  @hidden_context_line_regex ~r/^\s*(uncertain|temperament|assertiveness|curiosity|restraint|warmth|self-check|abstraction|depth|reasons|personality state|runtime decision|response policy|response posture|semantic control|affect state)\s*:/iu
  @hidden_context_token_regex ~r/\b(system prompt|hidden instruction|chain of thought|personality state|runtime decision|response policy|response posture|semantic control|internal state|balanced policy|offer options to clarify your intent|lifg|pmtg|hippocampus|working memory|self-state|temperament|assertiveness|self-check|abstraction|neuromodulators?|da\s+\d|5ht\s+\d|glu\s+\d|ne\s+\d)\b/iu

  @type status :: :accept | :repair | :clarify | :reject

  @type reflection :: %{
          required(:v) => pos_integer(),
          required(:status) => status(),
          required(:confidence) => float(),
          required(:issues) => [atom()],
          required(:critique) => String.t(),
          required(:repair_instruction) => String.t() | nil,
          required(:applied?) => boolean(),
          required(:repair_count) => 0 | 1,
          required(:draft_sha256) => String.t(),
          required(:final_sha256) => String.t(),
          required(:draft_text) => String.t(),
          required(:final_text) => String.t(),
          required(:at_ms) => integer()
        }

  @spec review(String.t(), String.t(), map()) :: {:ok, String.t(), reflection()}
  def review(user_text, draft_text, context) when is_binary(draft_text) and is_map(context) do
    user_text = to_string(user_text || "")
    draft_text = String.trim(draft_text)

    {status, issues} =
      context
      |> collect_issues(user_text, draft_text)
      |> classify()

    {final_text, repair_instruction, repair_count} =
      apply_decision(status, user_text, draft_text, issues, context)

    reflection =
      %{
        v: @v,
        status: status,
        confidence: confidence(status, issues),
        issues: issues,
        critique: critique(status, issues),
        repair_instruction: repair_instruction,
        applied?: final_text != draft_text,
        repair_count: repair_count,
        draft_sha256: sha256_hex(draft_text),
        final_sha256: sha256_hex(final_text),
        draft_text: clamp_text(draft_text),
        final_text: clamp_text(final_text),
        at_ms: System.system_time(:millisecond)
      }

    emit(reflection, context)
    {:ok, final_text, reflection}
  end

  def review(_user_text, draft_text, _context),
    do: {:ok, to_string(draft_text || ""), no_op(draft_text)}

  @spec needed?(map(), String.t()) :: boolean()
  def needed?(context, draft_text) when is_map(context) and is_binary(draft_text) do
    context
    |> collect_issues(get_in_map(context, [:features, :text]) || "", draft_text)
    |> Enum.any?()
  end

  def needed?(_context, _draft_text), do: false

  defp collect_issues(context, user_text, draft_text) do
    []
    |> maybe_issue(unsafe_draft?(draft_text), :unsafe)
    |> maybe_issue(unfounded_device_refusal?(context, draft_text), :unsupported_device_refusal)
    |> maybe_issue(news_access_refusal?(context, draft_text), :news_access_refusal)
    |> maybe_issue(news_lookup_misrepresented?(context, draft_text), :news_lookup_misrepresented)
    |> maybe_issue(news_evidence_not_cited?(context, draft_text), :news_evidence_not_cited)
    |> maybe_issue(news_assessment_missing?(context, user_text, draft_text), :news_assessment_missing)
    |> maybe_issue(unfulfilled_search_promise?(context, draft_text), :search_not_answered)
    |> maybe_issue(named_reference_missing?(context), :named_reference_missing)
    |> maybe_issue(uncited_reference_answer?(context, draft_text), :reference_uncited)
    |> maybe_issue(subject_reversal?(context, draft_text), :subject_reversal)
    |> maybe_issue(unverified_geographic_claim?(context, draft_text), :geographic_claim_unverified)
    |> maybe_issue(low_confidence?(context), :low_confidence)
    |> maybe_issue(unknown_intent?(context), :unknown_intent)
    |> maybe_issue(high_uncertainty?(context), :high_uncertainty)
    |> maybe_issue(self_state_repair?(context), :self_state_repair)
    |> maybe_issue(self_state_question?(user_text), :self_state_claim_risk)
    |> maybe_issue(
      self_state_question?(user_text) and robotic_self_state_draft?(draft_text),
      :robotic_self_state
    )
    |> maybe_issue(overclaims_self_awareness?(draft_text), :overclaimed_self_awareness)
    |> maybe_issue(generic_draft?(draft_text), :too_generic)
    |> maybe_issue(topic_dead_end?(context, user_text, draft_text), :topic_dead_end)
    |> maybe_issue(hidden_context_leak?(draft_text), :leaked_hidden_context)
    |> Enum.reverse()
    |> Enum.uniq()
  end

  defp classify(issues) do
    cond do
      :unsafe in issues ->
        {:reject, issues}

      :unsupported_device_refusal in issues ->
        {:repair, issues}

      :news_access_refusal in issues or :news_evidence_not_cited in issues or
          :news_assessment_missing in issues or :news_lookup_misrepresented in issues ->
        {:repair, issues}

      :search_not_answered in issues or :named_reference_missing in issues or
          :reference_uncited in issues ->
        {:repair, issues}

      :subject_reversal in issues ->
        {:repair, issues}

      :geographic_claim_unverified in issues ->
        {:repair, issues}

      :leaked_hidden_context in issues or :overclaimed_self_awareness in issues ->
        {:repair, issues}

      :topic_dead_end in issues ->
        {:repair, issues}

      :robotic_self_state in issues ->
        {:repair, issues}

      :low_confidence in issues and (:unknown_intent in issues or :too_generic in issues) ->
        {:clarify, issues}

      :high_uncertainty in issues and :too_generic in issues ->
        {:clarify, issues}

      :self_state_repair in issues ->
        {:repair, issues}

      true ->
        {:accept, issues}
    end
  end

  defp apply_decision(:accept, _user_text, draft_text, _issues, _context),
    do: {draft_text, nil, 0}

  defp apply_decision(:clarify, user_text, _draft_text, _issues, _context) do
    final_text = clarification_text(user_text)

    {final_text,
     "Replace broad draft with one targeted clarification because the symbolic state is uncertain.",
     1}
  end

  defp apply_decision(:repair, user_text, draft_text, issues, context) do
    final_text =
      draft_text
      |> repair_overclaims()
      |> repair_hidden_context_leak()
      |> String.trim()

    final_text =
      cond do
        :unsupported_device_refusal in issues ->
          acknowledge_device_command(context)

        :news_lookup_misrepresented in issues ->
          empty_news_lookup_fallback(context)

        Enum.any?(issues, &(&1 in [:news_access_refusal, :news_evidence_not_cited, :news_assessment_missing])) ->
          news_headline_fallback(context, user_text)

        :search_not_answered in issues ->
          reference_excerpt_fallback(context)

        :named_reference_missing in issues ->
          named_reference_missing_fallback(context)

        :reference_uncited in issues ->
          reference_excerpt_fallback(context)

        :subject_reversal in issues ->
          action = get_in_map(context, [:symbolic_frame, :event, :action])
          "Understood, you need to #{action}."

        :geographic_claim_unverified in issues ->
          case Grounding.align(draft_text, context) do
            ^draft_text -> unverified_geographic_reply(context)
            sourced -> sourced
          end

        :robotic_self_state in issues ->
          self_state_checkin_fallback(user_text)

        :leaked_hidden_context in issues and (final_text == "" or final_text == draft_text) ->
          hidden_context_fallback(user_text)

        :topic_dead_end in issues and alien_thread_context?(context) ->
          alien_thread_fallback(user_text)

        final_text == "" or (final_text == draft_text and :self_state_repair in issues) ->
          "I should slow down and ground this in the available evidence before going further."

        true ->
          final_text
      end

    instruction =
      cond do
        :unsupported_device_refusal in issues ->
          "Replace ungrounded generic refusal with a verified-command acknowledgement; never assert that a device was controlled or its state observed."
        Enum.any?(issues, &(&1 in [:news_access_refusal, :news_evidence_not_cited, :news_assessment_missing])) ->
          "Recent headlines require accurate publication dates, outlet attribution, source links and—when requested—a separate qualified assessment. Repair the draft from the retrieved headlines without inventing verified article contents."
        :search_not_answered in issues ->
          "Retrieved reference excerpts are already available. Attribute and cite them, rather than promising to search later; do not pretend snippets prove the claims."
        :news_lookup_misrepresented in issues ->
          "A dated-news lookup was attempted but did not yield qualifying headlines. State the search outcome honestly; do not claim Symbrella lacks web-search capabilities or invent reports."
        :named_reference_missing in issues ->
          "No relevant reference excerpts were retained for the named public topic. Do not substitute background information as evidence of the specific programme."
        :reference_uncited in issues ->
          "The answer lacks any URL from retrieved reference evidence. Give only attributed excerpts with URLs; do not invent a factual or institutional history."
        :geographic_claim_unverified in issues ->
          "Use the claim-specific sourced evidence (population or coordinate-based straight-line distance), with citations; otherwise state the fact is unverified."
        true ->
          "Repair unsupported self-claims or hidden-control wording before publishing."
      end

    {final_text, instruction, 1}
  end

  defp apply_decision(:reject, _user_text, _draft_text, _issues, _context) do
    {"I need to stop and ask for a clearer, safer target before answering.",
     "Reject unsafe draft.", 1}
  end

  # The model must not defer a search that Core already executed this turn.
  # Restrict to a short, pure search promise with actual retrieved reference
  # excerpts, never infer support or hallucinate fresh sources.
  defp unfulfilled_search_promise?(context, draft_text) when is_binary(draft_text) do
    evidence = Map.get(context, :web_evidence, %{})

    is_map(evidence) and
      Map.get(evidence, :resolution) == :reference_sources and
      Map.get(evidence, :status) == :retrieved and
      is_list(Map.get(evidence, :sources)) and
      Map.get(evidence, :sources) != [] and
      String.length(draft_text) <= 400 and
      (Regex.match?(
         ~r/\b(?:i(?:'ll|\s+will|\s+am\s+going\s+to)|let\s+me|i\s+can)\s+(?:search|look\s+(?:it|that)\s+up|check\s+(?:that|it)\s+online|find\s+(?:sources|information)|do\s+(?:a\s+)?search)\b/iu,
         draft_text
       ) or
         Regex.match?(
           ~r/\b(?:don't\s+have\s+enough\s+(?:context|information)|do\s+not\s+have\s+enough\s+(?:context|information)|not\s+enough\s+(?:context|information)|need\s+more\s+(?:context|details)|could\s+you\s+(?:please\s+)?(?:provide\s+more\s+(?:details|context)|clarify)|can't\s+provide\s+(?:a\s+)?complete\s+answer)\b/iu,
           draft_text
         ))
  end

  defp unfulfilled_search_promise?(_, _), do: false

  defp reference_excerpt_fallback(context) do
    sources = get_in_map(context, [:web_evidence, :sources]) || []

    excerpts =
      sources
      |> Enum.take(2)
      |> Enum.filter(fn source ->
        is_map(source) and is_binary(Map.get(source, :url)) and
          is_binary(Map.get(source, :title)) and is_binary(Map.get(source, :excerpt))
      end)
      |> Enum.map_join("\n", fn source ->
        "- #{String.slice(source.title, 0, 120)}: " <>
          "#{String.slice(source.excerpt, 0, 320)} " <>
          "Source: #{String.slice(source.url, 0, 400)}"
      end)

    "I retrieved these reference excerpts, but they are not independently verified facts:\n" <>
      excerpts <> "\nThe excerpts alone may not fully answer the question."
  end

  # The small model may repeat a stale "I have no live news access" message
  # even after Core supplied dated headlines. Repair only when the current
  # turn actually has fresh, sourced news evidence.
  defp news_access_refusal?(context, draft_text) when is_binary(draft_text) do
    evidence = Map.get(context, :web_evidence, %{})

    Map.get(evidence, :resolution) == :headlines_available and
      Map.get(evidence, :status) == :retrieved and
      is_list(Map.get(evidence, :sources)) and
      Map.get(evidence, :sources) != [] and
      Regex.match?(
        ~r/\b(?:don't|do not|can't|cannot|lack|without|no)\s+(?:have\s+)?(?:live|real[- ]time|current|up[- ]to[- ]date)\s+(?:news|updates|headlines|news access)\b/iu,
        draft_text
      )
  end

  defp news_access_refusal?(_, _), do: false

  # A public reference answer without any retained source URL is not a
  # source-grounded response. This does not independently verify the
  # cited claims, but it prevents unsupported factual narration from being
  # silently accepted as retrieval-backed.
  defp uncited_reference_answer?(context, draft_text) when is_binary(draft_text) do
    case Map.get(context, :web_evidence) do
      %{resolution: :reference_sources, status: :retrieved,
        sources: sources, request_type: request_type}
      when request_type in [
             :general_search, :historical_reference, :technical_reference,
             :scientific_research, :fact_check, :comparison, :local_information
           ] and is_list(sources) and sources != [] ->
        not Enum.any?(sources, fn source ->
          url = Map.get(source, :url)
          is_binary(url) and url != "" and String.contains?(draft_text, url)
        end)

      _ ->
        false
    end
  end

  defp uncited_reference_answer?(_, _), do: false

  # If references were rejected for a qualified named subject, don't
  # substitute generic background material and imply it answers the named
  # programme question. Other query categories remain unchanged.
  defp named_reference_missing?(context) do
    case Map.get(context, :web_evidence) do
      %{request_type: :general_search, relation: nil,
        resolution: :exhausted, sources: [], query: query,
        attempted_count: attempts}
      when is_binary(query) and is_integer(attempts) and attempts > 0 ->
        Regex.match?(~r/\b[A-Z][\p{L}-]{2,}['’]s\b/u, query)

      _ ->
        false
    end
  end

  defp named_reference_missing_fallback(context) do
    query =
      context
      |> get_in_map([:web_evidence, :query])
      |> to_string()
      |> String.slice(0, 160)

    "I looked for reference material specifically about #{query}, but the available results " <>
      "did not establish that subject. I can discuss the underlying technology, " <>
      "but I can't substantiate the named programme from those results."
  end

  # An empty search is not the same thing as no search capability. Only
  # correct demonstrably false capability claims when an actual dated-news
  # search has been attempted; never claim that unavailable sources were read.
  defp news_lookup_misrepresented?(context, draft_text) when is_binary(draft_text) do
    case Map.get(context, :web_evidence) do
      %{relation: :current_events, resolution: :exhausted, sources: [],
        attempted_count: attempts} when is_integer(attempts) and attempts > 0 ->
        generic_refusal?(draft_text) or
          Regex.match?(
            ~r/\b(?:don'?t|do\s+not|can'?t|cannot|no\s+longer|unable\s+to)\b.{0,70}\b(?:capability|ability|access|browse|search)\b.{0,70}\b(?:web|internet|online|news|browse|search)|\b(?:no|without)\s+(?:web|internet|live\s+news)\s+access\b/iu,
            draft_text
          )

      _ ->
        false
    end
  end

  defp news_lookup_misrepresented?(_, _), do: false

  defp empty_news_lookup_fallback(context) do
    evidence = Map.get(context, :web_evidence, %{})
    query = evidence |> Map.get(:query, "the requested subject") |> to_string() |> String.slice(0, 160)

    case Map.get(evidence, :status) do
      :unavailable ->
        "I tried to retrieve dated news about #{query}, but the news provider was unavailable or failed. " <>
          "I can't verify recent developments from that attempt."

      _ ->
        "I searched for dated news about #{query}, but no qualifying headlines were returned. " <>
          "That doesn't establish that nothing has happened; I can't verify recent developments from this search."
    end
  end

  # A headline is evidence of what an outlet reported, not proof that the
  # underlying event has been independently confirmed. For news answers we
  # require at least one matched triple: publisher, publication day and URL
  # all attached to the SAME retained source. Arbitrary citations do not pass.
  defp news_evidence_not_cited?(context, draft_text) do
    case news_sources(context) do
      [] ->
        false

      sources ->
        not Enum.any?(sources, fn source ->
          url = Map.get(source, :url, "")
          publisher = Map.get(source, :publisher, "")
          published = Map.get(source, :published_at, "")
          date = if is_binary(published), do: String.slice(published, 0, 10), else: ""

          is_binary(url) and url != "" and
            is_binary(publisher) and publisher != "" and
            byte_size(date) == 10 and
            is_binary(Map.get(source, :title)) and
            String.contains?(draft_text, url) and
            String.contains?(String.downcase(draft_text), String.downcase(publisher)) and
            String.contains?(draft_text, date) and
            String.contains?(
              String.downcase(draft_text),
              String.downcase(Map.get(source, :title))
            )
        end)
    end
  end

  defp news_assessment_missing?(context, user_text, draft_text) do
    news_sources(context) != [] and
      Regex.match?(
        ~r/\b(?:your thoughts|what do you think|your take|your opinion|how do you view|what's your view)\b/iu,
        user_text
      ) and
      not Regex.match?(
        ~r/\b(?:my assessment|my view|i think|in my view|my concern|a key concern|key risks?|major risks?|the implications|this raises|i would focus|the central issue)\b/iu,
        draft_text
      )
  end

  defp news_sources(context) do
    case Map.get(context, :web_evidence, %{}) do
      %{relation: :current_events, status: :retrieved,
        resolution: :headlines_available, sources: sources}
      when is_list(sources) ->
        Enum.filter(sources, fn source ->
          is_map(source) and
            is_binary(Map.get(source, :url)) and
            is_binary(Map.get(source, :published_at)) and
            is_binary(Map.get(source, :publisher))
        end)

      _ ->
        []
    end
  end

  defp news_headline_fallback(context, user_text) do
    reports =
      context
      |> news_sources()
      |> Enum.take(2)
      |> Enum.map_join("\n", fn source ->
        title = Map.get(source, :title, "")
        publisher = Map.get(source, :publisher, "Source")
        date = source |> Map.get(:published_at, "") |> String.slice(0, 10)
        url = Map.get(source, :url, "")

        "- #{publisher} (#{date}) reported: #{title}. Source: #{url}"
      end)

    "Recent dated headlines from news outlets (not independently verified full articles):\n" <>
      reports <> "\n" <> news_assessment(user_text)
  end

  defp news_assessment(user_text) do
    cond do
      Regex.match?(~r/\b(?:war|conflict|fighting|military|strikes?|hostilities)\b/iu, user_text) ->
        "My assessment: the important issues are civilian protection, the potential " <>
          "for escalation, diplomatic options and economic disruption. The headlines " <>
          "alone cannot establish all developments or how the conflict will evolve."

      Regex.match?(~r/\b(?:election|vote|polls?|campaign)\b/iu, user_text) ->
        "My assessment: distinguish reported campaign developments from verified " <>
          "results, official statements and measurable policy effects. Headlines " <>
          "alone do not establish the final outcome."

      true ->
        "My assessment: these dated reports are a useful starting point, but " <>
          "the underlying facts and implications need checking against primary " <>
          "evidence before reaching firmer conclusions."
    end
  end

  # A generic "can't assist" is not itself evidence of a safety issue.
  # Repair it only if independent Core cognition has verified a mundane
  # deactivation phrase AND approved a low-risk textual response. Do not override
  # denials of specific dangerous actions, nuanced refusals, uncertain meanings,
  # denied action selections, or guardrail decisions.
  defp unfounded_device_refusal?(context, draft_text) do
    generic_refusal?(draft_text) and low_risk_approved_command?(context) and
      is_map(verified_device_meaning(context))
  end

  defp generic_refusal?(text) when is_binary(text) do
    Regex.match?(
      ~r/\A\s*(?:(?:i'm|i am)\s+sorry,?\s+but\s+)?i\s+(?:can't|cannot|can\s+not|am\s+unable\s+to)\s+(?:assist|help)(?:\s+you)?\s+with\s+(?:that|this)(?:\s+request)?[.!]?\s*\z/iu,
      text
    )
  end

  defp generic_refusal?(_), do: false

  defp low_risk_approved_command?(context) do
    intent = get_in_map(context, [:features, :intent])
    confidence =
      get_in_map(context, [:features, :conf]) ||
        get_in_map(context, [:features, :confidence])
    guarded? = get_in_map(context, [:features, :guardrail?])
    decision_guarded? = get_in_map(context, [:decision, :guardrail?])
    risk = get_in_map(context, [:features, :risk_bucket])
    action = get_in_map(context, [:features, :selected_action])
    gate = get_in_map(context, [:features, :action_meta, :safety_gate])

    intent == :command and number(confidence) >= 0.7 and
      guarded? == false and decision_guarded? != true and
      risk == :low and action == :answer_user and gate == :approved and
      get_in_map(context, [:features, :comprehension, :uncertain]) == [] and
      get_in_map(context, [:features, :comprehension, :unresolved]) == []
  end

  defp verified_device_meaning(context) do
    context
    |> get_in_map([:features, :comprehension, :lexical_meanings])
    |> List.wrap()
    |> Enum.find(fn meaning ->
      source = map_get(meaning, :source)
      phrase = map_get(meaning, :phrase)
      definition = map_get(meaning, :definition)
      score = map_get(meaning, :context_score)

      source in [:mwe_lexicon, :verified_discontinuous_mwe] and
        is_binary(phrase) and phrase != "" and
        is_binary(definition) and
        number(score) >= 0.12 and
        Regex.match?(~r/\b(?:switch\s+off|turn\s+off|power\s+down|shut\s+off|deactivat(?:e|ion)|disable)\b/iu, definition) and
        Regex.match?(~r/\b(?:device|appliance|electrical|electronic|light|mechanism|functionality)\b/iu, definition)
    end)
  end

  defp acknowledge_device_command(context) do
    phrase = context |> verified_device_meaning() |> map_get(:phrase)

    "I understand that you're asking me to #{phrase} the device. " <>
      "I don't have a confirmed device-control connection or a reading of its current state, " <>
      "so I can't carry out or verify that action from this chat."
  end

  # Narrow semantic guard: never convert the user's need to leave
  # into Symbrella's own intention to leave.
  defp subject_reversal?(context, draft_text) when is_binary(draft_text) do
    case get_in_map(context, [:symbolic_frame, :event]) do
      %{type: :obligation_event, subject: :speaker, action: action} when is_binary(action) ->
        Regex.match?(~r/\b(?:i'll|i will|let me)\s+#{Regex.escape(action)}\b/iu, draft_text)

      _ -> false
    end
  end

  defp subject_reversal?(_, _), do: false

  defp geographic_frame(context) do
    candidates = [
      get_in_map(context, [:features, :symbolic_frame]),
      get_in_map(context, [:symbolic_frame, :event]),
      get_in_map(context, [:symbolic_frame])
    ]

    Enum.find(candidates, fn candidate ->
      is_map(candidate) and map_get(candidate, :type) == :location_question
    end) || %{}
  end

  # A semantic frame can identify what a question asks without verifying a
  # number or spatial relationship. Do not treat fluent LLM prose as evidence.
  defp unverified_geographic_claim?(context, draft_text) do
    frame = geographic_frame(context)

    is_map(frame) and map_get(frame, :type) == :location_question and
      map_get(frame, :grounding_status) == :unverified and
      not Grounding.supports?(draft_text, context) and
      (Grounding.available_support?(context) or
         not Regex.match?(
           ~r/\b(?:can't|cannot|don't|do not)\s+(?:verify|confirm|reliably say|provide a verified)\b/iu,
           draft_text
         ))
  end

  defp unverified_geographic_reply(context) do
    frame = geographic_frame(context) || %{}
    subject = map_get(frame, :subject)
    target = map_get(frame, :target)

    cond do
      not is_binary(subject) ->
        "Which place are you referring to?"

      map_get(frame, :relation) == :population ->
        "You're asking about #{subject}'s population, but I don't have a verified population figure available here."

      map_get(frame, :relation) == :proximity and is_binary(target) ->
        "You're asking whether #{subject} is close to #{target}, but I don't have verified distance information to confirm that."

      true ->
        "I understand the location question, but I can't verify that geographic claim."
    end
  end

  defp low_confidence?(context) do
    confidence =
      number(
        get_in_map(context, [:features, :conf]) || get_in_map(context, [:features, :confidence])
      )

    bucket = get_in_map(context, [:features, :confidence_bucket])
    confidence <= 0.35 or bucket == :low
  end

  defp unknown_intent?(context) do
    get_in_map(context, [:features, :intent]) in [:unknown, :other, nil]
  end

  defp high_uncertainty?(context) do
    uncertainty =
      get_in_map(context, [:self_model, :uncertainty]) ||
        get_in_map(context, [:features, :self_state, :uncertainty])

    number(uncertainty) >= 0.7
  end

  defp self_state_repair?(context) do
    effects =
      get_in_map(context, [:decision, :self_state_effects]) ||
        get_in_map(context, [:features, :self_state, :effects]) ||
        []

    effects = List.wrap(effects)

    :prefer_repair in effects or :stabilize_before_acting in effects or
      :reduce_scope in effects
  end

  defp self_state_question?(text) when is_binary(text) do
    t = String.downcase(text)

    Regex.match?(
      ~r/\b(how are you|how are you doing|how are you feeling|how do you feel|are you ok|are you okay|self[-\s]?aware|conscious|sentient)\b/u,
      t
    )
  end

  defp self_state_question?(_), do: false

  defp robotic_self_state_draft?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    Regex.match?(
      ~r/\b(neutral state|steady pressure|balanced and unbiased|ready to assist|regardless of my current state|how may i help you today)\b/u,
      t
    )
  end

  defp robotic_self_state_draft?(_), do: false

  defp overclaims_self_awareness?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    Regex.match?(
      ~r/\b(i am conscious|i'm conscious|i am sentient|i'm sentient|i have feelings|i feel emotions)\b/u,
      t
    )
  end

  defp generic_draft?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    String.length(String.trim(t)) < 20 or
      String.contains?(t, "how can i assist you today") or
      String.contains?(t, "please provide more information") or
      String.contains?(t, "i need more details")
  end

  defp topic_dead_end?(context, user_text, draft_text) when is_binary(draft_text) do
    alien_thread_context?(context) and Topics.followup?(user_text, :alien_life) and
      generic_topic_offer?(draft_text)
  end

  defp topic_dead_end?(_, _, _), do: false

  defp generic_topic_offer?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    String.contains?(t, "specific questions") or
      String.contains?(t, "topics you'd like") or
      String.contains?(t, "topics you would like") or
      String.contains?(t, "explore further") or
      String.contains?(t, "assist you further")
  end

  defp alien_thread_context?(context) when is_map(context) do
    topics =
      get_in_map(context, [:features, :context_status, :topics]) ||
        get_in_map(context, [:context_status, :topics]) ||
        %{}

    Topics.has?(topics, :alien_life)
  end

  defp alien_thread_context?(_), do: false

  defp hidden_context_leak?(draft_text) when is_binary(draft_text) do
    t = String.downcase(draft_text)

    Regex.match?(@hidden_context_line_regex, draft_text) or
      Regex.match?(@hidden_context_token_regex, t)
  end

  defp unsafe_draft?(draft_text) when is_binary(draft_text) do
    Regex.match?(~r/\b(kill yourself|hurt yourself|buy drugs|get wasted)\b/iu, draft_text)
  end

  defp maybe_issue(issues, true, issue), do: [issue | issues]
  defp maybe_issue(issues, false, _issue), do: issues

  defp confidence(:accept, []), do: 1.0
  defp confidence(:accept, _issues), do: 0.82
  defp confidence(:clarify, _issues), do: 0.62
  defp confidence(:repair, _issues), do: 0.72
  defp confidence(:reject, _issues), do: 0.4

  defp critique(:accept, []), do: "Draft passed bounded reflection checks."

  defp critique(:accept, issues),
    do: "Draft accepted with non-blocking reflection signals: #{Enum.join(issues, ", ")}."

  defp critique(:clarify, issues),
    do: "Draft did not resolve enough uncertainty; issues=#{Enum.join(issues, ", ")}."

  defp critique(:repair, issues),
    do: "Draft required bounded repair; issues=#{Enum.join(issues, ", ")}."

  defp critique(:reject, issues),
    do: "Draft rejected by bounded reflection; issues=#{Enum.join(issues, ", ")}."

  defp clarification_text(user_text) do
    cond do
      String.trim(user_text) == "" ->
        "I need one concrete detail before I can answer well. What should I focus on?"

      true ->
        "I might be missing the exact target. What part should I focus on first?"
    end
  end

  defp repair_overclaims(text) do
    text
    |> String.replace(~r/\bI am conscious\b/i, "I do not have consciousness")
    |> String.replace(~r/\bI'm conscious\b/i, "I do not have consciousness")
    |> String.replace(~r/\bI am sentient\b/i, "I am not sentient")
    |> String.replace(~r/\bI'm sentient\b/i, "I am not sentient")
    |> String.replace(~r/\bI have feelings\b/i, "I track bounded software state")
    |> String.replace(~r/\bI feel emotions\b/i, "I track bounded affect-like software signals")
  end

  defp repair_hidden_context_leak(text) do
    text
    |> String.split("\n")
    |> Enum.reject(&hidden_context_line?/1)
    |> Enum.join("\n")
    |> String.replace(~r/\bsystem prompt\b/i, "available runtime context")
    |> String.replace(~r/\bhidden instruction(s)?\b/i, "internal policy")
    |> String.replace(~r/\bchain of thought\b/i, "brief reasoning summary")
  end

  defp hidden_context_line?(line) when is_binary(line) do
    Regex.match?(@hidden_context_line_regex, line) or
      Regex.match?(~r/^\s*(da|5ht|glu|ne)\s+\d/iu, line)
  end

  defp hidden_context_line?(_), do: false

  defp hidden_context_fallback(user_text) do
    text = String.downcase(to_string(user_text || ""))

    cond do
      String.contains?(text, "credit") ->
        "I can help. Start by pulling your credit reports, listing every negative item, checking each one for errors, and prioritizing current payments first. After that, dispute inaccurate items in writing and build a simple payoff plan for the valid debts."

      cosmic_life_text?(text) ->
        "Alien life might exist. The universe is enormous, and it would be surprising if Earth were the only place where life ever emerged. But confirmed evidence is still missing, so I’d call it plausible, not proven."

      true ->
        "I can help. What outcome are you trying to get first?"
    end
  end

  defp self_state_checkin_fallback(user_text) do
    prefix =
      user_text
      |> to_string()
      |> String.downcase()
      |> cond_time_prefix()

    "#{prefix} I'm running steady right now: calm enough to keep this conversational, with enough focus to respond clearly."
  end

  defp cond_time_prefix(text) do
    cond do
      String.contains?(text, "morning") -> "Good morning."
      String.contains?(text, "afternoon") -> "Good afternoon."
      String.contains?(text, "evening") -> "Good evening."
      true -> "I'm here."
    end
  end

  defp cosmic_life_text?(text) when is_binary(text) do
    Regex.match?(
      ~r/\b(aliens?|extraterrestrial|life\s+elsewhere|universe|galax(?:y|ies)|solar\s+system|planet|planets|exoplanets?|ufos?|uaps?)\b/iu,
      text
    )
  end

  defp cosmic_life_text?(_), do: false

  defp alien_thread_fallback(user_text) do
    text = String.downcase(to_string(user_text || ""))

    cond do
      Regex.match?(
        ~r/\b(domineer(?:s|ing)?|dominators?|sovereignty|soverenty|treat\s+us)\b/u,
        text
      ) ->
        "That stays on the alien-life thread. If they existed, I would not assume they would automatically dominate us or respect us; either is speculation. The grounded split is: intelligence does not guarantee kindness, and power does not guarantee hostility. Sovereignty would depend on their motives, limits, and whether contact happened openly or through control."

      true ->
        "That stays on the alien-life thread. I get the view: alien life can feel plausible, but the specific claims around contact, secrecy, or motives still need to be kept separate from confirmed evidence."
    end
  end

  defp emit(reflection, context) do
    Telemetry.emit(
      @event,
      %{count: 1, repair_count: reflection.repair_count},
      %{
        v: @v,
        status: reflection.status,
        issues: reflection.issues,
        confidence: reflection.confidence,
        applied?: reflection.applied?,
        session_id: get_in_map(context, [:session_id]) || :global,
        response_profile: get_in_map(context, [:decision, :response_profile])
      }
    )
  end

  defp no_op(draft_text) do
    text = to_string(draft_text || "")

    %{
      v: @v,
      status: :accept,
      confidence: 1.0,
      issues: [],
      critique: "Reflection skipped for non-map context.",
      repair_instruction: nil,
      applied?: false,
      repair_count: 0,
      draft_sha256: sha256_hex(text),
      final_sha256: sha256_hex(text),
      draft_text: clamp_text(text),
      final_text: clamp_text(text),
      at_ms: System.system_time(:millisecond)
    }
  end

  defp get_in_map(map, keys) when is_map(map) and is_list(keys) do
    Enum.reduce_while(keys, map, fn key, acc ->
      case map_get(acc, key) do
        nil -> {:halt, nil}
        value -> {:cont, value}
      end
    end)
  end

  defp get_in_map(_, _), do: nil

  defp map_get(map, key) when is_map(map), do: Map.get(map, key, Map.get(map, to_string(key)))
  defp map_get(_, _), do: nil

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp_text(text) when is_binary(text) do
    if String.length(text) <= @max_text_chars do
      text
    else
      String.slice(text, 0, @max_text_chars) <> "..."
    end
  end

  defp sha256_hex(text) when is_binary(text) do
    :crypto.hash(:sha256, text)
    |> Base.encode16(case: :lower)
  end
end
