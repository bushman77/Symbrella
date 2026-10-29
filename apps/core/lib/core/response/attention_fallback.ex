defmodule Core.Response.AttentionFallback do
  @moduledoc """
  Native continuation from the current session's admitted comprehension and
  attention frame. No lexical guess, external fact, LLM, or cross-session
  global-WM item may authorize a response.

  This is deliberately weaker than answer generation: a focus is a topic,
  not an answer. Keep the distinction observable through response metadata.
  """

  alias Core.Brain.WM.Comprehension, as: WMComprehension
  alias Core.Response.LexicalDefinition

  @max_terms 2
  @max_repair_terms 6
  @completed_meanings [
    :explicit_temporal_report, :temporal_clarification_resolution,
    :temporal_preference_recall_question, :temporal_claim_conflict_question
  ]
  @question_intents [:ask, :ask_info, :question, :query, :help, :information_request]

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(text, %{risk_bucket: :low} = features, %{action: action}, si)
      when is_binary(text) and byte_size(text) <= 2048 and is_map(si) and
             action in [:answer, :offer_options, :act_first, :ask_first] do
    with nil <- LexicalDefinition.requested_term(text),
         false <- Map.get(features, :intent) == :define,
         session when is_binary(session) and byte_size(session) in 1..128 <-
           Map.get(si, :session_id),
         ^text <- Map.get(si, :sentence),
         true <- Map.get(si, :source) in [:user, :prod, :test],
         %{} = context <- Map.get(si, :wm_context),
         %{} = admitted <- WMComprehension.admitted_context(session),
         true <- context == admitted,
         :conversation_context_only <- Map.get(context, :verification),
         %{scope: :session, salience: salience, goal_terms: goal_terms} <-
           Map.get(context, :attention),
         true <- is_number(salience) and salience > 0.0,
         true <- is_list(goal_terms) and goal_terms != [],
         true <- Map.get(context, :focus_transition) != :release do
      case partial_repair(text, si, context, goal_terms, salience) do
        {:ok, _, _} = repair ->
          repair

        :not_applicable when action in [:answer, :offer_options, :act_first] ->
          case focused_terms(context) do
            {:ok, kind, terms} ->
              question? = Map.get(features, :intent) in @question_intents or
                String.ends_with?(String.trim(text), "?")

              reply = realize(kind, terms, question?)

              {:ok, reply, %{
                status: :attention_continuation,
                provenance: :admitted_same_session_comprehension,
                focus_kind: kind,
                focus_terms: terms,
                evidence_level: :conversation_context_only,
                semantic_verified?: false,
                factual_claims?: false,
                llm_called?: false
              }}

            _ ->
              :not_applicable
          end

        :not_applicable ->
          :not_applicable
      end
    else
      _ -> :not_applicable
    end
  end

  def generate(_, _, _, _), do: :not_applicable

  # Unknowns are selected from the *current turn's* weak/unresolved
  # comprehension, matched against the admitted same-session WM frame. An
  # attention match influences priority, but cannot turn a weak term into a
  # verified dictionary sense or an asserted real-world claim.
  defp partial_repair(text, si, context, goal_terms, salience) do
    summary = Map.get(si, :comprehension)

    with true <- is_map(summary),
         true <- Map.get(context, :turn_sha256) ==
                   (:crypto.hash(:sha256, text) |> Base.encode16(case: :lower)),
         true <- Map.get(context, :interpretation_status) not in [:fully_verified],
         true <- not completed_meaning?(si),
         uncertain when is_list(uncertain) <- Map.get(summary, :uncertain),
         true <- safe_terms_max(uncertain, @max_repair_terms) ==
                   safe_terms_max(Map.get(context, :uncertain), @max_repair_terms),
         [first | rest] <- safe_terms_max(uncertain, @max_repair_terms)
                           |> Enum.filter(fn term ->
                             surface_present?(text, term) and
                               not recognized_topic_part?(context, term)
                           end),
         true <- is_list(goal_terms) do
      candidates = [first | rest]
      target = Enum.find(candidates, &attended?(&1, goal_terms)) || first
      verified = verified_known(si, context, text, target)
      recognized = recognized_known(context, text, target)
      {known_level, known_term} =
        cond do
          is_binary(verified) -> {:dictionary_verified, verified}
          is_binary(recognized) -> {:recognized_focus, recognized}
          true -> {:none, nil}
        end

      reply =
        case {known_level, known_term} do
          {:dictionary_verified, word} ->
            "I understand \"#{word}\", but I'm not sure how you're using \"#{target}\" here. Could you clarify that part?"

          {:recognized_focus, word} ->
            "I'm following \"#{word}\", but I'm not sure how you're using \"#{target}\" here. Could you clarify that part?"

          _ ->
            "I'm not sure how you're using \"#{target}\" here. Could you clarify what you mean?"
        end

      {:ok, reply, %{
        status: :attention_comprehension_repair,
        provenance: :admitted_same_session_comprehension,
        recognized_term: known_term,
        recognized_level: known_level,
        unresolved_term: target,
        attention_selected?: attended?(target, goal_terms),
        attention_salience: salience,
        focus_terms: Enum.take(goal_terms, @max_terms),
        evidence_level: :conversation_context_only,
        semantic_verified?: false,
        factual_claims?: false,
        llm_called?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  # A lexical weak/uncertain label can describe a word that is already
  # recognized as part of the *requested topic*. Such a label is not, by
  # itself, a reason to ask what the topic word means. E.g. a question about
  # "the Pacific Ocean" must not become "what do you mean by ocean?"
  #
  # Both an explicit WM topic and its independently recorded recognized-topic
  # anchor are required. This must NOT suppress genuinely unrecognized words
  # elsewhere in the current utterance, or ambiguous non-topic vocabulary.
  defp recognized_topic_part?(context, term) do
    topic = Map.get(context, :topic)
    recognized = Map.get(context, :recognized_topics)

    is_binary(topic) and surface_present?(topic, term) and
      is_list(recognized) and
      Enum.any?(recognized, fn anchor ->
        is_binary(anchor) and surface_present?(anchor, term)
      end)
  end

  defp completed_meaning?(si) do
    case Map.get(si, :sentence_meaning) do
      %{kind: kind} when kind in @completed_meanings -> true
      _ -> false
    end
  end

  defp verified_known(si, context, text, target) do
    understood = safe_terms_max(Map.get(context, :understood), @max_repair_terms)

    si
    |> Map.get(:lexical_constructions, [])
    |> List.wrap()
    |> Enum.find_value(fn
      %{dictionary_verified?: true, id: id, phrase: phrase,
        definition: definition, context_score: score}
      when is_binary(id) and is_binary(phrase) and is_binary(definition) and
           is_number(score) and score >= 0.12 ->
        if phrase in understood and phrase != target and
             safe_term(phrase) == phrase and surface_present?(text, phrase),
          do: phrase, else: nil

      _ ->
        nil
    end)
  end

  defp recognized_known(context, text, target) do
    ([Map.get(context, :topic)] ++
       List.wrap(Map.get(context, :entities)) ++
       List.wrap(Map.get(context, :recognized_topics)))
    |> Enum.map(&safe_term/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.find(fn word ->
      String.downcase(word) != String.downcase(target) and
        surface_present?(text, word)
    end)
  end

  defp attended?(term, goal_terms) do
    Enum.any?(goal_terms, fn goal ->
      is_binary(goal) and String.downcase(String.trim(goal)) ==
        String.downcase(String.trim(term))
    end)
  end

  defp surface_present?(text, term)
       when is_binary(text) and is_binary(term) do
    Regex.match?(
      Regex.compile!("(?:\\A|[^\\p{L}\\p{N}])" <>
        Regex.escape(term) <> "(?:\\z|[^\\p{L}\\p{N}])", "iu"),
      text
    )
  end

  defp safe_terms_max(terms, limit) when is_list(terms) do
    terms
    |> Enum.map(&safe_term/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.take(limit)
  end

  defp safe_terms_max(_, _), do: []

  defp focused_terms(context) do
    topic = safe_term(Map.get(context, :topic))
    entities = safe_terms(Map.get(context, :entities))
    recognized = safe_terms(Map.get(context, :recognized_topics))

    cond do
      is_binary(topic) -> {:ok, :topic, [topic]}
      entities != [] -> {:ok, :entities, entities}
      recognized != [] -> {:ok, :recognized_topics, recognized}
      true -> :not_applicable
    end
  end

  defp safe_terms(values) when is_list(values) do
    values
    |> Enum.map(&safe_term/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.take(@max_terms)
  end

  defp safe_terms(_), do: []

  defp safe_term(term) when is_binary(term) do
    term = String.trim(term)

    if byte_size(term) <= 96 and
         Regex.match?(~r/\A[\p{L}\p{N}][\p{L}\p{M}\p{N} ',.\-]*\z/u, term) do
      term
    end
  end

  defp safe_term(_), do: nil

  defp realize(:topic, [topic], true),
    do: "I'm following the topic of #{topic}, but I don't have a grounded answer to that yet."

  defp realize(:topic, [topic], false),
    do: "I'm keeping #{topic} in focus, though I haven't fully interpreted your point yet."

  defp realize(_kind, terms, true),
    do: "I'm keeping #{join_terms(terms)} in mind, but I can't answer that reliably yet."

  defp realize(_kind, terms, false),
    do: "I'm following what you're saying about #{join_terms(terms)}, but I haven't established the relationship yet."

  defp join_terms([first, second]), do: first <> " and " <> second
  defp join_terms([only]), do: only
end
