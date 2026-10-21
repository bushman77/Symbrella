defmodule Core.Curiosity.ClarificationLoop do
  @moduledoc """
  Supervised, bounded conversation-clarification feedback.

  A user-visible question is proposed only for a recent, same-session,
  explicitly unresolved lexical term in a nonfactual statement. The current
  answer is never suppressed or rewritten. On the *next user turn*, a
  clarification is credited only when an independently dictionary-verified
  lexical construction for the same term appears in that turn's Core SI.
  A user explanation alone is NOT a verified dictionary sense or a fact.

  This loop does not write CognitiveIssues, change dictionary senses, promote
  conversational statements to facts, or modify CuriosityLearning's
  proof-grounded population-reasoning strategies.

  The first milestone uses bounded BEAM-local receipts. They are inspectable
  in IEx but do not survive a runtime restart.
  """
  use GenServer

  alias Core.Curiosity.ComprehensionGaps

  @max_sessions 64
  @max_receipts 128
  @pending_ttl_ms 10 * 60_000
  @cooldown_ms 10 * 60_000
  @focus_stopwords ~w(the this that these those there here what when where which while about after before their your our with between from still think known thing things someone something people please would could should conflict action reply message question maybe just some else where happening)

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @doc "Handle a completed real user/assistant turn; never emits chat messages itself."
  @spec observe(term(), map(), String.t(), map(), GenServer.server()) ::
          {:ask, String.t(), map()} | {:replace_generic, String.t(), map()} |
          {:tracked_existing, String.t(), map()} | :none
  def observe(session_id, si, reply, meta, server \\ __MODULE__)

  def observe(session_id, %{} = si, reply, %{} = meta, server)
      when is_binary(session_id) and byte_size(session_id) > 2 and
             byte_size(session_id) <= 128 and is_binary(reply) do
    gap = ComprehensionGaps.for_session(session_id)
    GenServer.call(server, {:observe, session_id, si, reply, meta, gap})
  rescue
    _ -> :none
  catch
    :exit, _ -> :none
  end

  def observe(_, _, _, _, _), do: :none

  @doc "Select the final text without ever appending a second question."
  @spec display_text(String.t(), term()) :: String.t()
  def display_text(_original, {:replace_generic, clarification, _flags})
      when is_binary(clarification), do: clarification
  def display_text(original, _decision), do: original

  # A model may phrase a generic pleasantry in dozens of ways. Match a
  # small *class* of whole-message open-ended prompts, not one literal
  # sentence. Do not rewrite a specific question about an actual concept,
  # an informative answer, or a multi-question reply.
  @generic_opening ~r/\A(?:(?:congratulations|(?:that|it|this) sounds (?:great|interesting|exciting|cool|neat|amazing)|that'?s (?:great|interesting|exciting|cool|neat|amazing)|great|awesome|nice|interesting|cool|neat|good for you)[!.,]\s*){0,2}/iu
  @generic_followups [
    ~r/\Awhat (?:have you been (?:learning|working) (?:about|on|in|with)|have you (?:learned|discovered)(?: so far)?|are you (?:working on|building|learning)(?: now)?)(?: (?:about|in|with|on) (?:your|the|this) (?:module|project))?\?\z/iu,
    ~r/\Awhat (?:does|will|is) (?:your|the|this) (?:module|project) (?:do|about|for)\?\z/iu,
    ~r/\Awhat(?:'s| is) (?:it|that|your module|your project|the module|the project) (?:about|for)\?\z/iu,
    ~r/\Awhat (?:kind|type) of (?:module|project) (?:is it|are you building)\?\z/iu,
    ~r/\A(?:can|could|would) you (?:tell me more|share more|elaborate)(?: about (?:it|that|your module|the module|your project|the project))?\?\z/iu,
    ~r/\Awhat (?:are you hoping to achieve|do you plan to do|are your plans|are your goals)(?: with (?:it|that|the module|your module|your project))?\?\z/iu,
    ~r/\Ahow (?:is|are|does) (?:it|that|the module|your module|the project|your project) (?:going|coming along|work|progressing)\?\z/iu,
    ~r/\Awhat (?:have you been focusing on|part are you working on)(?: in (?:your|the) (?:module|project))?\?\z/iu
  ]

  @spec replaceable_generic_reply?(term()) :: boolean()
  def replaceable_generic_reply?(reply) when is_binary(reply) do
    trimmed = String.trim(reply)
    question = trimmed |> String.replace(@generic_opening, "") |> String.trim()

    String.length(trimmed) <= 240 and
      length(String.split(question, "?", trim: true)) == 1 and
      Enum.any?(@generic_followups, &Regex.match?(&1, question))
  end

  def replaceable_generic_reply?(_), do: false

  @doc "Inspect the global learning bias and one session's pending/last receipt."
  @spec snapshot(term(), GenServer.server()) :: map()
  def snapshot(session_id \\ nil, server \\ __MODULE__), do:
    GenServer.call(server, {:snapshot, session_id})

  @doc "Pure strategy evaluation. Term matching alone never earns learning credit."
  @spec evaluate(map(), map()) :: atom()
  def evaluate(%{term: term, verified_before: before}, %{} = si)
      when is_binary(term) and is_list(before) do
    sentence = get(si, :sentence, "")
    current = verified_term_ids(si, term)

    cond do
      not is_binary(sentence) or not refers_to_term?(sentence, term) ->
        :unrelated_followup

      Enum.any?(current, &(not Enum.member?(before, &1))) ->
        :verified_improvement

      true ->
        :clarification_received_unverified
    end
  end

  def evaluate(_, _), do: :unrelated_followup

  @doc "Pure, conservative eligibility policy. Never infer facts from a gap."
  @spec proposal(map(), term(), String.t(), map()) :: {:ok, String.t()} | :none
  def proposal(si, gap, reply, meta) do
    case diagnose(si, gap, reply, meta) do
      {kind, term} when kind in [:eligible, :eligible_existing] -> {:ok, term}
      {:skip, _reason} -> :none
    end
  end

  @doc """
  Read-only diagnostic for why a completed turn can or cannot ask a
  clarification question. Reasons are bounded atoms, not user text.
  """
  @spec diagnose(term(), term(), term(), term()) ::
          {:eligible, String.t()} | {:skip, atom()}
  def diagnose(%{} = si, {:ok, %{status: :review_required,
      next_action: :consider_user_clarification, age_ms: age}}, reply, meta)
      when is_integer(age) and age >= 0 and age <= 5 * 60_000 and
           is_binary(reply) and is_map(meta) do
    comprehension = get(si, :comprehension, %{})
    sentence = get(si, :sentence, "")
    action = get(meta, :agency_decision, %{})
    reviewed = get(meta, :response_reflection, %{})

    cond do
      get(comprehension, :degraded?) != true ->
        {:skip, :comprehension_not_degraded}

      not is_binary(sentence) or String.contains?(sentence, "?") ->
        {:skip, :factual_or_question_turn}

      not user_originated_turn?(si) ->
        {:skip, :not_user_originated}

      not eligible_statement_intent?(si, sentence) ->
        {:skip, :intent_not_statement}

      Regex.match?(~r/\b(?:hurt myself|suicide|emergency|dangerous|overdose|abuse|violence|dying|kill myself|self-harm)\b/iu, sentence) ->
        {:skip, :safety_sensitive}

      not agency_allows_feedback?(action) ->
        {:skip, :agency_not_approved}

      get(meta, :response_source) != :llm ->
        {:skip, :not_normal_llm_reply}

      get(reviewed, :status, :accept) in [:reject, :clarify] ->
        {:skip, :response_not_final}

      true ->
        terms =
          comprehension
          |> get(:unresolved, [])
          |> List.wrap()
          |> Enum.filter(&eligible_focus?/1)
          |> Enum.sort_by(&String.length/1, :desc)

        case terms do
          [term | _] ->
            cond do
              already_clarifies?(reply, term) -> {:eligible_existing, String.trim(term)}
              get(action, :selected_action) == :ask_clarifying_question ->
                {:skip, :agency_clarification_not_term_linked}

              String.contains?(reply, "?") and
                  not replaceable_generic_reply?(reply) ->
                {:skip, :reply_already_asks_question}

              true ->
                {:eligible, String.trim(term)}
            end

          [] -> {:skip, :no_eligible_unresolved_term}
        end
    end
  end

  def diagnose(_, _, _, _), do: {:skip, :no_fresh_session_gap}

  defp agency_allows_feedback?(action) do
    case {get(action, :risk), get(action, :selected_action)} do
      {:low, :answer_user} -> true
      {:low, :ask_clarifying_question} ->
        selector = get(action, :selector, %{})
        get(selector, :safety_gate) == :approved and
          get(get(selector, :safety, %{}), :decision) == :approved and
          get(get(action, :permission, %{}), :status) == :not_required
      _ -> false
    end
  end

  # A declarative work report can be misclassified as :code or :ask.
  # Require an explicit first-person past/current engineering-work pattern,
  # not merely the absence of a question mark. Unsafe intents remain excluded.
  defp eligible_statement_intent?(si, sentence) do
    intent = get(si, :intent)

    intent in [:statement, :unknown, :inform, :tell, :smalltalk] or
      (intent in [:ask, :ask_info, :code, :debug, :memory_write] and
         is_binary(sentence) and
         Regex.match?(
           ~r/^\s*(?:(?:i['’]ve|i have|we['’]ve|we have)\s+(?:been\s+)?|(?:i['’]m|i am|we['’]re|we are)\s+(?:currently\s+)?)(?:working|building|developing|designing|creating|testing|writing|making|experimenting)\s+(?:on|with|in|at)\b/iu,
           sentence
         ))
  end

  # No second question is necessary when the model explicitly asked about
  # the meaning of the exact unresolved term. Mentioning a term in a
  # technical question ("How does X work?") does not meet this condition.
  @spec already_clarifies?(term(), term()) :: boolean()
  def already_clarifies?(reply, term) when is_binary(reply) and is_binary(term) do
    String.contains?(reply, "?") and
      refers_to_term?(reply, term) and
      (embedded_module_definition?(reply, term) or
      (Regex.match?(
        ~r/\b(?:what(?:'s| is| does)|what do you mean by|(?:can|could|would) you (?:explain|define|clarify)|what (?:is the meaning|do you mean))\b/iu,
        reply
      ) and
      (Regex.match?(~r/\b(?:mean|means|meaning|refer(?:ring)? to|define|explain|clarify)\b/iu,
         reply) or
         Regex.match?(
           Regex.compile!(
             "\\bwhat(?:'s| is)\\s+[\"']?" <> Regex.escape(term) <>
               "[\"']?\\s*\\?",
             "iu"
           ),
           reply
         ))))
  end

  # The actual live model asks "tell me more about what a glimmerflux
  # module is and how it works?" That is a user-answerable clarification,
  # not a generic "what are you working on" question.
  defp embedded_module_definition?(reply, term) do
    Regex.match?(~r/\b(?:can|could|would) you (?:tell me more about|explain|clarify|describe)\b/iu, reply) and
      Regex.match?(
        Regex.compile!(
          "\\bwhat\\s+(?:(?:a|an|the)\\s+)?[\"']?" <>
            Regex.escape(term) <>
            "[\"']?(?:\\s+(?:module|project|component|system|library|thing))?\\s+is\\b",
          "iu"
        ),
        reply
      )
  end

  def already_clarifies?(_, _), do: false

  # :source is a pipeline execution mode (:prod) in Core.resolve_input.
  # User provenance is independently marked with :experience_origin.
  # Never treat internal production events as user speech.
  defp user_originated_turn?(si) do
    get(si, :source) == :user or
      (get(si, :source) == :prod and get(si, :experience_origin) == :user_turn)
  end

  @impl true
  def init(opts) do
    cfg = Application.get_env(:core, __MODULE__, [])
    cfg = Keyword.merge(if(is_list(cfg), do: cfg, else: []), opts)
    {:ok, %{enabled?: Keyword.get(cfg, :enabled?, false),
            auto_ask?: Keyword.get(cfg, :auto_ask?, false),
            sessions: %{}, receipts: [], attempts: 0}}
  end

  @impl true
  def handle_call({:snapshot, sid}, _from, state) do
    receipts = state.receipts
    credited = Enum.count(receipts, &(&1.outcome == :verified_improvement))
    unverified = Enum.count(receipts, &(&1.outcome == :clarification_received_unverified))
    # Independent of Brain.CuriosityLearning. Only verified, term-linked
    # lexical results can change the small preference value.
    bias = min(credited * 0.03, 0.12)

    session = Map.get(state.sessions, sid, %{})
    summary = %{
      v: 1,
      enabled?: state.enabled?,
      auto_ask?: state.auto_ask?,
      attempts: state.attempts,
      learning: %{v: 1, bias: bias, success_count: credited,
        unverified_count: unverified, sample_count: length(receipts),
        cooldown_ms: cooldown_for(credited)},
      session: %{pending: Map.get(session, :pending),
        last_outcome: Map.get(session, :last_outcome),
        last_decision: Map.get(session, :last_decision),
        last_asked_ms: Map.get(session, :last_asked_ms)}
    }
    {:reply, summary, state}
  end

  @impl true
  def handle_call({:observe, sid, si, reply, meta, gap}, _from, state) do
    now = System.system_time(:millisecond)

    if state.enabled? do
      session = Map.get(state.sessions, sid, %{})
      pending = Map.get(session, :pending)
      credited = Enum.count(state.receipts, &(&1.outcome == :verified_improvement))

      cond do
        is_map(pending) ->
          outcome =
            if now - pending.asked_at_ms > @pending_ttl_ms,
              do: :expired,
              else: evaluate(pending, si)

          receipt = %{outcome: outcome, term: pending.term,
            verified?: outcome == :verified_improvement, at_ms: now}
          session = session
            |> Map.put(:pending, nil)
            |> Map.put(:last_outcome, outcome)
            |> Map.put(:last_decision, :followup_evaluated)
          updated = state
            |> put_session(sid, session)
            |> Map.update!(:receipts, &[receipt | &1] |> Enum.take(@max_receipts))
          :telemetry.execute([:core, :curiosity, :clarification_feedback],
            %{verified: if(outcome == :verified_improvement, do: 1, else: 0)},
            %{outcome: outcome, factual_authority?: false})
          {:reply, :none, updated}

        state.auto_ask? and
            now - Map.get(session, :last_asked_ms, 0) >= cooldown_for(credited) ->
          case diagnose(si, gap, reply, meta) do
            {kind, term} when kind in [:eligible, :eligible_existing] ->
              score = 0.58 + min(credited * 0.03, 0.12)
              if score >= 0.56 do
                delivery =
                  cond do
                    kind == :eligible_existing -> :tracked_existing
                    replaceable_generic_reply?(reply) -> :replace_generic
                    true -> :ask
                  end

                question =
                  if delivery == :tracked_existing do
                    reply
                  else
                    "I couldn't determine what you meant by \"#{term}\". Could you explain that term?"
                  end

                pending = %{term: term, verified_before: verified_term_ids(si, term),
                  asked_at_ms: now, source: :session_comprehension_gap}
                decision =
                  if delivery == :tracked_existing,
                    do: :existing_clarification_tracked,
                    else: :clarification_proposed

                state2 = state
                  |> put_session(sid, Map.merge(session, %{
                    pending: pending, last_asked_ms: now, last_decision: decision
                  }))
                  |> Map.update!(:attempts, &(&1 + 1))
                :telemetry.execute([:core, :curiosity, :clarification_prompt],
                  %{proposed: 1, score: score},
                  %{reason: :unresolved_lexical_meaning,
                    delivery: delivery, factual_authority?: false})
                {:reply, {delivery, question, %{kind: :clarification_prompt,
                  delivery: delivery, factual_authority?: false,
                  learning_credit?: false}}, state2}
              else
                {:reply, :none, state}
              end

            {:skip, reason} ->
              updated = put_session(state, sid, Map.put(session, :last_decision, reason))
              {:reply, :none, updated}
          end

        true ->
          updated = put_session(state, sid, Map.put(session, :last_decision, :cooldown))
          {:reply, :none, updated}
      end
    else
      {:reply, :none, state}
    end
  end

  @doc "Verified outcomes alone shorten the question cooldown, never below six minutes."
  def cooldown_for(verified_count) when is_integer(verified_count) and verified_count >= 0,
    do: max(@cooldown_ms - verified_count * 60_000, 6 * 60_000)

  def cooldown_for(_), do: @cooldown_ms

  defp put_session(state, sid, session) do
    sessions = Map.put(state.sessions, sid, session)
    sessions =
      if map_size(sessions) > @max_sessions do
        oldest = Enum.min_by(sessions, fn {_key, s} -> Map.get(s, :last_asked_ms, 0) end)
        Map.delete(sessions, elem(oldest, 0))
      else
        sessions
      end
    %{state | sessions: sessions}
  end

  defp verified_term_ids(si, term) do
    si
    |> get(:lexical_constructions, [])
    |> List.wrap()
    |> Enum.filter(fn construction ->
      get(construction, :dictionary_verified?) == true and
        is_binary(get(construction, :id)) and
        is_binary(get(construction, :definition)) and
        is_number(get(construction, :context_score)) and
        get(construction, :context_score) >= 0.12 and
        normalized(get(construction, :phrase, "")) == normalized(term)
    end)
    |> Enum.map(&get(&1, :id))
    |> Enum.uniq()
  end

  defp eligible_focus?(term) when is_binary(term) do
    trimmed = String.trim(term)
    words = String.split(trimmed, ~r/\s+/u, trim: true)
    length(words) == 1 and
      String.length(trimmed) in 5..32 and
      Regex.match?(~r/^[\p{L}][\p{L}'-]*$/u, trimmed) and
      String.downcase(trimmed) not in @focus_stopwords
  end

  defp eligible_focus?(_), do: false

  defp refers_to_term?(sentence, term) do
    Regex.match?(
      Regex.compile!("(?<![\\p{L}\\p{N}])" <> Regex.escape(term) <>
        "(?![\\p{L}\\p{N}])", "iu"),
      sentence
    )
  end

  defp normalized(s) when is_binary(s), do: s |> String.downcase() |> String.trim()
  defp normalized(_), do: ""

  defp get(map, key, default \\ nil)
  defp get(%{} = map, key, default),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  defp get(_, _, default), do: default
end
