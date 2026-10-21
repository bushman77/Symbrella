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
          {:ask, String.t(), map()} | :none
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
  def proposal(%{} = si, {:ok, %{status: :review_required,
      next_action: :consider_user_clarification, age_ms: age}}, reply, meta)
      when is_integer(age) and age >= 0 and age <= 5 * 60_000 and
           is_binary(reply) and is_map(meta) do
    comprehension = get(si, :comprehension, %{})
    sentence = get(si, :sentence, "")
    action = get(meta, :agency_decision, %{})
    reviewed = get(meta, :response_reflection, %{})

    cond do
      get(comprehension, :degraded?) != true -> :none
      not is_binary(sentence) or String.contains?(sentence, "?") -> :none
      not is_binary(reply) or String.contains?(reply, "?") -> :none
      get(si, :source, :user) != :user -> :none
      get(si, :intent) not in [:statement, :unknown, :inform, :tell] -> :none
      Regex.match?(~r/\b(?:hurt myself|suicide|emergency|dangerous|overdose|abuse|violence|dying|kill myself|self-harm)\b/iu, sentence) -> :none
      get(action, :risk) != :low or get(action, :selected_action) != :answer_user -> :none
      get(meta, :response_source) != :llm -> :none
      get(reviewed, :status, :accept) in [:reject, :clarify] -> :none
      true ->
        comprehension
        |> get(:unresolved, [])
        |> List.wrap()
        |> Enum.find_value(fn term ->
          if eligible_focus?(term), do: {:ok, String.trim(term)}, else: nil
        end)
        |> case do
          nil -> :none
          value -> value
        end
    end
  end

  def proposal(_, _, _, _), do: :none

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
        unverified_count: unverified, sample_count: length(receipts)},
      session: %{pending: Map.get(session, :pending),
        last_outcome: Map.get(session, :last_outcome),
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

      cond do
        is_map(pending) ->
          outcome =
            if now - pending.asked_at_ms > @pending_ttl_ms,
              do: :expired,
              else: evaluate(pending, si)

          receipt = %{outcome: outcome, term: pending.term,
            verified?: outcome == :verified_improvement, at_ms: now}
          session = session |> Map.put(:pending, nil) |> Map.put(:last_outcome, outcome)
          updated = state
            |> put_session(sid, session)
            |> Map.update!(:receipts, &[receipt | &1] |> Enum.take(@max_receipts))
          :telemetry.execute([:core, :curiosity, :clarification_feedback],
            %{verified: if(outcome == :verified_improvement, do: 1, else: 0)},
            %{outcome: outcome, factual_authority?: false})
          {:reply, :none, updated}

        state.auto_ask? and now - Map.get(session, :last_asked_ms, 0) >= @cooldown_ms ->
          case proposal(si, gap, reply, meta) do
            {:ok, term} ->
              # A slightly positive verified-feedback history makes proposals
              # more competitive without ever bypassing the hard safety gates.
              verified = Enum.count(state.receipts, &(&1.outcome == :verified_improvement))
              score = 0.58 + min(verified * 0.03, 0.12)
              if score >= 0.56 do
                question = "I couldn't determine what you meant by \"#{term}\". Could you explain that term?"
                pending = %{term: term, verified_before: verified_term_ids(si, term),
                  asked_at_ms: now, source: :session_comprehension_gap}
                state2 = state
                  |> put_session(sid, Map.merge(session, %{pending: pending, last_asked_ms: now}))
                  |> Map.update!(:attempts, &(&1 + 1))
                {:reply, {:ask, question, %{kind: :clarification_prompt,
                  factual_authority?: false, learning_credit?: false}}, state2}
              else
                {:reply, :none, state}
              end

            :none -> {:reply, :none, state}
          end

        true ->
          {:reply, :none, state}
      end
    else
      {:reply, :none, state}
    end
  end

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
