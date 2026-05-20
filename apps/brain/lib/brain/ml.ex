defmodule Brain.ML do
  @moduledoc """
  Brain.ML — turn-level feature assembly for Axon training and UI explainability.

  Responsibilities (stub + usable):
  - Subscribe to high-signal Brain PubSub topics (same ones the UI uses) plus "brain:blackboard".
  - Track the latest payload per topic (intent/mood/lifg/wm/etc).
  - Assemble a stable, versioned "turn record" suitable for:
      • dataset logging (JSONL / DB later)
      • feature extraction for Axon
      • optional UI display (via blackboard or a dedicated "brain:ml" topic)

  Turn-boundary policy (P-703):
  - Open a “pending turn” as soon as we see intent arrive (PubSub), and arm a short backstop timer.
  - If we later see Stage-1 pipeline stop (bridged via blackboard telemetry), we finalize/upgrade the record.
  - If Stage-1 never runs, the backstop timer still finalizes a minimal-but-valid record.

  Guardrails:
  - Brain.ML does not depend on Core.
  - Best-effort behavior; failures should not crash the system.
  """

  use Brain, region: :ml
  require Logger

  alias Brain.Bus
  alias Brain.ML.LIFG, as: MLLIFG

  @blackboard_topic "brain:blackboard"
  @ml_topic "brain:ml"

  # Mirror what the dashboard subscribes to (plus blackboard)
  @default_topics [
    @blackboard_topic,
    "brain:clock",
    "brain:intent",
    "brain:mood",
    "brain:lifg",
    "brain:wm"
  ]

  # Blackboard bridges this telemetry, so we finalize/upgrade on that event.
  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]
  @response_complete_event [:core, :response, :complete]

  @retry_ms 50

  @doc "Return the current Brain.ML state (debug/introspection)."
  def state, do: GenServer.call(__MODULE__, :state)

  @doc "Return just the most recent assembled turn record (or nil)."
  def last_turn, do: GenServer.call(__MODULE__, :last_turn)

  @doc "Return the ring buffer of assembled turns (newest-first)."
  def turns, do: GenServer.call(__MODULE__, :turns)

  # ───────────────────────── GenServer callbacks ─────────────────────────

  @impl true
  def init(opts) do
    opts_map =
      cond do
        is_map(opts) -> opts
        is_list(opts) -> Map.new(opts)
        true -> %{}
      end

    topics =
      Application.get_env(:brain, :ml_topics, @default_topics)
      |> List.wrap()
      |> Enum.uniq()

    keep_turns =
      Application.get_env(:brain, :ml_keep_turns, 50)
      |> case do
        n when is_integer(n) and n > 0 -> n
        _ -> 50
      end

    hydrate_lex? = truthy?(Application.get_env(:brain, :ml_hydrate_lex?, true))
    publish_blackboard? = truthy?(Application.get_env(:brain, :ml_publish_blackboard?, true))

    backstop_ms =
      Application.get_env(:brain, :ml_backstop_ms, 200)
      |> case do
        n when is_integer(n) and n >= 0 -> n
        _ -> 200
      end

    pending_max_age_ms =
      Application.get_env(:brain, :ml_pending_max_age_ms, 5_000)
      |> case do
        n when is_integer(n) and n > 0 -> n
        _ -> 5_000
      end

    upgrade_grace_ms =
      Application.get_env(:brain, :ml_upgrade_grace_ms, 2_000)
      |> case do
        n when is_integer(n) and n > 0 -> n
        _ -> 2_000
      end

    send(self(), {:subscribe, topics})

    {:ok,
     %{
       region: :ml,
       opts: opts_map,
       topics: topics,
       subscribed?: false,

       # latest payloads
       last_clock: nil,
       last_intent: nil,
       last_mood: nil,
       last_lifg: nil,
       last_wm: nil,

       # last blackboard env (raw), useful for debugging
       last_blackboard: nil,

       # pending turn (opened by intent, finalized by pipeline_stop or backstop)
       pending: nil,
       backstop_ms: backstop_ms,
       pending_max_age_ms: pending_max_age_ms,
       upgrade_grace_ms: upgrade_grace_ms,

       # assembled dataset items
       last_turn: nil,
       turns: [],
       keep_turns: keep_turns,
       hydrate_lex?: hydrate_lex?,
       publish_blackboard?: publish_blackboard?
     }}
  end

  @impl true
  def handle_call(:state, _from, state), do: {:reply, state, state}
  def handle_call(:last_turn, _from, state), do: {:reply, state.last_turn, state}
  def handle_call(:turns, _from, state), do: {:reply, state.turns, state}

  @impl true
  def handle_info({:subscribe, topics}, %{subscribed?: false} = state) do
    case safe_subscribe_all(topics) do
      :ok ->
        Logger.info(fn ->
          "[Brain.ML] subscribed to #{length(topics)} topics on #{inspect(Bus.name())}"
        end)

        {:noreply, %{state | subscribed?: true}}

      {:error, reason} ->
        Logger.warning(fn ->
          "[Brain.ML] PubSub not ready (#{inspect(reason)}); retrying in #{@retry_ms}ms"
        end)

        Process.send_after(self(), {:subscribe, topics}, @retry_ms)
        {:noreply, state}
    end
  end

  # ── Topic handlers (mirror LiveView message shapes) ────────────────────────

  @impl true
  def handle_info({:blackboard, env}, state) do
    state2 =
      state
      |> Map.put(:last_blackboard, env)
      |> maybe_finalize_turn_from_blackboard(env)

    {:noreply, state2}
  end

  def handle_info({:clock, m}, state), do: {:noreply, %{state | last_clock: m}}

  # Intent is our "turn open" signal
  def handle_info({:intent, m}, state) do
    state2 =
      state
      |> Map.put(:last_intent, m)
      |> open_pending_turn(m, :intent_topic)

    {:noreply, state2}
  end

  # Mood shapes can drift; accept a few common variants
  def handle_info({:mood, m}, state), do: {:noreply, %{state | last_mood: m}}
  def handle_info({:mood_update, m, _meta}, state), do: {:noreply, %{state | last_mood: m}}
  def handle_info({:mood_event, m, _meta}, state), do: {:noreply, %{state | last_mood: m}}

  # LIFG topic shapes can drift; accept a few common variants
  def handle_info({:lifg_update, m}, state), do: {:noreply, %{state | last_lifg: m}}
  def handle_info({:lifg, m}, state), do: {:noreply, %{state | last_lifg: m}}
  def handle_info({:lifg_stage1, m}, state), do: {:noreply, %{state | last_lifg: m}}

  def handle_info({:wm, m}, state), do: {:noreply, %{state | last_wm: m}}
  def handle_info({:wm_update, m}, state), do: {:noreply, %{state | last_wm: m}}

  # Backstop finalize
  def handle_info({:finalize_pending, turn_id}, state) do
    {:noreply, maybe_finalize_pending(state, turn_id)}
  end

  # Cleanup pending if it was only timer-finalized and never upgraded
  def handle_info({:drop_pending, turn_id}, state) do
    pending = state.pending

    state2 =
      if is_map(pending) and pending.id == turn_id and pending.finalized? == true do
        %{state | pending: nil}
      else
        state
      end

    {:noreply, state2}
  end

  # Catch-all: stay resilient
  def handle_info(_msg, state), do: {:noreply, state}

  # ───────────────────────── Turn open/finalize ─────────────────────────

  defp open_pending_turn(%{backstop_ms: ms} = state, intent_payload, source) do
    now_ms = now_ms()

    if is_map(intent_payload) and map_size(intent_payload) == 0 do
      state
    else
      state0 = cancel_pending_timer(state)
      turn_id = new_turn_id()

      tref =
        if is_integer(ms) and ms > 0 do
          Process.send_after(self(), {:finalize_pending, turn_id}, ms)
        else
          nil
        end

      %{
        state0
        | pending: %{
            id: turn_id,
            opened_at_ms: now_ms,
            source: source,
            intent: intent_payload,
            text: extract_text_any(intent_payload, state0.last_blackboard),
            timer_ref: tref,
            finalized?: false,
            finalized_at_ms: nil
          }
      }
    end
  end

  defp cancel_pending_timer(%{pending: %{timer_ref: tref}} = state) when is_reference(tref) do
    _ = Process.cancel_timer(tref)
    put_in(state, [:pending, :timer_ref], nil)
  end

  defp cancel_pending_timer(state), do: state

  defp maybe_finalize_pending(%{pending: nil} = state, _turn_id), do: state

  defp maybe_finalize_pending(%{pending: %{id: id}} = state, turn_id) when id != turn_id,
    do: state

  defp maybe_finalize_pending(%{pending: pending} = state, _turn_id) do
    now_ms = now_ms()

    if too_old?(pending.opened_at_ms, now_ms, state.pending_max_age_ms) do
      %{state | pending: nil}
    else
      # If already finalized, do nothing; pipeline stop may still upgrade later.
      if pending.finalized? do
        state
      else
        trigger = %{
          kind: :ml_backstop,
          event: [:brain, :ml, :turn, :backstop],
          measurements: %{},
          meta: %{source: pending.source},
          at_ms: now_ms
        }

        # IMPORTANT: On backstop we DO NOT attempt to pull LIFG directly,
        # because Stage1 may not have run for this turn.
        rec =
          build_turn_record(
            state,
            trigger,
            pending.id,
            pending.opened_at_ms,
            pending.intent,
            pending.text
          )

        state
        |> publish_and_push(rec, :append_or_replace)
        |> mark_pending_finalized(now_ms)
        |> schedule_pending_cleanup(pending.id)
      end
    end
  end

  defp mark_pending_finalized(%{pending: pending} = state, now_ms) when is_map(pending) do
    pending2 =
      pending
      |> Map.put(:finalized?, true)
      |> Map.put(:finalized_at_ms, now_ms)
      |> Map.put(:timer_ref, nil)

    %{state | pending: pending2}
  end

  defp mark_pending_finalized(state, _), do: state

  defp schedule_pending_cleanup(state, turn_id) do
    ms = state.upgrade_grace_ms

    if is_integer(ms) and ms > 0 do
      _ = Process.send_after(self(), {:drop_pending, turn_id}, ms)
    end

    state
  end

  # ───────────────────────── Blackboard → finalize/upgrade ─────────────────────────

  defp maybe_finalize_turn_from_blackboard(state, %{} = env) do
    kind = mget(env, :kind)

    case {kind, mget(env, :event)} do
      {:telemetry, @pipeline_stop_event} ->
        finalize_on_pipeline_stop(state, env)

      {:telemetry, @response_complete_event} ->
        upgrade_on_response_complete(state, env)

      _ ->
        state
    end
  end

  defp maybe_finalize_turn_from_blackboard(state, _), do: state

  defp finalize_on_pipeline_stop(state, bb_env) do
    now_ms = now_ms()
    pending = state.pending

    event_text = extract_text_any(state.last_intent, bb_env)
    recent_turn = recent_turn_for_text(state, event_text, now_ms)

    {turn_id, opened_at_ms, intent_payload, text} =
      cond do
        is_map(pending) and not too_old?(pending.opened_at_ms, now_ms, state.pending_max_age_ms) ->
          {pending.id, pending.opened_at_ms, pending.intent || state.last_intent,
           pending.text || event_text ||
             extract_text_any(state.last_intent, state.last_blackboard)}

        is_map(recent_turn) ->
          {mget(recent_turn, :turn_id),
           mget(recent_turn, :opened_at_ms) || mget(recent_turn, :at_ms),
           mget(recent_turn, :intent) || state.last_intent,
           event_text || mget(recent_turn, :text)}

        true ->
          {new_turn_id(), now_ms, state.last_intent,
           event_text || extract_text_any(state.last_intent, state.last_blackboard)}
      end

    trigger = %{
      kind: :telemetry,
      event: mget(bb_env, :event),
      measurements: mget(bb_env, :measurements) || %{},
      meta: mget(bb_env, :meta) || %{},
      at_ms: mget(bb_env, :at_ms) || now_ms
    }

    # Use the Stage1 stop event payload for this turn instead of sampling latest LIFG state,
    # which can race with another turn.
    state2 =
      case lifg_state_from_pipeline_stop(bb_env) do
        nil -> state
        lifg_state -> %{state | last_lifg: lifg_state}
      end

    rec = build_turn_record(state2, trigger, turn_id, opened_at_ms, intent_payload, text)

    state2
    |> cancel_pending_timer()
    |> publish_and_push(rec, :append_or_replace)
    |> Map.put(:pending, nil)
  end

  defp lifg_state_from_pipeline_stop(%{} = bb_env) do
    meta = mget(bb_env, :meta) || %{}

    %{
      region: :lifg,
      last: %{
        meta: meta,
        tokens: mget(meta, :tokens) || [],
        source: mget(meta, :source),
        guards: %{
          chargram_violation: mget(meta, :chargram_violation),
          missing_candidate_tokens: mget(meta, :missing_candidate_tokens) || [],
          missing_candidates: mget(meta, :missing_candidates),
          rejected_by_boundary: mget(meta, :rejected_by_boundary) || []
        },
        intent: mget(meta, :intent),
        confidence: mget(meta, :confidence),
        feature_mix: :lifg_stage1,
        ts_ms: mget(meta, :ts_ms),
        choices: mget(meta, :choices) || [],
        audit: %{
          boundary_drops: mget(meta, :boundary_drops),
          chargram_violation: mget(meta, :chargram_violation),
          dropped_tokens: mget(meta, :dropped_tokens),
          kept_tokens: mget(meta, :kept_tokens),
          missing_candidate_tokens: mget(meta, :missing_candidate_tokens) || [],
          missing_candidates: mget(meta, :missing_candidates),
          rejected_by_boundary: mget(meta, :rejected_by_boundary) || [],
          weak_decisions: mget(meta, :weak_decisions),
          guard_drops: mget(meta, :guard_drops),
          mwe_fallbacks: mget(meta, :mwe_fallbacks)
        },
        finalists: mget(meta, :finalists) || [],
        si_sentence: mget(meta, :sentence)
      }
    }
  end

  defp lifg_state_from_pipeline_stop(_), do: nil

  defp upgrade_on_response_complete(state, bb_env) do
    now_ms = now_ms()
    response = response_block_from_complete(bb_env, now_ms)
    user_text = response |> mget(:metadata) |> mget(:user_text)

    case turn_for_response_complete(state, user_text, now_ms) do
      %{} = turn ->
        upgraded =
          turn
          |> Map.put(:response, response)
          |> Map.put(:at_ms, now_ms)

        state
        |> publish_and_push(upgraded, :append_or_replace)

      nil ->
        state
    end
  end

  defp response_block_from_complete(%{} = bb_env, now_ms) do
    meta = mget(bb_env, :meta) || %{}
    measurements = mget(bb_env, :measurements) || %{}

    %{
      assistant_text: mget(meta, :assistant_text),
      assistant_chars: mget(measurements, :assistant_chars),
      user_chars: mget(measurements, :user_chars),
      tone: mget(meta, :tone),
      mode: mget(meta, :mode),
      response_profile: mget(meta, :response_profile),
      prompt_response_profile: mget(meta, :prompt_response_profile),
      simulated_affect: mget(meta, :simulated_affect),
      personality_state: mget(meta, :personality_state),
      system_sha256: mget(meta, :system_sha256),
      symbolic_frame: mget(meta, :symbolic_frame),
      metadata: meta,
      at_ms: mget(bb_env, :at_ms) || now_ms
    }
    |> drop_nil_values()
  end

  defp turn_for_response_complete(state, user_text, now_ms) do
    pending_id =
      case state.pending do
        %{id: id} -> id
        _ -> nil
      end

    by_pending =
      if is_nil(pending_id) do
        nil
      else
        find_recent_turn(state, fn turn -> mget(turn, :turn_id) == pending_id end)
      end

    by_text =
      if is_binary(user_text) and user_text != "" do
        find_recent_turn(state, fn turn -> same_turn_text?(mget(turn, :text), user_text) end)
      end

    by_recency =
      case state.last_turn do
        %{} = turn ->
          at_ms = mget(turn, :at_ms)

          if is_integer(at_ms) and now_ms - at_ms <= state.pending_max_age_ms do
            turn
          end

        _ ->
          nil
      end

    by_pending || by_text || by_recency
  end

  defp find_recent_turn(state, predicate) when is_function(predicate, 1) do
    (state.turns || [])
    |> Enum.find(fn
      %{} = turn -> predicate.(turn)
      _ -> false
    end)
  end

  defp same_turn_text?(turn_text, user_text) when is_binary(turn_text) and is_binary(user_text) do
    turn_text == user_text or String.starts_with?(turn_text, user_text) or
      String.starts_with?(user_text, turn_text)
  end

  defp same_turn_text?(_, _), do: false

  # ───────────────────────── Record construction ─────────────────────────

  defp build_turn_record(state, trigger, turn_id, opened_at_ms, intent_payload, text) do
    now_ms = now_ms()

    %{
      v: 1,
      turn_id: turn_id,
      opened_at_ms: opened_at_ms,
      at_ms: now_ms,

      # Training-grade input text (best-effort)
      text: if(is_binary(text) and text != "", do: text, else: nil),

      # Snapshot-ish features (latest known)
      clock: state.last_clock,
      intent: intent_payload || state.last_intent,
      mood: state.last_mood,
      wm: state.last_wm,

      # LIFG (winners for explainability)
      lifg: lifg_block(state),

      # The trigger
      trigger: %{
        kind: mget(trigger, :kind),
        event: mget(trigger, :event),
        measurements: mget(trigger, :measurements) || %{},
        meta: mget(trigger, :meta) || %{},
        at_ms: mget(trigger, :at_ms) || now_ms
      }
    }
  end

  defp extract_text_any(intent_payload, bb_payload) do
    t =
      mget(intent_payload || %{}, :text) ||
        mget(intent_payload || %{}, :sentence) ||
        mget(bb_payload || %{}, :text)

    if is_binary(t) and t != "", do: t, else: nil
  end

  defp lifg_block(state), do: MLLIFG.build_block(state.last_lifg, state.hydrate_lex?)

  # ───────────────────────── PubSub helpers ─────────────────────────

  defp safe_subscribe_all(topics) do
    try do
      Enum.each(topics, &Bus.subscribe/1)
      :ok
    rescue
      e -> {:error, e}
    catch
      :exit, reason -> {:error, reason}
    end
  end

  defp safe_broadcast(topic, msg) do
    try do
      Bus.broadcast(topic, msg)
      :ok
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp safe_blackboard_publish(payload) do
    try do
      Brain.Blackboard.publish(payload)
      :ok
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  # ───────────────────────── Publishing ─────────────────────────

  defp publish_turn_record(rec, state) do
    _ = safe_broadcast(@ml_topic, {:ml_turn, rec})

    if state.publish_blackboard? and Code.ensure_loaded?(Brain.Blackboard) do
      _ =
        safe_blackboard_publish(%{
          kind: :ml_turn,
          region: :ml,
          at_ms: rec.at_ms,
          v: rec.v,
          turn_id: rec.turn_id,
          hint: "turn record (ML)",
          turn:
            Map.take(rec, [
              :v,
              :turn_id,
              :opened_at_ms,
              :at_ms,
              :text,
              :intent,
              :mood,
              :wm,
              :lifg,
              :response,
              :trigger
            ])
        })

      :ok
    else
      :ok
    end
  end

  # Push new turns, but if we’re “upgrading” an already-backstopped turn_id, replace it.
  defp publish_and_push(state, rec, mode) do
    publish_turn_record(rec, state)

    turns0 = state.turns || []

    turns1 =
      case mode do
        :append_or_replace -> replace_or_prepend(turns0, rec, state.keep_turns)
        _ -> [rec | turns0] |> Enum.take(state.keep_turns)
      end

    %{state | last_turn: rec, turns: turns1}
  end

  defp replace_or_prepend(turns, %{turn_id: tid} = rec, keep) when is_list(turns) do
    {found?, turns2} =
      Enum.reduce(turns, {false, []}, fn t, {found, acc} ->
        if is_map(t) and mget(t, :turn_id) == tid do
          {true, [rec | acc]}
        else
          {found, [t | acc]}
        end
      end)

    turns3 = turns2 |> Enum.reverse()

    if found?, do: turns3 |> Enum.take(keep), else: [rec | turns] |> Enum.take(keep)
  end

  defp replace_or_prepend(_turns, rec, keep), do: [rec] |> Enum.take(keep)

  defp recent_turn_for_text(state, text, now_ms) when is_binary(text) and text != "" do
    case state.last_turn do
      %{} = turn ->
        turn_text = mget(turn, :text)
        turn_at = mget(turn, :at_ms)

        if turn_text == text and is_integer(turn_at) and now_ms - turn_at <= 500 do
          turn
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp recent_turn_for_text(_state, _text, _now_ms), do: nil

  defp too_old?(opened_at_ms, now_ms, max_age_ms)
       when is_integer(opened_at_ms) and is_integer(now_ms) and is_integer(max_age_ms) do
    now_ms - opened_at_ms > max_age_ms
  end

  defp too_old?(_, _, _), do: false

  defp new_turn_id, do: :erlang.unique_integer([:positive, :monotonic])
  defp now_ms, do: System.system_time(:millisecond)

  # ───────────────────────── Small utilities ─────────────────────────

  defp truthy?(v) when v in [true, "true", true, 1, "1", "yes", "on"], do: true
  defp truthy?(_), do: false

  defp mget(%{} = m, k), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp drop_nil_values(%{} = map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Enum.into(%{})
  end
end
