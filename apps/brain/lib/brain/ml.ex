defmodule Brain.ML do
  @moduledoc """
  Brain.ML — turn-level feature assembly for Axon training and UI explainability.

  Responsibilities (stub + usable):
  - Subscribe to high-signal Brain PubSub topics (same ones the UI uses) plus "brain:blackboard".
  - Track the latest payload per topic (intent/mood/lifg/wm/etc).
  - On Stage-1 pipeline stop (bridged as telemetry onto the blackboard), assemble a stable,
    versioned "turn record" suitable for:
      • dataset logging (JSONL / DB later)
      • feature extraction for Axon
      • optional UI display (via blackboard or a dedicated "brain:ml" topic)

  Guardrails:
  - Brain.ML does not depend on Core.
  - Best-effort behavior; failures should not crash the system.
  """

  use Brain, region: :ml
  require Logger

  alias Brain.Bus

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

  # Blackboard bridges this telemetry, so we finalize on that event.
  @pipeline_stop_event [:brain, :pipeline, :lifg_stage1, :stop]

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
  def handle_info({:intent, m}, state), do: {:noreply, %{state | last_intent: m}}
  def handle_info({:mood, m}, state), do: {:noreply, %{state | last_mood: m}}
  def handle_info({:lifg_update, m}, state), do: {:noreply, %{state | last_lifg: m}}
  def handle_info({:wm, m}, state), do: {:noreply, %{state | last_wm: m}}

  # Catch-all: stay resilient
  def handle_info(_msg, state), do: {:noreply, state}

  # ───────────────────────── Turn assembly ─────────────────────────

  defp maybe_finalize_turn_from_blackboard(state, %{} = env) do
    # Blackboard module publishes bridged telemetry as:
    # %{kind: :telemetry, event: [...], measurements: ..., meta: ..., at_ms: ...}
    kind = mget(env, :kind)

    if kind == :telemetry and (mget(env, :event) == @pipeline_stop_event) do
      rec = build_turn_record(state, env)
      publish_turn_record(rec, state)
      push_turn(state, rec)
    else
      state
    end
  end

  defp maybe_finalize_turn_from_blackboard(state, _), do: state

  defp build_turn_record(state, bb_env) do
    now_ms = System.system_time(:millisecond)

    %{
      v: 1,
      at_ms: now_ms,

      # Training-grade input text (best-effort; sourced from intent payload)
      text: extract_text_any(state.last_intent, state.last_blackboard),

      # Snapshot-ish features (latest known)
      clock: state.last_clock,
      intent: state.last_intent,
      mood: state.last_mood,
      wm: state.last_wm,
      lifg: lifg_block(state),

      # The trigger event (pipeline stop)
      trigger: %{
        kind: :telemetry,
        event: mget(bb_env, :event),
        measurements: mget(bb_env, :measurements) || %{},
        meta: mget(bb_env, :meta) || %{},
        at_ms: mget(bb_env, :at_ms) || now_ms
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

  defp lifg_block(state) do
    lifg = state.last_lifg

    winners =
      lifg
      |> extract_lifg_choices()
      |> Enum.map(&normalize_choice/1)
      |> maybe_hydrate_lex(state.hydrate_lex?)

    %{
      last_update: lifg,
      winners: winners
    }
  end

  defp push_turn(state, rec) do
    turns = [rec | (state.turns || [])] |> Enum.take(state.keep_turns)
    %{state | last_turn: rec, turns: turns}
  end

  defp publish_turn_record(rec, state) do
    # Dedicated ML topic for future consumers (Axon trainer, UI panel, logger)
    _ = safe_broadcast(@ml_topic, {:ml_turn, rec})

    # Optional: echo onto blackboard so your existing dashboard feed can show it
    if state.publish_blackboard? and Code.ensure_loaded?(Brain.Blackboard) do
      _ =
        safe_blackboard_publish(%{
          kind: :ml_turn,
          region: :ml, # ensures BrainLive.bb_tag/1 will tag it cleanly
          at_ms: rec.at_ms,
          v: rec.v,
          hint: "turn record (ML)",
          turn: Map.take(rec, [:v, :at_ms, :text, :intent, :mood, :wm, :lifg, :trigger])
        })

      :ok
    else
      :ok
    end
  end

  # ───────────────────────── Choice extraction + hydration ─────────────────────────

  defp extract_lifg_choices(nil), do: []

  defp extract_lifg_choices(%{} = m) do
    (mget(m, :choices) ||
       mget(m, :winners) ||
       mget_in(m, [:out, :choices]) ||
       mget_in(m, [:lifg, :choices]) ||
       [])
    |> List.wrap()
    |> Enum.filter(&is_map/1)
  end

  defp extract_lifg_choices(_), do: []

  defp normalize_choice(%{} = ch) do
    id =
      ch[:chosen_id] ||
        ch["chosen_id"] ||
        ch[:id] ||
        ch["id"]

    %{
      token_index: ch[:token_index] || ch["token_index"],
      lemma: ch[:lemma] || ch["lemma"],
      chosen_id: id,
      margin: ch[:margin] || ch["margin"],
      score: ch[:score] || ch["score"] || ch[:prob] || ch["prob"],
      scores: ch[:scores] || ch["scores"],
      alt_ids: ch[:alt_ids] || ch["alt_ids"] || []
    }
  end

  defp maybe_hydrate_lex(winners, false), do: winners

  defp maybe_hydrate_lex(winners, true) when is_list(winners) do
    Enum.map(winners, fn w ->
      id = w[:chosen_id]

      lex =
        if is_binary(id) and Code.ensure_loaded?(Db) and Code.ensure_loaded?(Db.BrainCell) do
          safe_fetch_cell_lex(id)
        else
          nil
        end

      if is_map(lex), do: Map.put(w, :lex, lex), else: w
    end)
  end

  defp safe_fetch_cell_lex(id) do
    try do
      case Db.get(Db.BrainCell, id) do
        nil ->
          nil

        row ->
          %{
            id: row.id,
            word: Map.get(row, :word),
            pos: Map.get(row, :pos),
            type: Map.get(row, :type),
            definition: Map.get(row, :definition),
            example: Map.get(row, :example),
            synonyms: Map.get(row, :synonyms),
            antonyms: Map.get(row, :antonyms)
          }
      end
    rescue
      _ -> nil
    catch
      _, _ -> nil
    end
  end

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

  # ───────────────────────── Small utilities ─────────────────────────

  defp truthy?(v) when v in [true, "true", :true, 1, "1", "yes", "on"], do: true
  defp truthy?(_), do: false

  defp mget(%{} = m, k), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp mget_in(m, [k | rest]) when is_map(m) do
    case mget(m, k) do
      %{} = next -> mget_in(next, rest)
      other -> other
    end
  end

  defp mget_in(_, _), do: nil
end

