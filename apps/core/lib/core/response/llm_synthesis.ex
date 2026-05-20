defmodule Core.Response.LlmSynthesis do
  @moduledoc """
  LLM-backed response synthesis for Core.Response with bounded multi-turn context.

  Stores recent turn history in ETS keyed by `session_id`.

  ETS ownership note:
  ETS tables are owned by the creating process. If LLM calls happen inside
  short-lived Task processes, ETS would disappear when the Task exits. To avoid
  that, we attempt to create the ETS table with a long-lived `:heir` process.
  """

  require Logger

  alias Core.Telemetry
  alias Core.Response.Context
  alias Core.Response.LlmPrompt
  alias Core.Response.SelfStateSummary

  # Suppress compile-time warnings if optional apps/modules are not built/loaded yet.
  @compile {:no_warn_undefined, Llm}
  @compile {:no_warn_undefined, Brain}
  @compile {:no_warn_undefined, Brain.Introspection}
  @compile {:no_warn_undefined, Brain.Introspect}
  @compile {:no_warn_undefined, Brain.MoodCore}
  @compile {:no_warn_undefined, Brain.SelfPortrait}

  @timeout_ms 15_000

  # Keep N user/assistant turn pairs (N*2 messages).
  @history_turn_pairs 6

  # Per-message clamp for stored history.
  @max_item_chars 1_600

  @ets_table :core_llm_chat_history
  @heir_data :core_llm_chat_history

  # Toggle prompt logging (dev-only recommended):
  #   config :core, :log_llm_prompts?, true

  # Safety caps so logs don't explode.
  @max_system_chars 8_000
  @max_user_chars 2_000

  # ── Public API ────────────────────────────────────────────────────────────

  @spec generate(String.t(), map(), map(), map()) :: {:ok, String.t()} | {:error, term()}
  def generate(user_text, features, decision, mood) do
    if llm_available?() do
      do_generate(user_text, features, decision, mood)
    else
      {:error, :llm_not_available}
    end
  end

  @spec record_turn(term(), String.t(), String.t()) :: :ok
  def record_turn(session_id, user_text, assistant_text) do
    ensure_table!()
    remember(session_id, user_text, assistant_text)
  end

  # ── Internal ──────────────────────────────────────────────────────────────

  defp do_generate(user_text, features, decision, mood) do
    ensure_table!()

    context = prompt_context(user_text, features, decision, mood)
    session_id = Map.get(context, :session_id, :global)

    system_prompt = LlmPrompt.build_system_prompt(context)
    emit_prompt_event(system_prompt, user_text, context)
    history = history_messages(session_id, @history_turn_pairs)

    messages =
      [%{"role" => "system", "content" => system_prompt}] ++
        history ++
        [%{"role" => "user", "content" => user_text}]

    if log_prompts?() do
      log_prompt_bundle(messages, context)
    end

    case llm_client().chat(messages, timeout: @timeout_ms) do
      {:ok, %{content: content}} when is_binary(content) and content != "" ->
        out = String.trim(content)
        remember(session_id, user_text, out)
        emit_complete_event(user_text, out, context, system_prompt)
        {:ok, out}

      {:ok, other} ->
        Logger.debug("[LlmSynthesis] Unexpected LLM response: #{inspect(other)}")
        {:error, :unexpected_response}

      {:error, reason} ->
        Logger.debug("[LlmSynthesis] LLM error: #{inspect(reason)}")
        {:error, reason}
    end
  rescue
    e ->
      Logger.debug("[LlmSynthesis] Exception: #{Exception.message(e)}")
      {:error, :exception}
  catch
    :exit, reason ->
      Logger.debug("[LlmSynthesis] Exit: #{inspect(reason)}")
      {:error, :exit}
  end

  defp prompt_context(user_text, features, decision, mood) do
    wm_items = safe_wm_items()
    self_model = safe_self_model()
    runtime_state = safe_runtime_state(wm_items)

    features =
      features
      |> ensure_map()
      |> put_if_missing(:self_model, self_model)
      |> put_if_missing(:runtime_state, runtime_state)

    Context.from_response_parts(user_text, features, ensure_map(decision), ensure_map(mood), %{
      wm_items: wm_items,
      self_model: self_model,
      runtime_state: runtime_state
    })
  end

  # ── ETS initialization / ownership ─────────────────────────────────────────

  defp ensure_table! do
    case :ets.whereis(@ets_table) do
      :undefined ->
        heir = heir_pid()

        opts =
          [
            :named_table,
            :public,
            :set,
            {:read_concurrency, true},
            {:write_concurrency, true}
          ] ++
            if is_pid(heir) do
              [{:heir, heir, @heir_data}]
            else
              []
            end

        _tid = :ets.new(@ets_table, opts)

        Logger.debug(
          "[LlmSynthesis] ETS created table=#{inspect(@ets_table)} owner=#{inspect(self())} heir=#{inspect(heir)}"
        )

        :ok

      _tid ->
        :ok
    end
  end

  # Prefer a long-lived process as heir so ETS survives Task exit.
  # 1) Llm GenServer (best)
  # 2) Umbrella Task supervisor (if registered)
  # 3) nil (no heir)
  defp heir_pid do
    cond do
      Code.ensure_loaded?(Llm) and is_pid(Process.whereis(Llm)) ->
        Process.whereis(Llm)

      Code.ensure_loaded?(Symbrella.TaskSup) and is_pid(Process.whereis(Symbrella.TaskSup)) ->
        Process.whereis(Symbrella.TaskSup)

      true ->
        nil
    end
  end

  # ── History store (ETS) ───────────────────────────────────────────────────

  defp history_messages(session_id, turn_pairs) when is_integer(turn_pairs) and turn_pairs > 0 do
    msgs =
      case :ets.lookup(@ets_table, session_id) do
        [{^session_id, list}] when is_list(list) -> list
        _ -> []
      end

    take_n = min(length(msgs), turn_pairs * 2)
    Enum.take(msgs, -take_n)
  end

  defp history_messages(_session_id, _turn_pairs), do: []

  defp remember(session_id, user_text, assistant_text) do
    user_msg = %{"role" => "user", "content" => clamp_text(user_text)}
    asst_msg = %{"role" => "assistant", "content" => clamp_text(assistant_text)}

    prev =
      case :ets.lookup(@ets_table, session_id) do
        [{^session_id, list}] when is_list(list) -> list
        _ -> []
      end

    next =
      (prev ++ [user_msg, asst_msg])
      |> trim_history(@history_turn_pairs)

    :ets.insert(@ets_table, {session_id, next})
    :ok
  rescue
    e ->
      Logger.warning("[LlmSynthesis] remember failed: #{Exception.message(e)}")
      :ok
  catch
    :exit, reason ->
      Logger.warning("[LlmSynthesis] remember exit: #{inspect(reason)}")
      :ok
  end

  defp trim_history(list, turn_pairs) do
    max_msgs = max(0, turn_pairs * 2)
    if length(list) <= max_msgs, do: list, else: Enum.take(list, -max_msgs)
  end

  defp clamp_text(text) when is_binary(text) do
    t = String.trim(text)

    cond do
      t == "" -> ""
      String.length(t) <= @max_item_chars -> t
      true -> String.slice(t, 0, @max_item_chars) <> "…"
    end
  end

  defp clamp_text(other), do: other |> to_string() |> clamp_text()

  # ── Prompt logging (safe + capped) ─────────────────────────────────────────

  defp log_prompts? do
    Application.get_env(:core, :log_llm_prompts?, false) == true
  end

  defp log_prompt_bundle(messages, context) do
    req = :erlang.unique_integer([:positive])
    features = Map.get(context, :features, %{})
    decision = Map.get(context, :decision, %{})
    mood = Map.get(context, :mood, %{})
    self_model = Map.get(context, :self_model)

    intent = Map.get(features, :intent)
    mode = Map.get(decision, :mode)
    tone = Map.get(decision, :tone)
    tone_hint = Map.get(mood, :tone_hint)

    {system, user} = extract_system_user(messages)
    {sys2, sys_trunc?} = cap_text(system, @max_system_chars)
    {usr2, usr_trunc?} = cap_text(user, @max_user_chars)

    Logger.info("""
    [LlmSynthesis] PROMPT_BEGIN req=#{req} intent=#{inspect(intent)} mode=#{inspect(mode)} tone=#{inspect(tone)} tone_hint=#{inspect(tone_hint)} self_model=#{inspect(self_model_log(self_model))}
    ---SYSTEM sha256=#{sha256_hex(system)} chars=#{String.length(system)} truncated=#{sys_trunc?}---
    #{sys2}
    ---USER sha256=#{sha256_hex(user)} chars=#{String.length(user)} truncated=#{usr_trunc?}---
    #{usr2}
    PROMPT_END
    """)
  rescue
    e ->
      Logger.debug("[LlmSynthesis] prompt log failed: #{Exception.message(e)}")
      :ok
  end

  defp emit_prompt_event(system_prompt, user_text, context) do
    {system_preview, system_truncated?} = cap_text(system_prompt, @max_system_chars)
    {user_preview, user_truncated?} = cap_text(user_text, @max_user_chars)
    prompt_fields = prompt_fields(system_prompt)

    Telemetry.emit(
      [:core, :response, :prompt],
      %{
        system_chars: String.length(system_prompt),
        user_chars: String.length(to_string(user_text || ""))
      },
      %{
        session_id: Map.get(context, :session_id, :global),
        intent: get_in_map(context, [:features, :intent]),
        mode: get_in_map(context, [:decision, :mode]),
        tone: get_in_map(context, [:decision, :tone]),
        response_profile: Map.get(prompt_fields, :response_profile),
        simulated_affect: Map.get(prompt_fields, :simulated_affect),
        personality_state: Map.get(prompt_fields, :personality_state),
        system_sha256: sha256_hex(system_prompt),
        system_prompt: system_preview,
        system_truncated?: system_truncated?,
        user_text: user_preview,
        user_truncated?: user_truncated?
      }
    )
  rescue
    e ->
      Logger.debug("[LlmSynthesis] prompt telemetry failed: #{Exception.message(e)}")
      :ok
  end

  defp emit_complete_event(user_text, assistant_text, context, system_prompt) do
    {user_preview, user_truncated?} = cap_text(user_text, @max_user_chars)
    {assistant_preview, assistant_truncated?} = cap_text(assistant_text, @max_user_chars)
    prompt_fields = prompt_fields(system_prompt)
    prompt_profile = Map.get(prompt_fields, :response_profile)

    Telemetry.emit(
      [:core, :response, :complete],
      %{
        user_chars: String.length(to_string(user_text || "")),
        assistant_chars: String.length(to_string(assistant_text || ""))
      },
      %{
        session_id: Map.get(context, :session_id, :global),
        intent: get_in_map(context, [:features, :intent]),
        mode: get_in_map(context, [:decision, :mode]),
        tone: get_in_map(context, [:decision, :tone]),
        response_profile: response_profile_value(prompt_profile, context),
        prompt_response_profile: prompt_profile,
        simulated_affect: Map.get(prompt_fields, :simulated_affect),
        personality_state: Map.get(prompt_fields, :personality_state),
        system_sha256: sha256_hex(system_prompt),
        symbolic_frame: Map.get(context, :symbolic_frame),
        user_text: user_preview,
        user_truncated?: user_truncated?,
        assistant_text: assistant_preview,
        assistant_truncated?: assistant_truncated?
      }
    )
  rescue
    e ->
      Logger.debug("[LlmSynthesis] complete telemetry failed: #{Exception.message(e)}")
      :ok
  end

  defp extract_system_user(messages) when is_list(messages) do
    sys =
      messages
      |> Enum.find(%{}, fn m -> Map.get(m, "role") == "system" end)
      |> Map.get("content", "")

    usr =
      messages
      |> Enum.reverse()
      |> Enum.find(%{}, fn m -> Map.get(m, "role") == "user" end)
      |> Map.get("content", "")

    {to_string(sys || ""), to_string(usr || "")}
  end

  defp extract_system_user(_), do: {"", ""}

  defp cap_text(text, max_chars)
       when is_binary(text) and is_integer(max_chars) and max_chars > 0 do
    if String.length(text) <= max_chars do
      {text, false}
    else
      {String.slice(text, 0, max_chars) <> "…", true}
    end
  end

  defp cap_text(other, max_chars), do: cap_text(to_string(other || ""), max_chars)

  defp sha256_hex(text) when is_binary(text) do
    :crypto.hash(:sha256, text)
    |> Base.encode16(case: :lower)
  end

  defp prompt_fields(system_prompt) when is_binary(system_prompt) do
    %{
      response_profile: prompt_line_value(system_prompt, "Response profile:"),
      simulated_affect: prompt_line_value(system_prompt, "Simulated affect:"),
      personality_state: prompt_line_value(system_prompt, "Personality state:")
    }
  end

  defp prompt_fields(_), do: %{}

  defp prompt_line_value(prompt, prefix) when is_binary(prompt) and is_binary(prefix) do
    prompt
    |> String.split("\n")
    |> Enum.find("", &String.starts_with?(&1, prefix))
    |> String.replace_prefix(prefix, "")
    |> String.trim()
    |> String.trim_trailing(".")
    |> blank_to_nil()
  end

  defp response_profile_value(prompt_profile, context) do
    prompt_profile
    |> existing_atom_value()
    |> case do
      nil -> get_in_map(context, [:decision, :response_profile])
      value -> value
    end
  end

  defp existing_atom_value(value) when is_atom(value), do: value

  defp existing_atom_value(value) when is_binary(value) do
    try do
      String.to_existing_atom(value)
    rescue
      ArgumentError -> value
    end
  end

  defp existing_atom_value(_), do: nil

  defp blank_to_nil(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp blank_to_nil(value), do: value

  defp get_in_map(map, keys) when is_map(map) and is_list(keys) do
    Enum.reduce_while(keys, map, fn key, acc ->
      case map_get(acc, key) do
        nil -> {:halt, nil}
        value -> {:cont, value}
      end
    end)
  end

  defp get_in_map(_, _), do: nil

  defp safe_wm_items do
    if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
      try do
        case Brain.snapshot_wm() do
          %{wm: wm} when is_list(wm) -> wm
          _ -> []
        end
      rescue
        _ -> []
      catch
        :exit, _ -> []
      end
    else
      []
    end
  end

  defp safe_self_model do
    cond do
      Code.ensure_loaded?(Brain.Introspection) and
          function_exported?(Brain.Introspection, :snapshot, 0) ->
        try do
          Brain.Introspection.snapshot()
        rescue
          _ -> nil
        catch
          :exit, _ -> nil
        end

      true ->
        nil
    end
  end

  defp safe_runtime_state(wm_items) do
    mood = safe_mood_snapshot()
    lifg = safe_lifg_runtime()
    wm = wm_runtime(wm_items)
    self_portrait = safe_self_portrait_snapshot()

    runtime = %{
      source: :brain,
      phase: :prompt_context,
      status: :ready,
      mood: mood_values(mood),
      neuromodulators: neuromodulator_values(mood),
      tone_hint: map_get(mood, :tone_hint),
      pressure_label: map_get(mood, :pressure_label),
      mood_trace: mood |> map_get(:mood_trace, []) |> List.wrap() |> Enum.take(3),
      wm: wm,
      lifg: lifg,
      self_portrait: self_portrait
    }

    Map.put(
      runtime,
      :self_state_summary,
      SelfStateSummary.prompt_line(%{
        mood: mood,
        self_portrait: self_portrait,
        wm: %{wm: wm_items, cfg: %{capacity: map_get(wm, :capacity)}},
        lifg: %{state: %{last: lifg}, running?: map_get(lifg, :running?)}
      })
    )
  end

  defp safe_self_portrait_snapshot do
    if Code.ensure_loaded?(Brain.SelfPortrait) and
         function_exported?(Brain.SelfPortrait, :snapshot, 0) do
      try do
        Brain.SelfPortrait.snapshot()
      rescue
        _ -> %{}
      catch
        :exit, _ -> %{}
      end
    else
      %{}
    end
  end

  defp safe_mood_snapshot do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :snapshot, 0) do
      try do
        Brain.MoodCore.snapshot()
      rescue
        _ -> %{}
      catch
        :exit, _ -> %{}
      end
    else
      %{}
    end
  end

  defp safe_lifg_runtime do
    if Code.ensure_loaded?(Brain.Introspect) and
         function_exported?(Brain.Introspect, :snapshot, 1) do
      try do
        Brain.Introspect.snapshot(:lifg)
        |> lifg_runtime_from_snapshot()
      rescue
        _ -> %{}
      catch
        :exit, _ -> %{}
      end
    else
      %{}
    end
  end

  defp lifg_runtime_from_snapshot(%{} = snapshot) do
    state = map_get(snapshot, :state, %{})
    last = map_get(state, :last, %{})
    audit = map_get(last, :audit, %{})
    guards = map_get(last, :guards, %{})
    meta = map_get(last, :meta, %{})
    choices = List.wrap(map_get(last, :choices, []))

    missing = number(map_get(guards, :missing_candidates) || map_get(audit, :missing_candidates))
    weak = number(map_get(audit, :weak_decisions))
    fallback = number(map_get(audit, :fallback_winners) || map_get(audit, :mwe_fallbacks))
    chargram = number(map_get(guards, :chargram_violation) || map_get(audit, :chargram_violation))
    boundary = boundary_count(guards, audit)
    acc_conflict = map_get(meta, :acc_conflict)

    degraded? =
      missing > 0 or weak > 0 or fallback > 0 or chargram > 0 or boundary > 0 or
        (is_number(acc_conflict) and acc_conflict >= 0.5)

    %{
      focused?: true,
      running?: map_get(snapshot, :running?) == true,
      intent: map_get(last, :intent),
      confidence: map_get(last, :confidence),
      choices_count: length(choices),
      missing_candidates: missing,
      weak_decisions: weak,
      fallback_winners: fallback,
      chargram_violations: chargram,
      boundary_drops: boundary,
      acc_conflict: acc_conflict,
      degraded?: degraded?
    }
  end

  defp lifg_runtime_from_snapshot(_), do: %{}

  defp wm_runtime(wm_items) when is_list(wm_items) do
    capacity =
      if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
        try do
          case Brain.snapshot_wm() do
            %{cfg: %{capacity: cap}} when is_number(cap) -> cap
            _ -> nil
          end
        rescue
          _ -> nil
        catch
          :exit, _ -> nil
        end
      end

    size = length(wm_items)
    load = if is_number(capacity) and capacity > 0, do: size / capacity, else: nil

    %{
      size: size,
      capacity: capacity,
      load: load,
      concepts: LlmPrompt.summarize_wm(wm_items)
    }
  end

  defp mood_values(mood) do
    case map_get(mood, :mood) do
      values when is_map(values) ->
        %{
          exploration: map_get(values, :exploration),
          inhibition: map_get(values, :inhibition),
          vigilance: map_get(values, :vigilance),
          plasticity: map_get(values, :plasticity)
        }

      _ ->
        %{}
    end
  end

  defp neuromodulator_values(mood) do
    case map_get(mood, :levels) do
      levels when is_map(levels) ->
        %{
          dopamine: map_get(levels, :da),
          serotonin: map_get(levels, :"5ht"),
          glutamate: map_get(levels, :glu),
          norepinephrine: map_get(levels, :ne)
        }

      _ ->
        %{}
    end
  end

  defp boundary_count(guards, audit) do
    rejected = map_get(guards, :rejected_by_boundary) || map_get(audit, :rejected_by_boundary)

    cond do
      is_list(rejected) -> length(rejected)
      is_binary(rejected) -> String.length(rejected)
      is_number(map_get(audit, :boundary_drops)) -> map_get(audit, :boundary_drops)
      true -> 0
    end
  end

  defp number(value) when is_integer(value), do: value
  defp number(value) when is_float(value), do: round(value)
  defp number(_), do: 0

  defp self_model_log(nil), do: nil

  defp self_model_log(model) do
    %{
      v: model_value(model, :v),
      confidence: model_value(model, :confidence),
      uncertainty: model_value(model, :uncertainty),
      stability: model_value(model, :stability),
      cognitive_load: model_value(model, :cognitive_load)
    }
  end

  defp model_value(model, key) when is_map(model), do: Map.get(model, key)

  defp model_value(_, _), do: nil

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default

  defp ensure_map(map) when is_map(map), do: map
  defp ensure_map(_), do: %{}

  defp put_if_missing(map, _key, nil), do: map

  defp put_if_missing(map, key, value) when is_map(map) do
    if Map.has_key?(map, key) or Map.has_key?(map, Atom.to_string(key)) do
      map
    else
      Map.put(map, key, value)
    end
  end

  # ── Helpers ───────────────────────────────────────────────────────────────

  defp llm_available? do
    client = llm_client()

    Code.ensure_loaded?(client) and
      function_exported?(client, :chat, 2) and
      is_pid(Process.whereis(client))
  end

  defp llm_client do
    Application.get_env(:core, :llm_client, Llm)
  end
end
