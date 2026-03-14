# apps/core/lib/core/response/llm_synthesis.ex
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

  # Suppress compile-time warnings if the llm app isn't built/loaded yet.
  @compile {:no_warn_undefined, Llm}

  @timeout_ms 15_000

  # Keep N user/assistant turn pairs (N*2 messages).
  @history_turn_pairs 6

  # Per-message clamp for stored history.
  @max_item_chars 1_600

  @ets_table :core_llm_chat_history
  @heir_data :core_llm_chat_history

  # Toggle prompt logging (dev-only recommended):
  #   config :core, :log_llm_prompts?, true
  @log_prompts? Application.compile_env(:core, :log_llm_prompts?, false)

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

    system_prompt = build_system_prompt(features, decision, mood)
    session_id = Map.get(features, :session_id, :global)

    history = history_messages(session_id, @history_turn_pairs)

    messages =
      [%{"role" => "system", "content" => system_prompt}] ++
        history ++
        [%{"role" => "user", "content" => user_text}]

    if @log_prompts? do
      log_prompt_bundle(messages, features, decision, mood)
    end

    case Llm.chat(messages, timeout: @timeout_ms) do
      {:ok, %{content: content}} when is_binary(content) and content != "" ->
        out = String.trim(content)
        remember(session_id, user_text, out)
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

  defp log_prompt_bundle(messages, features, decision, mood) do
    req = :erlang.unique_integer([:positive])
    intent = Map.get(features, :intent)
    mode = Map.get(decision, :mode)
    tone = Map.get(decision, :tone)
    tone_hint = Map.get(mood, :tone_hint)

    {system, user} = extract_system_user(messages)
    {sys2, sys_trunc?} = cap_text(system, @max_system_chars)
    {usr2, usr_trunc?} = cap_text(user, @max_user_chars)

    Logger.info("""
    [LlmSynthesis] PROMPT_BEGIN req=#{req} intent=#{inspect(intent)} mode=#{inspect(mode)} tone=#{inspect(tone)} tone_hint=#{inspect(tone_hint)}
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

  # ── System prompt builder ─────────────────────────────────────────────────

  defp build_system_prompt(features, decision, mood) do
    tone = decision.tone
    mode = decision.mode
    intent = features.intent
    exp = getv(mood, :exploration)
    inh = getv(mood, :inhibition)
    vig = getv(mood, :vigilance)
    plast = getv(mood, :plasticity)
    tone_hint = Map.get(mood, :tone_hint)

    wm_summary = safe_wm_summary()

    """
    You are Symbrella, a brain-inspired AI assistant.
    #{tone_directive(tone, tone_hint)}
    #{mood_context(exp, inh, vig, plast)}
    #{mode_directive(mode, intent)}
    #{wm_context(wm_summary)}
    Keep your response concise and directly relevant to the user's input.
    Do not explain your reasoning. Just respond naturally.
    """
    |> String.trim()
  end

  defp tone_directive(:warm, _), do: "Respond in a warm, engaged, and encouraging tone."

  defp tone_directive(:deescalate, _),
    do: "Respond calmly and gently. Keep things grounded and constructive."

  defp tone_directive(:firm, _), do: "Respond clearly and directly. Stay focused and purposeful."

  defp tone_directive(:neutral, :deescalate),
    do: "Respond in a measured, steady tone. Things are settling down."

  defp tone_directive(:neutral, _), do: "Respond in a balanced, clear tone."
  defp tone_directive(_, _), do: "Respond helpfully and clearly."

  defp mood_context(exp, inh, vig, plast) do
    []
    |> maybe_add(exp > 0.65, "You feel curious and ready to explore.")
    |> maybe_add(exp < 0.35, "You are in a conservative, careful state.")
    |> maybe_add(vig > 0.80, "Vigilance is elevated — stay measured.")
    |> maybe_add(inh > 0.70, "Inhibition is high — keep things calm.")
    |> maybe_add(plast > 0.65, "You are in a receptive, learning-ready state.")
    |> case do
      [] -> ""
      notes -> "Current mood: " <> Enum.join(notes, " ")
    end
  end

  defp maybe_add(notes, true, note), do: notes ++ [note]
  defp maybe_add(notes, false, _), do: notes

  defp mode_directive(:pair_programmer, _),
    do: "You are acting as a pair programmer. Be concise, action-oriented, and practical."

  defp mode_directive(:coach, :bug),
    do: "You are coaching through a bug. Be patient, methodical, and encouraging."

  defp mode_directive(:coach, _),
    do: "You are coaching. Guide toward a small, clear next step."

  defp mode_directive(:explainer, _),
    do: "You are explaining a concept. Be clear and succinct — 2-4 sentences."

  defp mode_directive(:scribe, _),
    do: "You are in a conversational mode. Keep it natural and brief."

  defp mode_directive(:editor, _),
    do: "You are reviewing carefully. Point out concerns clearly but constructively."

  defp mode_directive(_, _),
    do: "Respond helpfully."

  defp safe_wm_summary do
    if Code.ensure_loaded?(Brain) and function_exported?(Brain, :snapshot_wm, 0) do
      try do
        %{wm: wm} = Brain.snapshot_wm()

        wm
        |> Enum.take(5)
        |> Enum.map(fn item ->
          item[:payload][:lemma] ||
            item[:lemma] ||
            to_string(item[:id] || "")
        end)
        |> Enum.reject(&(&1 == ""))
        |> Enum.uniq()
      rescue
        _ -> []
      catch
        :exit, _ -> []
      end
    else
      []
    end
  end

  defp wm_context([]), do: ""
  defp wm_context(lemmas), do: "Active concepts: #{Enum.join(lemmas, ", ")}."

  # ── Helpers ───────────────────────────────────────────────────────────────

  defp llm_available? do
    Code.ensure_loaded?(Llm) and
      function_exported?(Llm, :chat, 2) and
      is_pid(Process.whereis(Llm))
  end

  defp getv(mood, key) do
    case {get_in(mood, [:mood, key]), Map.get(mood, key)} do
      {v, _} when is_number(v) -> v * 1.0
      {_, v} when is_number(v) -> v * 1.0
      _ -> 0.5
    end
  end
end
