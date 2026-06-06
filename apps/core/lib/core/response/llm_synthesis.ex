defmodule Core.Response.LlmSynthesis do
  @moduledoc """
  LLM-backed response synthesis for Core.Response with bounded multi-turn context.

  Reads recent turn history from ETS keyed by `session_id`.
  `Core.Response.plan/2` owns recording via `record_turn/3`.

  ETS ownership note:
  ETS tables are owned by the creating process. If LLM calls happen inside
  short-lived Task processes, ETS would disappear when the Task exits. To avoid
  that, we attempt to create the ETS table with a long-lived `:heir` process.
  """

  require Logger

  alias Core.Telemetry
  alias Core.Response.Context
  alias Core.Response.LlmChatHistory
  alias Core.Response.LlmPrompt
  alias Core.Response.LlmPromptEvents
  alias Core.Response.ReflectionLoop
  alias Core.Response.RuntimeContext

  # Suppress compile-time warnings if optional apps/modules are not built/loaded yet.
  @compile {:no_warn_undefined, Llm}

  @timeout_ms 15_000

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
    LlmChatHistory.record_turn(session_id, user_text, assistant_text)
  end

  @spec history_status(term()) :: map()
  def history_status(session_id) do
    LlmChatHistory.status(session_id)
  end

  # ── Internal ──────────────────────────────────────────────────────────────

  defp do_generate(user_text, features, decision, mood) do
    LlmChatHistory.ensure_table!()

    context = prompt_context(user_text, features, decision, mood)
    session_id = Map.get(context, :session_id, :global)

    system_prompt = LlmPrompt.build_system_prompt(context)
    emit_prompt_event(system_prompt, user_text, context)
    history = LlmChatHistory.messages(session_id)

    messages =
      [%{"role" => "system", "content" => system_prompt}] ++
        history ++
        [%{"role" => "user", "content" => user_text}]

    if log_prompts?() do
      log_prompt_bundle(messages, context)
    end

    case llm_client().chat(messages, timeout: @timeout_ms) do
      {:ok, %{content: content}} when is_binary(content) and content != "" ->
        draft = sanitize_model_text(content)
        {:ok, out, reflection} = ReflectionLoop.review(user_text, draft, context)
        emit_complete_event(user_text, out, context, system_prompt, reflection)
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

  defp sanitize_model_text(text) when is_binary(text) do
    text
    |> String.replace(~r/<\|(?:im_(?:end|start)|eot_id|endoftext|end_of_text)(?:\|>)?/u, "")
    |> String.trim()
  end

  defp prompt_context(user_text, features, decision, mood) do
    runtime = RuntimeContext.snapshot()
    wm_items = runtime.wm_items
    self_model = runtime.self_model
    runtime_state = runtime.runtime_state
    session_id = Map.get(ensure_map(features), :session_id, :global)
    context_status = history_status(session_id)

    features =
      features
      |> ensure_map()
      |> put_if_missing(:context_status, context_status)
      |> put_if_missing(:self_model, self_model)
      |> put_if_missing(:runtime_state, runtime_state)

    Context.from_response_parts(user_text, features, ensure_map(decision), ensure_map(mood), %{
      wm_items: wm_items,
      self_model: self_model,
      runtime_state: runtime_state
    })
  end

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

    {system, user} = LlmPromptEvents.extract_system_user(messages)
    {sys2, sys_trunc?} = LlmPromptEvents.cap_text(system, @max_system_chars)
    {usr2, usr_trunc?} = LlmPromptEvents.cap_text(user, @max_user_chars)

    Logger.info("""
    [LlmSynthesis] PROMPT_BEGIN req=#{req} intent=#{inspect(intent)} mode=#{inspect(mode)} tone=#{inspect(tone)} tone_hint=#{inspect(tone_hint)} self_model=#{inspect(RuntimeContext.self_model_log(self_model))}
    ---SYSTEM sha256=#{LlmPromptEvents.sha256_hex(system)} chars=#{String.length(system)} truncated=#{sys_trunc?}---
    #{sys2}
    ---USER sha256=#{LlmPromptEvents.sha256_hex(user)} chars=#{String.length(user)} truncated=#{usr_trunc?}---
    #{usr2}
    PROMPT_END
    """)
  rescue
    e ->
      Logger.debug("[LlmSynthesis] prompt log failed: #{Exception.message(e)}")
      :ok
  end

  defp emit_prompt_event(system_prompt, user_text, context) do
    {measurements, metadata} = LlmPromptEvents.prompt(system_prompt, user_text, context)
    Telemetry.emit([:core, :response, :prompt], measurements, metadata)
  rescue
    e ->
      Logger.debug("[LlmSynthesis] prompt telemetry failed: #{Exception.message(e)}")
      :ok
  end

  defp emit_complete_event(user_text, assistant_text, context, system_prompt, reflection) do
    {measurements, metadata} =
      LlmPromptEvents.complete(user_text, assistant_text, context, system_prompt, reflection)

    Telemetry.emit([:core, :response, :complete], measurements, metadata)
  rescue
    e ->
      Logger.debug("[LlmSynthesis] complete telemetry failed: #{Exception.message(e)}")
      :ok
  end

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
