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
  alias Core.Response.AnswerPolish
  alias Core.Response.LlmChatHistory
  alias Core.Response.LlmPrompt
  alias Core.Response.LlmPromptEvents
  alias Core.Response.ResponseFrame
  alias Core.Response.EvidenceRevision
  alias Core.Response.GroundedRevision
  alias Core.Response.ReflectionLoop
  alias Core.Response.AnswerAdequacy
  alias Core.Response.RuntimeContext
  alias Core.Tools.WebRetrieval

  # Suppress compile-time warnings if optional apps/modules are not built/loaded yet.
  @compile {:no_warn_undefined, Llm}

  # Toggle prompt logging (dev-only recommended):
  #   config :core, :log_llm_prompts?, true

  # Safety caps so logs don't explode.
  @max_system_chars 8_000
  @max_user_chars 2_000

  # ── Public API ────────────────────────────────────────────────────────────

  @spec generate(String.t(), map(), map(), map(), atom() | nil) ::
          {:ok, String.t(), map()} | {:error, term()}
  def generate(user_text, features, decision, mood, prompt_type) do
    if llm_available?() do
      do_generate(user_text, features, decision, mood, prompt_type)
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

  defp do_generate(user_text, features, decision, mood, prompt_type) do
    LlmChatHistory.ensure_table!()

    raw_context = prompt_context(user_text, features, decision, mood)
    session_id = Map.get(raw_context, :session_id, :global)

    # Retrieval follow-ups may resolve "search for it" using a bounded
    # window of this session's prior USER turns. The history is not copied
    # into the response context or trusted as external evidence. A separate
    # short-lived, same-session receipt supports audit follow-ups such as
    # "well did you?" even when no new retrieval is requested.
    previous_retrieval = LlmChatHistory.recent_retrieval(session_id)

    enriched =
      WebRetrieval.enrich(raw_context,
        history: LlmChatHistory.messages(session_id, 3))

    context =
      if is_nil(Map.get(enriched, :web_evidence)) and is_map(previous_retrieval),
        do: Map.put(enriched, :prior_retrieval, previous_retrieval),
        else: enriched

    LlmChatHistory.record_retrieval(session_id, Map.get(context, :web_evidence))

    history = LlmChatHistory.messages(session_id, history_turn_pairs())
    answer_target = AnswerAdequacy.answer_target(user_text, history)

    context =
      if is_binary(answer_target) do
        Map.put(context, :answer_target_question, answer_target)
      else
        context
      end

    # Give the model Symbrella's current cognitive state rather than a
    # sentence-shaped factual answer. History informs continuity, but never
    # becomes a source of external factual verification.
    context =
      Map.put(context, :response_frame, ResponseFrame.build(context, history))

    system_prompt =
      build_prompt(context, prompt_type)
      |> then(fn prompt ->
        if is_binary(answer_target) and
             AnswerAdequacy.factual_question?(answer_target) and
             String.trim(answer_target) != String.trim(user_text) do
          prompt <> "\nThe latest user turn requests an answer to this previous " <>
            "question from the SAME session: " <> inspect(answer_target) <>
            ". Answer that question directly; do not repeat it or answer " <>
            "the follow-up as a standalone statement."
        else
          prompt
        end
      end)

    emit_prompt_event(system_prompt, user_text, context)

    messages =
      [%{"role" => "system", "content" => system_prompt}] ++
        history ++
        [%{"role" => "user", "content" => user_text}]

    if log_prompts?() do
      log_prompt_bundle(messages, context)
    end

    case llm_client().chat(messages, initial_chat_opts(context)) do
      {:ok, %{content: content}} when is_binary(content) and content != "" ->
        draft =
          content
          |> sanitize_model_text()
          |> then(&AnswerPolish.polish(user_text, &1, context))
        {:ok, initial_out, initial_reflection} =
          ReflectionLoop.review(user_text, draft, context)

        {answer_out, answer_reflection} =
          maybe_revise_answer(user_text, messages, context, initial_out, initial_reflection)

        {grounded_out, grounded_reflection} =
          maybe_revise_grounded(messages, context, answer_out, answer_reflection)

        {out, reflection} =
          maybe_revise_evidence(user_text, draft, context, grounded_out, grounded_reflection)

        emit_complete_event(user_text, out, context, system_prompt, reflection)
        {:ok, out, reflection}

      {:ok, other} ->
        Logger.debug("[LlmSynthesis] Unexpected LLM response: #{inspect(other)}")
        {:error, :unexpected_response}

      {:error, reason} ->
        Logger.debug("[LlmSynthesis] LLM error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  # A question echo or content-free non-answer gets at most one additional
  # model pass with the actual same-session question restated. If the retry
  # also fails review, preserve Reflection's transparent failure instead of
  # publishing another echo or hallucinating a numerical answer.
  defp maybe_revise_answer(user_text, messages, context, original_out, reflection) do
    if :answer_not_supplied in Map.get(reflection, :issues, []) and
         is_binary(Map.get(context, :answer_target_question)) do
      question = Map.fetch!(context, :answer_target_question)

      revision_messages =
        case messages do
          [%{"role" => "system", "content" => system} | rest] ->
            [%{"role" => "system", "content" => system <>
              "\n\nAnswer-revision instruction: The factual question is " <>
              inspect(question) <> ". The last draft did not answer it. " <>
              "Provide a concise substantive answer if you know it reliably, " <>
              "clearly identifying approximations. Use existing retrieved " <>
              "evidence where present. If you cannot answer reliably, explain " <>
              "the specific limitation. Never echo the question or merely " <>
              "say 'I don't know'. Do not invent facts."} | rest]

          other ->
            other
        end

      result =
        try do
          llm_client().chat(revision_messages, initial_chat_opts(context))
        rescue
          _ -> {:error, :answer_revision_exception}
        catch
          :exit, _ -> {:error, :answer_revision_timeout}
        end

      {answer, selected_review, outcome} =
        case result do
          {:ok, %{content: content}} when is_binary(content) and content != "" ->
            candidate =
              content
              |> sanitize_model_text()
              |> then(&AnswerPolish.polish(user_text, &1, context))

            {:ok, validated, review} =
              ReflectionLoop.review(user_text, candidate, context)

            if review.status == :accept and validated == candidate and
                 not AnswerAdequacy.failed_answer?(question, candidate) do
              {candidate, review, :accepted}
            else
              {original_out, reflection, :rejected}
            end

          _ ->
            {original_out, reflection, :unavailable}
        end

      {answer, Map.put(selected_review, :answer_revision,
        %{attempted?: true, outcome: outcome, issue: :answer_not_supplied})}
    else
      {original_out, reflection}
    end
  end

  # When the first draft fails a claim-specific population check, the first
  # response remains a safe deterministic fallback. Give the model at most one
  # more attempt to phrase the supported fact conversationally, then validate
  # the new draft independently. Never publish a rejected revision.
  defp maybe_revise_grounded(messages, context, original_out, first_reflection) do
    cfg = Application.get_env(:core, :llm_synthesis, [])

    if Keyword.get(cfg, :grounded_revision_enabled?, true) and
         GroundedRevision.eligible?(context, first_reflection) do
      revision_messages = GroundedRevision.messages(messages, context)
      options =
        initial_chat_opts(context)
        |> Keyword.put(:max_tokens, llm_config(:grounded_revision_max_tokens, 220))

      result =
        try do
          llm_client().chat(revision_messages, options)
        rescue
          _ -> {:error, :grounded_revision_exception}
        catch
          :exit, _ -> {:error, :grounded_revision_timeout}
        end

      {out, review, trace} =
        case result do
          {:ok, %{content: content}} when is_binary(content) and content != "" ->
            candidate =
              content
              |> sanitize_model_text()
              |> then(&AnswerPolish.polish(Map.get(context, :user_text, ""), &1, context))

            {:ok, validated, candidate_review} =
              ReflectionLoop.review(Map.get(context, :user_text, ""), candidate, context)

            if candidate_review.status == :accept and validated == candidate do
              {candidate, candidate_review,
               %{attempted?: true, outcome: :accepted, source: :model_revision,
                 prior_issues: first_reflection.issues,
                 first_draft: String.slice(first_reflection.draft_text || "", 0, 500),
                 candidate: String.slice(candidate, 0, 500)}}
            else
              {original_out, first_reflection,
               %{attempted?: true, outcome: :rejected, source: :core_fallback,
                 prior_issues: first_reflection.issues,
                 candidate_issues: candidate_review.issues,
                 first_draft: String.slice(first_reflection.draft_text || "", 0, 500),
                 candidate: String.slice(candidate, 0, 500)}}
            end

          _ ->
            {original_out, first_reflection,
             %{attempted?: true, outcome: :unavailable, source: :core_fallback,
               prior_issues: first_reflection.issues,
               first_draft: String.slice(first_reflection.draft_text || "", 0, 500)}}
        end

      {out, Map.put(review, :grounded_revision, trace)}
    else
      {original_out, first_reflection}
    end
  end

  # A failed news draft gets ONE extra model call, only when fresh dated
  # headlines are available and the failure was news grounding/completeness.
  # Never retry on unsafe drafts; never let an unvalidated candidate replace
  # the already-grounded deterministic answer from Reflection.
  defp maybe_revise_evidence(user_text, draft, context, original_out, first_reflection) do
    cfg = Application.get_env(:core, :llm_synthesis, [])

    if Keyword.get(cfg, :evidence_revision_enabled?, true) and
         EvidenceRevision.eligible?(context, first_reflection) do
      revise_once(user_text, draft, context, original_out, first_reflection)
    else
      {original_out, first_reflection}
    end
  end

  defp revise_once(user_text, draft, context, original_out, first_reflection) do
    messages = EvidenceRevision.messages(user_text, context, first_reflection)
    started_at = System.monotonic_time(:millisecond)

    revision_options =
      chat_opts()
      |> Keyword.put(:max_tokens, llm_config(:evidence_revision_max_tokens, 360))

    result =
      try do
        llm_client().chat(messages, revision_options)
      rescue
        error -> {:error, {:revision_exception, Exception.message(error)}}
      catch
        :exit, _ -> {:error, :revision_timeout}
      end

    {out, reflection, candidate, review, outcome} =
      case result do
        {:ok, %{content: content}} when is_binary(content) and content != "" ->
          candidate =
            content
            |> sanitize_model_text()
            |> then(&AnswerPolish.polish(user_text, &1, context))

          if String.trim(candidate) == "" do
            {original_out, first_reflection, candidate, nil, :empty}
          else
            {:ok, validated, review} = ReflectionLoop.review(user_text, candidate, context)

            # Reflection may have "repaired" the candidate with a deterministic
            # fallback. That is NOT a successful model revision.
            if review.status == :accept and validated == candidate do
              {candidate, review, candidate, review, :accepted}
            else
              {original_out, first_reflection, candidate, review, :rejected}
            end
          end

        {:error, _reason} ->
          {original_out, first_reflection, nil, nil, :client_error}

        _ ->
          {original_out, first_reflection, nil, nil, :invalid_response}
      end

    elapsed_ms = System.monotonic_time(:millisecond) - started_at

    trace =
      EvidenceRevision.trace(
        first_reflection, draft, messages, candidate, review, outcome, elapsed_ms
      )

    emit_revision_event(context, trace)

    {out, Map.put(reflection, :evidence_revision, trace)}
  end

  defp emit_revision_event(context, trace) do
    Telemetry.emit(
      [:core, :response, :evidence_revision],
      %{elapsed_ms: Map.get(trace, :elapsed_ms, 0), attempts: 1},
      %{
        session_id: Map.get(context, :session_id, :global),
        outcome: Map.get(trace, :outcome),
        selected_source: Map.get(trace, :selected_source),
        initial_issues: Map.get(trace, :initial_issues, []),
        candidate_issues: Map.get(trace, :candidate_issues, [])
      }
    )
  end

  defp build_prompt(context, prompt_type)
       when prompt_type in [
              :mood_indices,
              :self_state_feeling,
              :self_portrait,
              :runtime_self_check
            ],
       do: LlmPrompt.self_state_prompt(context)

  defp build_prompt(context, _), do: LlmPrompt.build_system_prompt(context)

  defp sanitize_model_text(text) when is_binary(text) do
    text
    |> normalize_model_control_tokens()
    |> strip_leading_assistant_marker()
    |> truncate_generated_role_continuation()
    |> String.replace(~r/\n*\s*\((?:note|explanation):\s*.*\)\s*$/ius, "")
    |> String.trim()
  end

  defp normalize_model_control_tokens(text) do
    text
    |> String.replace(~r/<\|im_start\|>/u, "\n")
    |> String.replace(~r/<\|(?:im_end|eot_id|endoftext|end_of_text)\|>/u, "\n")
  end

  defp strip_leading_assistant_marker(text) do
    text
    |> String.replace(~r/\A[ \t]*(?:assistant|symbrella)[ \t]*:[ \t]*/iu, "")
    |> String.replace(~r/\A[ \t]*(?:assistant|symbrella)[ \t]*(?:\r?\n)+/iu, "")
  end

  defp truncate_generated_role_continuation(text) do
    lines = String.split(text, ~r/\r?\n/u, trim: false)

    {kept, _seen_content?} =
      Enum.reduce_while(lines, {[], false}, fn line, {acc, seen_content?} ->
        trimmed = String.trim(line)

        cond do
          seen_content? and generated_role_header?(trimmed) ->
            {:halt, {acc, seen_content?}}

          true ->
            {:cont, {[line | acc], seen_content? or trimmed != ""}}
        end
      end)

    kept
    |> Enum.reverse()
    |> Enum.join("\n")
  end

  defp generated_role_header?(""), do: false

  defp generated_role_header?(line) when is_binary(line) do
    Regex.match?(~r/^(?:user|assistant|system|symbrella)\s*$/iu, line) or
      Regex.match?(~r/^(?:user|assistant|system|symbrella)\s*:/iu, line)
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
  end

  defp emit_prompt_event(system_prompt, user_text, context) do
    {measurements, metadata} = LlmPromptEvents.prompt(system_prompt, user_text, context)
    Telemetry.emit([:core, :response, :prompt], measurements, metadata)
  end

  defp emit_complete_event(user_text, assistant_text, context, system_prompt, reflection) do
    {measurements, metadata} =
      LlmPromptEvents.complete(user_text, assistant_text, context, system_prompt, reflection)

    Telemetry.emit([:core, :response, :complete], measurements, metadata)
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

  defp initial_chat_opts(%{web_evidence: %{resolution: :headlines_available}}) do
    chat_opts()
    |> Keyword.put(:max_tokens, llm_config(:news_first_max_tokens, 280))
  end

  defp initial_chat_opts(_), do: chat_opts()

  defp chat_opts do
    timeout_ms = llm_config(:timeout_ms, 10_000)
    ready_timeout_ms = min(timeout_ms, llm_config(:ready_timeout_ms, 2_500))

    [
      timeout: timeout_ms,
      call_timeout: timeout_ms + 1_000,
      ready_timeout: ready_timeout_ms,
      max_tokens: llm_config(:max_tokens, 220)
    ]
  end

  defp history_turn_pairs do
    llm_config(:history_turn_pairs, 6)
  end

  defp llm_config(key, default) do
    :core
    |> Application.get_env(:llm_synthesis, [])
    |> Keyword.get(key, default)
    |> positive_integer(default)
  end

  defp positive_integer(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_integer(_value, default), do: default
end
