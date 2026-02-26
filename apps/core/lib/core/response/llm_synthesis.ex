# apps/core/lib/core/response/llm_synthesis.ex
defmodule Core.Response.LlmSynthesis do
  @moduledoc """
  LLM-backed response synthesis for Core.Response.

  Replaces templated `Modes.compose/3` calls with a Qwen/llama.cpp generation
  shaped by the brain's current state: mood tone, working memory contents,
  intent, and the raw user input.

  ## Usage

  Called from `Core.Response.plan/2` after the policy decision is made:

      case LlmSynthesis.generate(text_in, features, decision, mood) do
        {:ok, text}   -> text
        {:error, _}   -> Modes.compose(intent, decision.tone, decision.mode)
      end

  Falls back gracefully to templates if:
    • Llm is not running / not reachable
    • llama.cpp returns an error
    • Generation takes longer than `@timeout_ms`

  ## Prompt design

  System prompt is built from:
    • Tone directive  — warm / neutral / firm / deescalate
    • Mood indices    — exploration, inhibition, vigilance, plasticity (0..1)
    • Working memory  — top WM lemmas (what the brain is currently focused on)
    • Intent          — what the user is trying to do
    • Mode            — pair_programmer / coach / explainer etc.

  The system prompt intentionally does NOT include the full Symbrella
  codebase or docs — the brain pipeline has already done that work upstream.
  """

  require Logger

  @timeout_ms 15_000

  # ── Public API ────────────────────────────────────────────────────────────

  @doc """
  Generate a response via llama.cpp shaped by brain state.

  Returns {:ok, text} on success, {:error, reason} on failure.
  Callers should fall back to Modes.compose/3 on error.
  """
  @spec generate(String.t(), map(), map(), map()) :: {:ok, String.t()} | {:error, term()}
  def generate(user_text, features, decision, mood) do
    if llm_available?() do
      do_generate(user_text, features, decision, mood)
    else
      {:error, :llm_not_available}
    end
  end

  # ── Internal ──────────────────────────────────────────────────────────────

  defp do_generate(user_text, features, decision, mood) do
    system_prompt = build_system_prompt(features, decision, mood)

    messages = [
      %{"role" => "system", "content" => system_prompt},
      %{"role" => "user",   "content" => user_text}
    ]

    case Llm.chat(messages, timeout: @timeout_ms) do
      {:ok, %{content: content}} when is_binary(content) and content != "" ->
        {:ok, String.trim(content)}

      {:ok, other} ->
        Logger.debug("[LlmSynthesis] Unexpected LLM response: #{inspect(other)}")
        {:error, :unexpected_response}

      {:error, reason} ->
        Logger.debug("[LlmSynthesis] LLM error: #{inspect(reason)}")
        {:error, reason}
    end
  rescue
    e ->
      Logger.debug("[LlmSynthesis] Exception: #{inspect(e)}")
      {:error, :exception}
  catch
    :exit, reason ->
      Logger.debug("[LlmSynthesis] Exit: #{inspect(reason)}")
      {:error, :exit}
  end

  # ── System prompt builder ─────────────────────────────────────────────────

  defp build_system_prompt(features, decision, mood) do
    tone      = decision.tone
    mode      = decision.mode
    intent    = features.intent
    exp       = getv(mood, :exploration)
    inh       = getv(mood, :inhibition)
    vig       = getv(mood, :vigilance)
    plast     = getv(mood, :plasticity)
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

  # ── Tone directives ───────────────────────────────────────────────────────

  defp tone_directive(:warm, _),
    do: "Respond in a warm, engaged, and encouraging tone."

  defp tone_directive(:deescalate, _),
    do: "Respond calmly and gently. Keep things grounded and constructive."

  defp tone_directive(:firm, _),
    do: "Respond clearly and directly. Stay focused and purposeful."

  defp tone_directive(:neutral, :deescalate),
    do: "Respond in a measured, steady tone. Things are settling down."

  defp tone_directive(:neutral, _),
    do: "Respond in a balanced, clear tone."

  defp tone_directive(_, _),
    do: "Respond helpfully and clearly."

  # ── Mood context ──────────────────────────────────────────────────────────

  defp mood_context(exp, inh, vig, plast) do
    []
    |> maybe_add(exp > 0.65,   "You feel curious and ready to explore.")
    |> maybe_add(exp < 0.35,   "You are in a conservative, careful state.")
    |> maybe_add(vig > 0.80,   "Vigilance is elevated — stay measured.")
    |> maybe_add(inh > 0.70,   "Inhibition is high — keep things calm.")
    |> maybe_add(plast > 0.65, "You are in a receptive, learning-ready state.")
    |> case do
      []    -> ""
      notes -> "Current mood: " <> Enum.join(notes, " ")
    end
  end

  defp maybe_add(notes, true, note), do: notes ++ [note]
  defp maybe_add(notes, false, _),   do: notes

  # ── Mode directives ───────────────────────────────────────────────────────

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

  # ── WM context ───────────────────────────────────────────────────────────

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

  defp wm_context([]),     do: ""
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

