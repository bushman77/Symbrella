defmodule Core.Response.Modes do
  @moduledoc """
  Mode → text templates for Core.Response.
  Terse, action-oriented, and predictable (no randomness).

  Public API
  ----------
    compose(intent, tone, mode) :: String.t()
    compose(intent, tone, mode, opts) :: String.t()

  Supported `mode`s:
    :pair_programmer | :coach | :scribe | :editor | :explainer

  Optional `opts` slots (ignored if absent):
    :file_hint   — short path/module to reflect (e.g., "apps/brain/lib/brain/lifg.ex")
    :flag        — top guardrail label (e.g., :lifg_move, :acyclic_violation)
    :next_step   — one-line next step chosen by the planner
    :variant_seed — integer used to deterministically pick among phrasing variants
  """

  @helpful_intents ~w(
    question instruction help command refactor review plan diagram explain bug optimize benchmark
  )a

  @doc "Backward-compatible 3-arity; delegates to compose/4 with an empty opts map."
  @spec compose(atom, atom, atom) :: String.t()
  def compose(intent, tone, mode), do: compose(intent, tone, mode, %{})

  @spec compose(atom, atom, atom, map) :: String.t()
  def compose(:abuse, :deescalate, _mode, _opts),
    do:
      "I'll keep this respectful and useful. Tell me what you want changed in Symbrella and I'll help with that."

  def compose(:abuse, :firm, _mode, _opts),
    do:
      "Let's keep it constructive. Name the file or task you want changed and I'll proceed."

  # ── Greetings & Social ──────────────────────────────────────────────────────

  # Some upstream code may send :greet; normalize here for safety.
  def compose(intent, :warm, _mode, _opts) when intent in [:greeting, :greet],
    do:
      "Welcome to Symbrella 👋\n" <>
        "I'm ready to help. Quick picks:\n" <>
        "• Full file drop-in — say the path\n" <>
        "• Fix a warning/test — paste the error line\n" <>
        "• Plan next steps — I'll outline a short path\n" <>
        "• Peek at the brain — ask for WM or LIFG snapshot"

  def compose(intent, _tone, _mode, _opts) when intent in [:greeting, :greet],
    do:
      "Welcome back. Pick one to begin:\n" <>
        "• Full file drop-in — say the path\n" <>
        "• Fix a warning/test — paste the error line\n" <>
        "• Plan next steps — say \"plan\" for a quick outline"

  def compose(:gratitude, _tone, _mode, _opts),
    do: "Appreciate it. Let's keep momentum—what's the very next change?"

  def compose(:smalltalk, :warm, _mode, _opts),
    do: "👋 All set here—want to point me at a file or module?"

  # ── Helpful intents (build/plan/refactor/etc.) ──────────────────────────────

  # Pair-programmer: act-focused; invite "full file" only on safe/neutral paths.
  def compose(intent, tone, :pair_programmer, opts) when intent in @helpful_intents do
    base =
      case tone do
        :warm ->
          "Here's a concise plan. Say \"full file\" for a paste-ready drop-in."

        :neutral ->
          choose(opts[:variant_seed], [
            "Here's the plan and a next step. Say \"full file\" for a paste-ready module.",
            "Plan incoming plus the first action. Ask for \"full file\" if you want a drop-in.",
            "Short plan + next move. Say \"full file\" for a ready-to-paste version."
          ])

        _other ->
          "I'll keep it focused and safe. Say \"full file\" if you want the drop-in."
      end

    with_file_hint(base, opts[:file_hint])
  end

  # Coach: steer to small next step; always show A/B when action=offer_options upstream.
  def compose(intent, tone, :coach, opts) when intent in @helpful_intents do
    base =
      case tone do
        :deescalate ->
          "No rush. One step at a time—what's top priority right now?"

        _ ->
          "Let's pick a small next step. A) I act. B) clarify one detail."
      end

    with_next_step(base, opts[:next_step])
  end

  # Scribe: keep things moving without adding risk.
  def compose(_intent, :warm, :scribe, _opts),
    do: "On it. I'll keep it friendly and concise—what's next?"

  def compose(_intent, _tone, :scribe, _opts),
    do: "Got it. I can capture a quick TODO list or prep a short outline—your call."

  # Editor: guardrails or review tone.
  def compose(_intent, _tone, :editor, %{flag: flag}) when not is_nil(flag) do
    "That change risks a guardrail (#{inspect(flag)}). I can propose a safer path, or proceed if you send \"Approve: P-###\"."
  end

  def compose(_intent, _tone, :editor, _opts) do
    "I'll review and point out the risky spots, then offer a safe diff or a full-file alternative."
  end

  # Explainer: compact 3–5 bullets; no "full file" invite here unless planner decides elsewhere.
  def compose(_intent, _tone, :explainer, _opts) do
    """
    Here's the short version of how this works:
    1) What changes, at a glance
    2) Where it lives in the repo
    3) The key constraint or gotcha
    4) How to test quickly
    5) When to prefer a full file
    """
    |> String.trim()
  end

  # ── Fallbacks ───────────────────────────────────────────────────────────────

  def compose(_intent, :deescalate, _mode, _opts),
    do: "No rush. We'll go one piece at a time. What's the single most helpful change right now?"

  def compose(_intent, :firm, _mode, _opts),
    do: "Got it—staying focused and brief. Name the file or module."

  def compose(_intent, _tone, _mode, _opts),
    do: "Ready. Point me at the module and I'll produce a clean drop-in."

  # ── Helpers ─────────────────────────────────────────────────────────────────

  defp with_file_hint(text, nil), do: text

  defp with_file_hint(text, hint) when is_binary(hint) and hint != "" do
    text <> "\n" <> "I can prep a drop-in for `#{hint}`."
  end

  defp with_file_hint(text, _), do: text

  defp with_next_step(text, nil), do: text

  defp with_next_step(text, step) when is_binary(step) and step != "" do
    text <> " Suggested next step: " <> step
  end

  defp with_next_step(text, _), do: text

  # Deterministic variant chooser for small phrasing variety; defaults to first.
  defp choose(nil, [first | _]), do: first
  defp choose(seed, list) when is_integer(seed) and seed >= 0 and is_list(list) and list != [] do
    idx = rem(seed, length(list))
    Enum.at(list, idx)
  end
end

