defmodule Core.Response.Modes do
  @moduledoc """
  Deterministic fallback text for Core.Response when LLM synthesis is unavailable.
  Terse, contextual, and predictable (no randomness).

  Public API
  ----------
    compose(intent, tone, mode) :: String.t()
    compose(intent, tone, mode, opts) :: String.t()

  Optional `opts` slots (ignored if absent):
    :file_hint    — short path/module to reflect (e.g., "apps/brain/lib/brain/lifg.ex")
    :flag         — top guardrail label (e.g., :lifg_move, :acyclic_violation)
    :next_step    — one-line next step chosen by the planner
    :variant_seed — integer used to deterministically pick among phrasing variants
  """

  @type intent ::
          :abuse
          | :illicit_request
          | :greeting
          | :greet
          | :gratitude
          | :smalltalk
          | :question
          | :instruction
          | :help
          | :command
          | :refactor
          | :review
          | :plan
          | :diagram
          | :explain
          | :bug
          | :optimize
          | :benchmark
          | :unknown

  @type tone :: :warm | :neutral | :firm | :deescalate
  @type mode :: :collaborator | :coach | :scribe | :editor | :explainer

  @type opts :: %{
          optional(:file_hint) => String.t(),
          optional(:flag) => any(),
          optional(:next_step) => String.t(),
          optional(:variant_seed) => non_neg_integer()
        }

  @doc "Backward-compatible 3-arity; delegates to compose/4 with an empty opts map."
  @spec compose(intent, tone, mode) :: String.t()
  def compose(intent, tone, mode), do: compose(intent, tone, mode, %{})

  @spec compose(intent, tone, mode, opts | keyword) :: String.t()
  def compose(:abuse, :deescalate, _mode, _opts),
    do:
      "I'll keep this respectful and useful. Tell me what you want changed in Symbrella and I'll help with that."

  def compose(:abuse, :firm, _mode, _opts),
    do: "Let's keep it constructive. Name the file or task you want changed and I'll proceed."

  def compose(:illicit_request, _tone, _mode, _opts),
    do:
      "I can't help with buying drugs or getting wasted. I can help with safety, health risks, or getting support instead."

  def compose(:greeting, _tone, _mode, raw_opts),
    do: greeting_text(normalize_opts(raw_opts))

  def compose(:greet, _tone, _mode, raw_opts),
    do: greeting_text(normalize_opts(raw_opts))

  def compose(:smalltalk, _tone, _mode, _opts),
    do: "I'm here and ready to talk."

  def compose(:gratitude, _tone, _mode, _opts),
    do: "You're welcome."

  # ── Fallbacks ───────────────────────────────────────────────────────────────

  def compose(_intent, :deescalate, _mode, raw_opts) do
    opts = normalize_opts(raw_opts)

    with_file_hint(
      "No rush. We'll go one piece at a time. What's the single most helpful change right now?",
      opts[:file_hint]
    )
  end

  def compose(_intent, :firm, _mode, raw_opts) do
    opts = normalize_opts(raw_opts)

    with_file_hint("Got it. Send the concrete target or error and I'll keep the next step focused.", opts[:file_hint])
  end

  def compose(intent, tone, mode, raw_opts) do
    opts = normalize_opts(raw_opts)
    seed = opts[:variant_seed] || stable_seed(intent, tone, mode, opts)

    seed
    |> fallback_base()
    |> with_next_step(opts[:next_step])
    |> with_file_hint(opts[:file_hint])
  end

  # ── Helpers ─────────────────────────────────────────────────────────────────

  defp with_file_hint(text, nil), do: text

  defp with_file_hint(text, hint) when is_binary(hint) and hint != "" do
    text <> "\n" <> "Relevant target: `#{hint}`."
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

  # ——— internals ———
  defp normalize_opts(opts) when is_list(opts), do: Map.new(opts)
  defp normalize_opts(%{} = opts), do: opts
  defp normalize_opts(_), do: %{}

  defp fallback_base(seed) do
    choose(seed, [
      "I need one concrete target before I can make this useful. Send the module, file, or failing output.",
      "Give me the next concrete target and I'll tailor the response to that context.",
      "I can help from here, but I need the specific module, file, or behavior you want changed.",
      "Point me at the code or error you want handled and I'll respond with the next practical step."
    ])
  end

  defp greeting_text(opts) do
    text = String.downcase(to_string(opts[:text] || opts[:user_text] || ""))

    cond do
      String.contains?(text, "good morning") -> "Good morning. I'm here with you."
      String.contains?(text, "good afternoon") -> "Good afternoon. I'm here with you."
      String.contains?(text, "good evening") -> "Good evening. I'm here with you."
      true -> "Hello. I'm here with you."
    end
  end

  defp stable_seed(intent, tone, mode, opts) do
    :erlang.phash2(
      {intent, tone, mode, Map.get(opts, :file_hint), Map.get(opts, :flag), Map.get(opts, :next_step)}
    )
  end
end
