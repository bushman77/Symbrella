defmodule Core.ResponsePlanner do
  @moduledoc """
  ResponsePlanner — choose a tone from MoodCore + intent, and return text.

  v1.3 changes:
  • Command/imperative detection from raw text (e.g., “eat a banana”).
  • Benign override: helpful intents get :warm even if vigilance is elevated.
  • Anti-sticky deescalation after a hostile turn.
  • Greeting/gratitude/smalltalk kept warm unless vigilance is extreme.
  • Simple inline “how-to” answers (banana example).
  """

  @type si_like :: %{
          optional(:intent) => atom,
          optional(:keyword) => any,
          optional(:confidence) => number,
          optional(:text) => String.t()
        }

  @type mood_like :: %{
          optional(:mood) => %{
            optional(:exploration) => number,
            optional(:inhibition) => number,
            optional(:vigilance) => number,
            optional(:plasticity) => number
          },
          optional(:tone_hint) => atom
        }

  @helpful_intents [:question, :instruction, :help, :command]

  @spec plan(si_like, mood_like) :: {atom, String.t(), map}
  def plan(si, mood \\ %{}) do
    intent0 = Map.get(si, :intent, :unknown)
    conf    = clamp01(Map.get(si, :confidence, 0.0))
    text_in = to_string(Map.get(si, :text) || Map.get(si, :keyword) || "")

    # Mood indices (defensive whether nested or top-level)
    vig = getv(mood, :vigilance)
    inh = getv(mood, :inhibition)
    exp = getv(mood, :exploration)
    tone_hint = Map.get(mood, :tone_hint)

    # Upgrade/normalize intent from raw text when classifier is unsure or too coarse
    intent = normalize_intent(intent0, text_in)

    benign?  = benign_text?(text_in)
    hostile? = hostile_text?(text_in)

    tone = choose_tone(intent, vig, inh, exp, tone_hint, benign?, hostile?)

    # Tiny inline answer (kept simple on purpose)
    inline =
      case maybe_inline_answer(text_in) do
        "" -> nil
        s  -> s
      end

    text = inline || default_text(intent, tone)

    meta = %{
      intent_inferred: intent,
      intent_original: intent0,
      confidence: conf,
      tone: tone,
      benign: benign?,
      hostile: hostile?,
      mood_tone_hint: tone_hint,
      mood_sample: %{exploration: exp, inhibition: inh, vigilance: vig}
    }

    {tone, text, meta}
  end

  # ---------- Intent normalization from text ----------

  defp normalize_intent(intent, text) when intent in [:unknown, :other, nil] do
    t = dn(text)

    cond do
      greeting?(t)   -> :greeting
      gratitude?(t)  -> :gratitude
      smalltalk?(t)  -> :smalltalk
      question?(t)   -> :question
      command?(t)    -> :command
      true           -> :unknown
    end
  end

  defp normalize_intent(intent, text) do
    # If classifier gave us something generic, still detect obvious commands
    if intent in [:statement, :other] and command?(dn(text)), do: :command, else: intent
  end

  # ---------- Tone selection with benign override ----------

  # Abuse prioritizes safety; can be firm if vigilance already low.
  defp choose_tone(:abuse, vig, _inh, _exp, tone_hint, _benign?, _hostile?) do
    if vig > 0.50 or tone_hint == :deescalate, do: :deescalate, else: :firm
  end

  # Greetings/thanks/smalltalk → warm unless system is extremely vigilant.
  defp choose_tone(intent, vig, _inh, _exp, _tone_hint, _benign?, _hostile?)
       when intent in [:greeting, :gratitude, :smalltalk] do
    if vig >= 0.95, do: :deescalate, else: :warm
  end

  # Helpful intents: if text is benign, prefer warm (override high vigilance).
defp choose_tone(intent, vig, inh, exp, _tone_hint, true = _benign, _hostile)
     when intent in @helpful_intents do
  cond do
    vig >= 0.98 -> :neutral   # extreme vigilance still tones it down
    exp >= 0.35 and inh >= 0.30 -> :warm
    true -> :neutral
  end
end

  # Helpful intents but text not clearly benign → fall back to vigilance
  defp choose_tone(intent, vig, inh, exp, _tone_hint, _benign, _hostile)
       when intent in @helpful_intents do
    cond do
      vig >= 0.85 -> :deescalate
      exp >= 0.45 and inh >= 0.35 -> :warm
      true -> :neutral
    end
  end

  # Everyone else: avoid sticky deescalation once vigilance cooled off.
  defp choose_tone(_intent, vig, _inh, _exp, tone_hint, _benign, _hostile) do
    cond do
      tone_hint == :deescalate and vig < 0.75 -> :neutral
      tone_hint in [:warm, :neutral, :firm, :deescalate] -> tone_hint
      true -> :neutral
    end
  end

  # ---------- Default texts ----------

  defp default_text(:abuse, :deescalate),
    do: "I’m going to keep this respectful and useful. Tell me what you want changed in Symbrella and we’ll fix it."

  defp default_text(:abuse, :firm),
    do: "Let’s keep it constructive. I’m here to help you ship. What should we adjust next?"

  defp default_text(:greeting, :warm),
    do: "Hey—good to see you! What should we tune next?"

  defp default_text(:greeting, _),
    do: "Hey there. I’m ready when you are."

  defp default_text(:gratitude, _),
    do: "Appreciate it. Let’s keep momentum—what’s the very next change?"

  defp default_text(:smalltalk, :warm),
    do: "👋 All set here—want to point me at a file or module?"

  defp default_text(intent, tone) when intent in @helpful_intents do
    case tone do
      :deescalate -> "No rush. We’ll go one piece at a time. What’s top priority right now?"
      :warm       -> "Here’s a concise answer. Want an example or deeper steps?"
      _           -> "Here’s the answer and a next step. Say “full file” if you want a paste-ready module."
    end
  end

  defp default_text(_intent, :warm),
    do: "On it. I’ll keep things friendly and concise—what’s next?"

  defp default_text(_intent, :deescalate),
    do: "No rush. We’ll go one piece at a time. What’s top priority right now?"

  defp default_text(_intent, :firm),
    do: "Got it—staying focused and brief. Name the file or module."

  defp default_text(_intent, _),
    do: "Ready. Point me at the module and I’ll produce a clean drop-in."

  # ---------- Inline micro-answers ----------

  # Relaxed guard: answer if the text clearly asks for/commands a banana action.
  defp maybe_inline_answer(text) do
    t = dn(text)

    cond do
      banana?(t) -> banana_answer()
      true       -> ""
    end
  end

  defp banana?(t) do
    String.contains?(t, "banana") and
      (Regex.match?(~r/\bhow\s+(do\s+i|to)\b.*banana/, t) or
       Regex.match?(~r/\beat\b.*banana/, t) or
       Regex.match?(~r/\bbanana\b/, t))
  end

  defp banana_answer do
    """
    Here’s a quick way to eat a banana:

    1) Check ripeness — yellow with a few brown specks is sweetest.
    2) Rinse the peel (optional).
    3) Peel from the non-stem end (pinch and pull) or from the stem.
    4) Eat as-is, or slice into yogurt/cereal/smoothie.
    5) Dispose of the peel (don’t leave it on the ground).

    Tip: Underripe (greener) = firmer, less sweet. Overripe = great for smoothies or banana bread.
    """
  end

  # ---------- text classification helpers ----------

  defp greeting?(t),
    do:
      Regex.match?(~r/\b(hi|hello|hey|hiya|yo|sup|good (morning|afternoon|evening))\b/i, t)

  defp gratitude?(t),
    do: Regex.match?(~r/\b(thanks|thank you|appreciate it|ty)\b/i, t)

  defp smalltalk?(t),
    do: Regex.match?(~r/\b(how'?s it going|how are you|what'?s up|wyd)\b/i, t)

  defp question?(t) do
    String.contains?(t, "?") or
      Regex.match?(~r/\b(how|what|why|when|where|who|which)\b/i, t)
  end

  defp command?(t) do
    start_verbs = ~w(
      eat open show give make create add remove delete build generate write explain fix patch
      install update set configure run start stop render compile list show tell provide walk
      add-in drop-in refactor wire hook connect print log trace enable disable outline summarize
    )

    please_forms? = Regex.match?(~r/^\s*(please|pls|plz)\b/i, t)
    head = t |> String.trim_leading() |> String.split(~r/\s+/, parts: 2) |> List.first() |> to_string()

    verb_head? = Enum.member?(start_verbs, head)
    patterns? =
      Regex.match?(~r/\b(show|give|tell)\s+me\b/i, t) or
      Regex.match?(~r/\bwalk\s+me\s+through\b/i, t)

    please_forms? or verb_head? or patterns?
  end

  defp hostile_text?(t) do
    Regex.match?(~r/\b(fuck\s+you|bitch|asshole|idiot|stupid|dumbass|kill\s+yourself|kys|die|shut\s+up|screw\s+you)\b/i, t)
  end

  defp benign_text?(t) do
    # Benign if not hostile and doesn’t contain obvious slurs/insults/threats
    not hostile_text?(t)
  end

  # ---------- utils ----------

  defp getv(mood, key) do
    case {get_in(mood, [:mood, key]), Map.get(mood, key)} do
      {v, _} when is_number(v) -> v
      {_, v} when is_number(v) -> v
      _ -> 0.0
    end
  end

  defp dn(nil), do: ""
  defp dn(t), do: t |> to_string() |> String.downcase() |> String.trim()

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp01(_), do: 0.0
end

