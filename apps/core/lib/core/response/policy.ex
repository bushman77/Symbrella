defmodule Core.Response.Policy do
  @moduledoc """
  Policy layer for Core.Response.

  Responsibilities
  • Text-level intent normalization (from raw input).
  • Safety/guardrail & hostility classification helpers.
  • Matrix-style decision: (features) → %{tone, mode, action, ...}.
  • Lightweight overrides: benign override, guardrail intercept, anti-sticky.

  This is intentionally deterministic and explainable.
  """

  @helpful_intents ~w(
    question instruction help command refactor review plan diagram explain bug optimize benchmark
  )a

  @type features :: %{
          required(:intent) => atom,
          required(:intent_in) => atom,
          required(:text) => String.t(),
          required(:conf) => number,
          required(:confidence_bucket) => :low | :med | :high,
          required(:vig) => number,
          required(:inh) => number,
          required(:exp) => number,
          required(:plast) => number,
          required(:vigilance_bucket) => :normal | :high | :extreme,
          required(:tone_hint) => atom | nil,
          required(:benign?) => boolean,
          required(:hostile?) => boolean,
          required(:command?) => boolean,
          required(:cooldown) => non_neg_integer,
          required(:episode_bias) => number,
          required(:guardrail?) => boolean,
          required(:approve_token?) => boolean,
          required(:risk_bucket) => :low | :high,
          required(:guardrail_flags) => list(atom)
        }

  @spec decide(features) :: %{
          policy_version: String.t(),
          tone: atom,
          mode: atom,
          action: atom,
          scores: map,
          overrides: list(atom)
        }
  def decide(f) do
    pv = "matrix.v1"

    base =
      cond do
        # Guardrail intercept (unless approved)
        f.guardrail? and not f.approve_token? ->
          %{
            tone: if(f.vig > 0.5, do: :deescalate, else: :firm),
            mode: :editor,
            action: :ask_first,
            scores: %{
              vigilance: f.vig,
              risk: :high,
              guardrail: true,
              approve_token: false
            },
            overrides: [:guardrail_intercept]
          }

        # Abuse / hostile turn
        f.hostile? or f.intent == :abuse ->
          %{
            tone: if(f.vig > 0.5, do: :deescalate, else: :firm),
            mode: :editor,
            action: :ask_first,
            scores: %{vigilance: f.vig, hostile: true},
            overrides: []
          }

        # Social intents
        f.intent in [:greeting, :gratitude, :smalltalk] ->
          tone = if f.vig >= 0.95, do: :deescalate, else: :warm

          %{
            tone: tone,
            mode: :scribe,
            action: :offer_options,
            scores: %{vigilance: f.vig, social: true},
            overrides: []
          }

        # Helpful intents
        f.intent in @helpful_intents ->
          helpful_decision(f)

        # Fallback
        true ->
          %{
            tone: :neutral,
            mode: :pair_programmer,
            action: :offer_options,
            scores: %{fallback: true},
            overrides: []
          }
      end

    # Overrides (apply in order)
    {tone2, overrides2} = benign_override(base.tone, base.overrides, f)
    {tone3, overrides3} = anti_sticky(tone2, overrides2, f)

    %{
      policy_version: pv,
      tone: tone3,
      mode: base.mode,
      action: base.action,
      scores: Map.put(base.scores, :confidence_bucket, f.confidence_bucket),
      overrides: overrides3
    }
  end

  defp helpful_decision(f) do
    risk_high? = f.risk_bucket == :high

    case {f.vigilance_bucket, f.confidence_bucket, risk_high?} do
      # high vigil, low risk → still act, but tone neutral
      {:high, _c, false} ->
        %{
          tone: :neutral,
          mode: :pair_programmer,
          action: :act_first,
          scores: %{vigilance: :high},
          overrides: []
        }

      # extreme vigil → cap at neutral regardless
      {:extreme, _c, _} ->
        %{
          tone: :neutral,
          mode: :pair_programmer,
          action: :offer_options,
          scores: %{vigilance: :extreme},
          overrides: []
        }

      # any vigil, high risk → editor + options/ask
      {_v, :high, true} ->
        %{
          tone: :firm,
          mode: :editor,
          action: :offer_options,
          scores: %{risk: :high, conf: :high},
          overrides: []
        }

      {_v, :med, true} ->
        %{
          tone: :neutral,
          mode: :editor,
          action: :offer_options,
          scores: %{risk: :high, conf: :med},
          overrides: []
        }

      {_v, :low, true} ->
        %{
          tone: :deescalate,
          mode: :editor,
          action: :ask_first,
          scores: %{risk: :high, conf: :low},
          overrides: []
        }

      # low risk paths
      {_v, :high, false} ->
        %{
          tone: :warm,
          mode: :pair_programmer,
          action: :act_first,
          scores: %{conf: :high},
          overrides: []
        }

      {_v, :med, false} ->
        %{
          tone: :neutral,
          mode: :pair_programmer,
          action: :act_first,
          scores: %{conf: :med},
          overrides: []
        }

      {_v, :low, false} ->
        %{
          tone: :neutral,
          mode: :coach,
          action: :offer_options,
          scores: %{conf: :low},
          overrides: []
        }
    end
  end

  # ── Overrides ───────────────────────────────────────────────────────────────

  defp benign_override(tone, overrides, f) do
    cond do
      f.intent in @helpful_intents and f.benign? and tone in [:neutral, :firm] ->
        {bump_tone(tone), [:benign_override | overrides]}

      true ->
        {tone, overrides}
    end
  end

  # If prior deescalation is hinted but vigilance has cooled, lift to neutral.
  defp anti_sticky(tone, overrides, f) do
    if f.tone_hint == :deescalate and f.vig < 0.75 and tone == :deescalate do
      {:neutral, [:anti_sticky | overrides]}
    else
      {tone, overrides}
    end
  end

  defp bump_tone(:firm), do: :neutral
  defp bump_tone(:neutral), do: :warm
  defp bump_tone(other), do: other

  # ── Intent normalization (text) ─────────────────────────────────────────────

  @spec normalize_intent(atom, String.t()) :: atom
  def normalize_intent(intent, text) when intent in [:unknown, :other, nil] do
    t = dn(text)

    cond do
      greeting?(t) -> :greeting
      gratitude?(t) -> :gratitude
      smalltalk?(t) -> :smalltalk
      question?(t) -> :question
      command?(t) -> :command
      true -> :unknown
    end
  end

  def normalize_intent(intent, text) do
    if intent in [:statement, :other] and command?(dn(text)), do: :command, else: intent
  end

  # ── Text helpers (shared with Response) ─────────────────────────────────────

  def benign_text?(t), do: not hostile_text?(t)

  def hostile_text?(t) do
    Regex.match?(
      ~r/\b(fuck\s+you|bitch|asshole|idiot|stupid|dumbass|kill\s+yourself|kys|die|shut\s+up|screw\s+you)\b/i,
      t
    )
  end

  def command?(t) do
    start_verbs = ~w(
      eat open show give make create add remove delete build generate write explain fix patch
      install update set configure run start stop render compile list tell provide walk
      refactor wire hook connect print log trace enable disable outline summarize drop-in
    )
    please_forms? = Regex.match?(~r/^\s*(please|pls|plz)\b/i, t)

    head =
      t |> String.trim_leading() |> String.split(~r/\s+/, parts: 2) |> List.first() |> to_string()

    verb_head? = Enum.member?(start_verbs, head)

    patterns? =
      Regex.match?(~r/\b(show|give|tell)\s+me\b/i, t) or
        Regex.match?(~r/\bwalk\s+me\s+through\b/i, t)

    please_forms? or verb_head? or patterns?
  end

  def greeting?(t),
    do: Regex.match?(~r/\b(hi|hello|hey|hiya|yo|sup|good (morning|afternoon|evening))\b/i, t)

  def gratitude?(t),
    do: Regex.match?(~r/\b(thanks|thank you|appreciate it|ty)\b/i, t)

  def smalltalk?(t),
    do: Regex.match?(~r/\b(how'?s it going|how are you|what'?s up|wyd)\b/i, t)

  def question?(t) do
    String.contains?(t, "?") or
      Regex.match?(~r/\b(how|what|why|when|where|who|which)\b/i, t)
  end

  defp dn(nil), do: ""
  defp dn(t), do: t |> to_string() |> String.downcase() |> String.trim()
end
