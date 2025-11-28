defmodule Brain.AffectLatents do
  @moduledoc """
  TRCS affect latents derived from mood indices (+ optional context).

  Latents (all 0.0..1.0):

    :threat   — “how much should I brace / de-escalate?”
    :reward   — “how much should I approach / engage?”
    :control  — “how much sense of agency / predictability?”
    :safety   — “how OK is it to relax & stay open?”

  Inputs:

    mood: %{
      exploration: float,
      inhibition:  float,
      vigilance:   float,
      plasticity:  float
    }

    ctx (optional): %{
      intent_valence: -1.0..+1.0,  # abuse vs gratitude
      acc_conflict:   0.0..1.0,    # conflict from ACC
      wm_load:        0.0..1.0     # working memory density
    }

  For now we make conservative, easily-tunable formulas; later we can
  refine them with learned weights or more signals.
  """

  @type mood_t :: %{
          exploration: number,
          inhibition: number,
          vigilance: number,
          plasticity: number
        }

  @type ctx_t :: %{
          optional(:intent_valence) => number,
          optional(:acc_conflict) => number,
          optional(:wm_load) => number
        }

  @type latents_t :: %{
          threat: float,
          reward: float,
          control: float,
          safety: float
        }

  @spec compute(mood_t, ctx_t) :: latents_t
  def compute(mood, ctx \\ %{}) do
    exp = clamp(mood[:exploration] || mood["exploration"] || 0.5)
    inh = clamp(mood[:inhibition] || mood["inhibition"] || 0.5)
    vig = clamp(mood[:vigilance] || mood["vigilance"] || 0.5)
    pla = clamp(mood[:plasticity] || mood["plasticity"] || 0.5)

    intent_val = clamp_sym(ctx[:intent_valence] || ctx["intent_valence"] || 0.0)
    acc_conf = clamp(ctx[:acc_conflict] || ctx["acc_conflict"] || 0.0)
    wm_load = clamp(ctx[:wm_load] || ctx["wm_load"] || 0.0)

    # Threat: high vigilance, high conflict, low exploration
    threat =
      0.50 * vig +
        0.25 * acc_conf +
        0.15 * max(0.0, inh - 0.5) +
        0.10 * max(0.0, 0.5 - exp)

    # Reward: exploration + plasticity + positive intent
    reward =
      0.40 * exp +
        0.30 * pla +
        0.20 * max(0.0, -0.3 * (inh - 0.5) + 0.5) +
        0.10 * (intent_val * 0.5 + 0.5)

    # Control: “do I feel in control of the situation?”
    #  - good when exploration+plasticity beat inhibition
    #  - goes down with high WM load & high conflict
    control_drive = 0.5 * exp + 0.5 * pla
    control_brake = 0.6 * inh + 0.2 * acc_conf + 0.2 * wm_load
    control = 0.5 + 0.7 * (control_drive - control_brake)

    # Safety: low threat + decent reward + not totally inhibited
    safety =
      0.50 * (1.0 - threat) +
        0.30 * reward +
        0.20 * max(0.0, 1.0 - inh)

    %{
      threat: clamp(threat),
      reward: clamp(reward),
      control: clamp(control),
      safety: clamp(safety)
    }
  end

  # --- helpers --------------------------------------------------------

  defp clamp(v) when is_number(v), do: v |> max(0.0) |> min(1.0)
  defp clamp(_), do: 0.5

  # clamp symmetric -1..+1
  defp clamp_sym(v) when is_number(v) do
    v
    |> max(-1.0)
    |> min(1.0)
  end

  defp clamp_sym(_), do: 0.0
end
