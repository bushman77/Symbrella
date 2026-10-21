defmodule Brain.CuriosityPolicy do
  @moduledoc """
  Pure advisory scoring and a hard *scheduling* gate for internal curiosity.
  Never grants evidence authority, permission to use tools, or admission to WM.

  The learning bias is a bounded read-only projection of durable investigation
  outcomes. Cognitive load and inhibition can defer automatic work, but they
  cannot override external feature gates, source checks or user decisions.
  """
  alias Brain.CuriosityLearning

  @kinds [:missing_link, :direct, :two_hop, :acquisition]

  def assess(kind, state) when kind in @kinds and is_map(state) do
    load = signal(state, :cognitive_load, 0.0)
    inhibition = signal(state, :inhibition, 0.5)
    vigilance = signal(state, :vigilance, 0.5)
    uncertainty = signal(state, :uncertainty, 0.5)
    plasticity = signal(state, :plasticity, 0.5)
    speculative? = kind in [:two_hop, :acquisition]

    # Small changes of score, not modifications of the evidentiary floors.
    uncertainty_adjustment = 0.08 * (uncertainty - 0.5)
    exploration = if speculative?, do: 0.08 * (plasticity - 0.5), else: 0.025 * (plasticity - 0.5)
    vigilance_adjustment = (if speculative?, do: -0.07, else: 0.035) * (vigilance - 0.5)
    load_penalty = -0.12 * load
    inhibition_penalty = -0.08 * inhibition
    biases = get(state, :investigative_biases, CuriosityLearning.neutral())
    learned = CuriosityLearning.bias(biases, kind)

    %{
      allowed?: load < 0.90 and inhibition < 0.90,
      reason: if(load >= 0.90 or inhibition >= 0.90,
        do: :self_model_overloaded, else: :ready),
      kind: kind,
      learned_bias: learned,
      adjustment: uncertainty_adjustment + exploration + vigilance_adjustment +
        load_penalty + inhibition_penalty + learned
    }
  end

  def assess(_, _), do: assess(:missing_link, %{})

  @doc "Apply only bounded score modulation; no score alone grants authorization."
  def score(base, kind, state) do
    assessment = assess(kind, state)
    {clamp(number(base, 0.0) + assessment.adjustment), assessment}
  end

  defp signal(state, key, default), do: state |> get(key, default) |> number(default) |> clamp()

  defp number(value, _) when is_number(value), do: value * 1.0
  defp number(_, default), do: default
  defp clamp(value), do: max(0.0, min(1.0, value))
  defp get(state, key, default) when is_map(state),
    do: Map.get(state, key, Map.get(state, Atom.to_string(key), default))
  defp get(_, _, default), do: default
end
