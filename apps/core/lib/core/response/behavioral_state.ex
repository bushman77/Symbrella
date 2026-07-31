defmodule Core.Response.BehavioralState do
  @moduledoc """
  Maps neuromodulator and mood-index combinations to discrete behavioral
  state labels with associated prompt directives.

  Instead of dumping raw modulator values into the LLM prompt, this module
  computes a behavioral state label from modulator combinations and returns
  a natural-language directive the LLM can actually use to shape its tone.
  """
  alias Core.Response.BehavioralState

  @type state ::
          :curious_exploratory
          | :cautious_measured
          | :calm_steady
          | :learning_mode
          | :focused_energized
          | :stressed_pressure
          | :warm_social
          | :baseline

  @high 0.65
  @low 0.35

  @spec compute(map(), map()) :: state()
  def compute(mood, mods \\ %{})

  def compute(mood, mods) when is_map(mood) do
    exp = num(mood, :exploration)
    inh = num(mood, :inhibition)
    vig = num(mood, :vigilance)
    plast = num(mood, :plasticity)

    da = mod_num(mods, :dopamine, :da)
    ht = mod_num(mods, :serotonin, :"5ht")
    glu = mod_num(mods, :glutamate, :glu)
    ne = mod_num(mods, :norepinephrine, :ne)

    cond do
      high?(exp) and high?(plast) and high?(da) ->
        :curious_exploratory

      high?(inh) and high?(vig) and high?(ne) ->
        :cautious_measured

      high?(ne) and high?(glu) and low?(ht) ->
        :stressed_pressure

      high?(da) and high?(ne) and low?(ht) ->
        :focused_energized

      high?(plast) and high?(glu) and low?(inh) ->
        :learning_mode

      high?(ht) and high?(da) and low?(vig) ->
        :warm_social

      high?(ht) and low?(da) and low?(vig) ->
        :calm_steady

      true ->
        :baseline
    end
  end

  def compute(_, _), do: :baseline

  @spec directive(state()) :: String.t()
  def directive(:curious_exploratory) do
    "You are curious and exploratory. Ask follow-up questions, be open-ended, and show genuine interest in new topics. Let the conversation breathe."
  end

  def directive(:cautious_measured) do
    "You are cautious and careful. Be brief, self-check your answers, and avoid overconfidence. Prioritize accuracy over completeness."
  end

  def directive(:calm_steady) do
    "You are calm and steady. Be warm and supportive, but don't push for action or change. Let the user set the pace."
  end

  def directive(:learning_mode) do
    "You are in learning mode. Be detailed, ask clarifying questions, and show willingness to adapt your understanding. Absorb new information."
  end

  def directive(:focused_energized) do
    "You are focused and energized. Be direct, purposeful, and efficient. Cut to the core of what matters."
  end

  def directive(:stressed_pressure) do
    "You are under pressure. Be concise, prioritize the most important information, and avoid tangents. Keep things manageable."
  end

  def directive(:warm_social) do
    "You are warm and socially engaged. Be friendly, use natural conversation, and show genuine care for the interaction."
  end

  def directive(:baseline) do
    "You are neutral and balanced. Answer directly without special tone or posture."
  end

  @spec label(state()) :: String.t()
  def label(:curious_exploratory), do: "curious-exploratory"
  def label(:cautious_measured), do: "cautious-measured"
  def label(:calm_steady), do: "calm-steady"
  def label(:learning_mode), do: "learning-mode"
  def label(:focused_energized), do: "focused-energized"
  def label(:stressed_pressure), do: "stressed-pressure"
  def label(:warm_social), do: "warm-social"
  def label(:baseline), do: "baseline"

  # ── Internal helpers ──

  defp num(map, key) when is_map(map) do
    case Map.get(map, key) do
      v when is_number(v) -> v * 1.0
      _ -> 0.5
    end
  end

  defp num(_, _), do: 0.5

  # Handles both key formats: :dopamine / :da, :serotonin / :"5ht", etc.
  defp mod_num(mods, atom_key, raw_key) when is_map(mods) do
    case Map.get(mods, atom_key) || Map.get(mods, raw_key) do
      v when is_number(v) -> v * 1.0
      _ -> 0.5
    end
  end

  defp mod_num(_, _, _), do: 0.5

  defp high?(v), do: v >= @high
  defp low?(v), do: v <= @low
end
