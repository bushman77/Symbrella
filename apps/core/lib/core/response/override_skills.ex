defmodule Core.Response.OverrideSkills do
  @moduledoc """
  Dispatcher for deterministic response override skills.
  """

  alias Core.Response.OverrideSkills.Companion
  alias Core.Response.OverrideSkills.SelfState
  alias Core.Response.OverrideSkills.Utility

  @spec apply(map(), map(), map()) :: {map(), map() | nil}
  def apply(features, decision, guard) do
    if Map.get(guard, :guardrail?) do
      {decision, nil}
    else
      features
      |> run_override(decision, [
        &Utility.illicit_redirect/2,
        &Companion.apply/2,
        &Utility.time/2,
        &SelfState.apply/2,
        &Utility.alarm/2
      ])
      |> case do
        {:ok, result} -> result
        :pass -> {decision, nil}
      end
    end
  end

  @spec deterministic_inline_text(map() | nil) :: String.t() | nil
  def deterministic_inline_text(%{id: id, inline_text: s})
      when id in [
             :illicit_request_redirect,
             :time,
             :mood_indices,
             :self_portrait,
             :runtime_self_check,
             :trust_repair,
             :companion_repair,
             :casual_companion,
             :idle_curiosity_casual,
             :alarm_capability
           ] and is_binary(s) and s != "" do
    s
  end

  def deterministic_inline_text(_), do: nil

  @spec llm_prompt_override(map() | nil) :: atom() | nil
  def llm_prompt_override(%{llm_prompt: prompt}) when is_atom(prompt), do: prompt
  def llm_prompt_override(_), do: nil

  @spec read_only_mood_query?(String.t()) :: boolean()
  defdelegate read_only_mood_query?(text), to: SelfState

  @spec self_check_query?(String.t()) :: boolean()
  defdelegate self_check_query?(text), to: SelfState

  defp run_override(_features, _decision, []), do: :pass

  defp run_override(features, decision, [override | rest]) do
    case override.(features, decision) do
      {:ok, result} -> {:ok, result}
      :pass -> run_override(features, decision, rest)
    end
  end
end
