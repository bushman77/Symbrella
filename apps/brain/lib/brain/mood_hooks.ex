defmodule Brain.MoodHooks do
  @moduledoc """
  Thin adapter for optional MoodCore side effects.
  """

  @spec apply_intent(atom(), number()) :: :ok | term()
  def apply_intent(intent, conf) do
    if Code.ensure_loaded?(Brain.MoodCore) and
         function_exported?(Brain.MoodCore, :apply_intent, 2) do
      Brain.MoodCore.apply_intent(intent, conf)
    else
      :ok
    end
  end

  @doc """
  Fires the orienting response based on novelty (0.0 to 1.0).
  """
  def orient(novelty) when is_float(novelty) do
    Brain.MoodCore.orient(novelty)
    :ok
  end

  @spec register_activation(map()) :: :ok | term()
  def register_activation(active_cells) do
    if Code.ensure_loaded?(Brain.MoodCore) and
         function_exported?(Brain.MoodCore, :register_activation, 1) do
      Brain.MoodCore.register_activation(active_cells)
    else
      :ok
    end
  end

  @spec update_wm(list()) :: :ok | term()
  def update_wm(wm_list) do
    if Code.ensure_loaded?(Brain.MoodCore) and function_exported?(Brain.MoodCore, :update_wm, 1) do
      Brain.MoodCore.update_wm(wm_list)
    else
      :ok
    end
  end
end
