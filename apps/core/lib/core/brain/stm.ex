defmodule Core.Brain.STM do
  @moduledoc """
  Short-term memory stage bridge for `Core.SemanticInput`.

  This stage asks Brain to enrich the SI with the current active-cell snapshot
  and any token normalization Brain performs at STM time.
  """

  @spec run(map()) :: map()
  def run(%{} = si) do
    if Code.ensure_loaded?(Brain) and is_pid(Process.whereis(Brain)) and
         function_exported?(Brain, :stm, 1) do
      case Brain.stm(si) do
        %{} = out -> out
        _ -> si
      end
    else
      si
    end
  end

  def run(si), do: si
end
