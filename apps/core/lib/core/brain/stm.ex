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
        %{} = out -> attach_neuron_snapshot(si, out)
        _ -> si
      end
    else
      si
    end
  end

  @doc """
  Preserve the distinction between lexical candidates (:active_cells list) and
  currently firing neurons (:neuron_activation map). Brain.stm/1 historically
  overwrote active_cells with the runtime map; LTM would then discard it.
  """
  @spec attach_neuron_snapshot(map(), map()) :: map()
  def attach_neuron_snapshot(si, out) when is_map(si) and is_map(out) do
    activation = Map.get(out, :active_cells)

    if is_map(activation) do
      out
      |> Map.put(:neuron_activation, activation)
      |> Map.put(:active_cells, lexical_candidates(si))
    else
      out
    end
  end

  defp lexical_candidates(si) do
    case Map.get(si, :active_cells, []) do
      cells when is_list(cells) -> cells
      _ -> []
    end
  end

  def run(si), do: si
end
