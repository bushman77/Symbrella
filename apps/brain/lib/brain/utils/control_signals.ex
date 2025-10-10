# lib/brain/utils/control_signals.ex
defmodule Brain.Utils.ControlSignals do
  @moduledoc """
  Utilities for coalescing control-signal pairs before fan-out.
  """

  @spec coalesce_pairs(list()) :: list({binary(), number()})
  def coalesce_pairs(list) when is_list(list) do
    list
    |> Enum.map(&to_pair/1)
    |> Enum.reject(&match?({:__invalid, _}, &1))
    |> Enum.group_by(fn {id, _amt} -> id end)
    |> Enum.map(fn {id, pairs} ->
      {id, Enum.reduce(pairs, 0.0, fn {_id, amt}, acc -> acc + amt end)}
    end)
  end

  defp to_pair({id, amt}) when is_binary(id) and is_number(amt), do: {id, amt}
  defp to_pair({id, _ti, amt}) when is_binary(id) and is_number(amt), do: {id, amt}
  defp to_pair(%{id: id, amount: amt}) when is_binary(id) and is_number(amt), do: {id, amt}

  defp to_pair(%{id: id, token_index: _ti, amount: amt}) when is_binary(id) and is_number(amt),
    do: {id, amt}

  defp to_pair(_), do: {:__invalid, 0.0}
end
