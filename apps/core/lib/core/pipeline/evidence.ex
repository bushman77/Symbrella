defmodule Core.Pipeline.Evidence do
  @moduledoc """
  Evidence cleanup helpers for `Core.SemanticInput`.
  """

  @spec drop_empty(map()) :: map()
  def drop_empty(%{} = si) do
    case Map.get(si, :evidence) do
      %{relations: []} = ev when map_size(ev) == 1 ->
        Map.delete(si, :evidence)

      _ ->
        si
    end
  end

  def drop_empty(other), do: other
end
