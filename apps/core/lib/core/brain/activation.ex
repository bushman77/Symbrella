defmodule Core.Brain.Activation do
  @moduledoc """
  Activation notification stage for `Core.SemanticInput`.
  """

  @spec notify(map(), keyword()) :: map()
  def notify(%{} = si, opts) when is_list(opts) do
    payload = %{
      delta: Keyword.get(opts, :delta, 0.1),
      decay: Keyword.get(opts, :decay, 0.98),
      via: :core
    }

    rows = Map.get(si, :active_cells, []) || []
    lifg_count = si |> Map.get(:lifg_choices, []) |> length()

    if rows != [] do
      _ = Core.Brain.activate_cells(rows, payload)
    end

    Map.update(si, :trace, [], fn tr ->
      [{:activated, %{rows: length(rows), lifg_choices: lifg_count, shape: :activate_cells}} | tr]
    end)
  end

  def notify(si, _opts), do: si
end
