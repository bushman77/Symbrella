defmodule Core.Pipeline.LTM do
  @moduledoc """
  Long-term memory enrichment stage for `Core.SemanticInput`.

  Responsibilities:
  - read LTM matches from `Db.ltm/2`
  - merge normalized active cells into the SI
  - accumulate DB hit metadata into `:activation_summary`
  """

  alias Db
  alias Db.BrainCell

  @spec run(map(), keyword()) :: map()
  def run(%{} = si, opts) when is_list(opts) do
    case Db.ltm(si, opts) do
      {:ok, %{rows: rows, db_hits: db_hits}} ->
        existing =
          case Map.get(si, :active_cells, []) do
            cells when is_list(cells) -> cells
            _ -> []
          end

        active_cells =
          (existing ++ rows)
          |> Enum.flat_map(&sanitize_cell/1)
          |> Enum.reject(&(cell_id(&1) == nil))
          |> Enum.uniq_by(&cell_id/1)

        db_hits_ms =
          case db_hits do
            %MapSet{} = ms -> ms
            other -> MapSet.new(List.wrap(other))
          end

        activation_summary0 =
          case Map.get(si, :activation_summary) do
            %{} = summary -> summary
            _ -> %{}
          end

        activation_summary =
          Map.update(activation_summary0, :db_hits, db_hits_ms, fn acc ->
            acc_ms =
              cond do
                match?(%MapSet{}, acc) -> acc
                is_list(acc) -> MapSet.new(acc)
                is_nil(acc) -> MapSet.new()
                true -> MapSet.new(List.wrap(acc))
              end

            MapSet.union(acc_ms, db_hits_ms)
          end)

        si
        |> Map.put(:active_cells, active_cells)
        |> Map.put(:activation_summary, activation_summary)

      _ ->
        si
    end
  end

  def run(si, _opts), do: si

  defp sanitize_cell(%BrainCell{} = cell), do: [cell]
  defp sanitize_cell(%{id: _} = cell), do: [cell]
  defp sanitize_cell(%{"id" => _} = cell), do: [cell]
  defp sanitize_cell(id) when is_binary(id), do: [%{id: id}]
  defp sanitize_cell(_), do: []

  defp cell_id(%BrainCell{id: id}), do: id
  defp cell_id(%{id: id}), do: id
  defp cell_id(%{"id" => id}), do: id
  defp cell_id(id) when is_binary(id), do: id
  defp cell_id(_), do: nil
end
