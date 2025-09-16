defmodule Brain.Activator do
  @moduledoc "Activates brain cells for all POS variants of tokens."
  require Logger
  alias Db.Lexicon
  alias Db.BrainCell

  @doc """
  Ensure and activate all POS variants for the given tokens.
  Returns {cells, started_ids}.

  This implementation defaults to **soft activation** so the system cannot crash
  if runtime processes are not yet implemented. If you later add a real
  Brain.Cell + Brain.CellSup, set `enable_runtime? = true` and ensure names match.
  """
  def activate_all_pos(tokens, opts \\ []) do
    cells = Lexicon.ensure_pos_variants_from_tokens(tokens, opts)

    enable_runtime? = false
    started =
      if enable_runtime? do
        Enum.reduce(cells, [], fn %BrainCell{id: id} = cell, acc ->
          case start_runtime_cell(cell) do
            {:ok, _pid} -> [id | acc]
            {:already, _pid} -> [id | acc]
            {:soft, :ok} -> [id | acc]
            {:error, reason} ->
              Logger.warn("Activation failed for #{id}: #{inspect(reason)}")
              acc
          end
        end)
      else
        Enum.map(cells, & &1.id)
      end

    {cells, Enum.uniq(started)}
  end

  # Only used if you flip enable_runtime? to true.
  defp start_runtime_cell(%BrainCell{} = _cell) do
    # Soft fallback by default
    {:soft, :ok}
  end
end
