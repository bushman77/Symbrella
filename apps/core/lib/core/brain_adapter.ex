defmodule Core.BrainAdapter do
  @moduledoc "Thin adapter to read Brain's active-cells snapshot for Core."

  @spec snapshot() :: {:ok, {term(), list()}} | {:error, term()}
  def snapshot do
    brain = Module.concat(Elixir, Brain)

    if Code.ensure_loaded?(brain) and function_exported?(brain, :active_cells_snapshot, 0) do
      try do
        {:ok, apply(brain, :active_cells_snapshot, [])}
      rescue
        _ -> {:error, :brain_error}
      catch
        _, _ -> {:error, :brain_error}
      end
    else
      {:error, :no_brain_snapshot}
    end
  end
end

