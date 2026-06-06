defmodule Core.Pipeline.LTM.Source do
  @moduledoc """
  Boundary source for long-term memory candidate reads.
  """

  @compile {:no_warn_undefined, Db}

  @spec ltm(map(), keyword()) :: {:ok, map()}
  def ltm(si, opts) when is_map(si) and is_list(opts) do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :ltm, 2) do
      Db.ltm(si, opts)
    else
      empty()
    end
  rescue
    _ -> empty()
  catch
    _, _ -> empty()
  end

  def ltm(_si, _opts), do: empty()

  defp empty, do: {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}
end
