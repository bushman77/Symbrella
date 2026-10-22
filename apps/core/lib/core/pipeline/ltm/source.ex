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


  @doc """
  Checked, read-only LTM lookup for external cognitive diagnostics.

  Unlike ltm/2, failures are NOT disguised as a legitimate empty dictionary.
  Never exposes raw SQL, credentials or submitted external text in errors.
  """
  @spec checked_ltm(map(), keyword()) :: {:ok, map()} | {:error, atom()}
  def checked_ltm(si, opts) when is_map(si) and is_list(opts) do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :ltm, 2) do
      case Db.ltm(si, opts) do
        {:ok, %{rows: rows, db_hits: %MapSet{}} = result} when is_list(rows) ->
          {:ok, result}

        _ ->
          {:error, :invalid_ltm_result}
      end
    else
      {:error, :ltm_module_unavailable}
    end
  rescue
    _ -> {:error, :ltm_query_failed}
  catch
    :exit, _ -> {:error, :ltm_query_exited}
    _, _ -> {:error, :ltm_query_failed}
  end

  def checked_ltm(_si, _opts), do: {:error, :invalid_ltm_input}

  defp empty, do: {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}
end
