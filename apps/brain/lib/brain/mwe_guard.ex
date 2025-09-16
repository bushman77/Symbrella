defmodule Brain.MWEGuard do
  @moduledoc """
  NegCache guard for multiword-expression (MWE) probing.
  No remote calls here; we just partition missing MWEs into
  {guarded_by_negcache, pending_for_probe}.
  """

  alias Db.BrainCell

  @type phrase :: String.t()
  @type result :: {[phrase()], [phrase()]}

  @spec partition_missing_mwes([Core.Token.t()], [BrainCell.t()]) :: result()
  def partition_missing_mwes(tokens, db_cells) do
    # Only consider multiword tokens not present in DB
    mw_tokens =
      tokens
      |> Enum.filter(& &1.mw)
      |> Enum.map(& &1.phrase)

    present_norms =
      db_cells
      |> Enum.map(& &1.norm)
      |> MapSet.new()

    missing =
      mw_tokens
      |> Enum.map(&normize/1)
      |> Enum.reject(&MapSet.member?(present_norms, &1))
      |> Enum.uniq()

    Enum.split_with(missing, &maybe_negcache_member?/1)
  end

  # Dynamic NegCache capability detection
  defp maybe_negcache_member?(norm) do
    cond do
      Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :member?, 1) ->
        Core.NegCache.member?(norm)

      Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :contains?, 1) ->
        Core.NegCache.contains?(norm)

      Code.ensure_loaded?(Core.NegCache) and function_exported?(Core.NegCache, :get, 1) ->
        !!Core.NegCache.get(norm)

      true ->
        false
    end
  end

  defp normize(p) do
    p
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end
end
