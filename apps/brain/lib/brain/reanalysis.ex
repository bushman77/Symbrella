defmodule Brain.LIFG.Reanalysis do
  @moduledoc ~S"""
  Reanalysis fallback for LIFG: if the top candidate fails integration, flip to the next-best.

  Usage:
      {:ok, winner, trace} =
        Brain.LIFG.Reanalysis.pick(candidates, si,
          fit?: fn cand, si -> integration_ok?(cand, si) end,
          max_flips: 1
        )

  Options:
    * `:fit?`       - (required) `fn candidate :: map(), si :: any() -> boolean end`
    * `:max_flips`  - how many losers we may skip before failing (default: 1)
    * `:order`      - `:score_desc` (default) or `:keep` to keep incoming order

  Returns:
    * `{:ok, candidate, trace}` on success
    * `{:error, :no_candidates, trace}` if list empty
    * `{:error, :no_fit, trace}` if no acceptable candidate within flip budget

  `trace` is a list like: `[{:reject, "bank|noun|river"}, {:accept, "bank|noun|money"}]`
  """

  @type candidate :: map()

  @spec pick([candidate], any, keyword()) ::
          {:ok, candidate, list()} | {:error, :no_candidates | :no_fit, list()}
  def pick(cands, si, opts \\ []) when is_list(cands) do
    fit? =
      case Keyword.fetch(opts, :fit?) do
        {:ok, fun} when is_function(fun, 2) -> fun
        _ -> raise ArgumentError, "Brain.LIFG.Reanalysis.pick/3 requires :fit? (arity 2)"
      end

    max_flips = Keyword.get(opts, :max_flips, 1)
    order = Keyword.get(opts, :order, :score_desc)

    ordered =
      case order do
        :keep ->
          cands

        :score_desc ->
          Enum.sort_by(cands, fn c ->
            {-(Map.get(c, :score, Map.get(c, :prior, 0.0)) * 1.0), Map.get(c, :id, "")}
          end)
      end

    do_pick(ordered, si, fit?, max_flips, 0, [])
  end

  defp do_pick([], _si, _fit?, _max_flips, _used, trace),
    do: {:error, :no_candidates, Enum.reverse(trace)}

  defp do_pick([cand | rest], si, fit?, max_flips, used, trace) do
    id = Map.get(cand, :id, inspect(cand))

    if fit?.(cand, si) do
      {:ok, cand, Enum.reverse([{:accept, id} | trace])}
    else
      if used >= max_flips do
        # Budget exhausted: do NOT count this extra reject.
        {:error, :no_fit, Enum.reverse(trace)}
      else
        # Consume one flip (reject current) and continue.
        do_pick(rest, si, fit?, max_flips, used + 1, [{:reject, id} | trace])
      end
    end
  end
end
