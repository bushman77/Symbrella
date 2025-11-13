defmodule Brain.LIFG.Stage1Bridge do
  @moduledoc """
  Thin compatibility layer around `Brain.LIFG.Stage1`.

  • `safe_call/3` — calls Stage1 with kw opts; falls back to legacy arity.
  • `normalize_result/1` — normalizes all historical return shapes to
    `{:ok, si, choices, audit}` or `{:error, reason}`.
  """

  @doc """
  Robust Stage-1 call (supports new/legacy arities + shapes).

  Tries:
    1) `Brain.LIFG.Stage1.run(si, kw_opts)`
    2) Fallback: `Brain.LIFG.Stage1.run(si, weights_only, eff_opts)`
  """
  @spec safe_call(map(), keyword(), keyword() | map()) :: any
  def safe_call(si, kw_opts, eff_opts) when is_map(si) and is_list(kw_opts) do
    try do
      Brain.LIFG.Stage1.run(si, kw_opts)
    rescue
      _ ->
        weights_only = Keyword.get(kw_opts, :weights, %{})
        Brain.LIFG.Stage1.run(si, weights_only, eff_opts)
    catch
      :exit, _ ->
        weights_only = Keyword.get(kw_opts, :weights, %{})
        Brain.LIFG.Stage1.run(si, weights_only, eff_opts)
    end
  end

  @doc """
  Normalize Stage-1 results into a single, stable tuple shape.
  """
  @spec normalize_result(term()) :: {:ok, map(), list(), map()} | {:error, term()}
  def normalize_result(result) do
    case result do
      {:ok, %{si: si, choices: choices, audit: audit}}
      when is_map(si) and is_list(choices) and is_map(audit) ->
        {:ok, si, choices, audit}

      {:ok, %{si: si, choices: choices}} when is_map(si) and is_list(choices) ->
        {:ok, si, choices, %{}}

      {:ok, si, meta} when is_map(si) and is_map(meta) ->
        choices = Map.get(meta, :choices, [])
        audit = Map.get(meta, :audit, %{})
        {:ok, si, List.wrap(choices), audit}

      {:ok, si} when is_map(si) ->
        {:ok, si, [], %{}}

      other ->
        {:error, other}
    end
  end
end
