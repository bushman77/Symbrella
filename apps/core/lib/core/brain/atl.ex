defmodule Core.Brain.ATL do
  @moduledoc """
  ATL integration stages for `Core.SemanticInput`.
  """

  alias Core.Brain.Runtime

  @spec ingest(map(), keyword()) :: map()
  def ingest(%{lifg_choices: choices, tokens: tokens} = si, _opts)
      when is_list(choices) and is_list(tokens) do
    if choices == [] do
      si
    else
      slate =
        case Process.whereis(Brain.ATL) do
          pid when is_pid(pid) ->
            if Process.alive?(pid) do
              Runtime.apply_if_exported(Brain.ATL, :ingest, [choices, tokens], %{})
            else
              Runtime.apply_if_exported(Brain.ATL, :reduce, [choices, tokens], %{})
            end

          _ ->
            Runtime.apply_if_exported(Brain.ATL, :reduce, [choices, tokens], %{})
        end

      if is_map(slate) do
        si
        |> Map.put(:atl_slate, slate)
        |> Map.update(:trace, [], fn tr ->
          [
            %{
              stage: :atl,
              ts_ms: System.system_time(:millisecond),
              winners: Map.get(slate, :winner_count, 0),
              concepts: slate |> Map.get(:by_norm, %{}) |> map_size()
            }
            | tr
          ]
        end)
      else
        si
      end
    end
  end

  def ingest(si, _opts), do: si

  @spec attach_lifg_pairs(map(), keyword()) :: map()
  def attach_lifg_pairs(%{} = si, opts) when is_list(opts) do
    case Runtime.apply_if_exported(Brain.ATL, :attach_lifg_pairs, [si, opts], si) do
      %{} = out -> out
      _ -> si
    end
  end

  def attach_lifg_pairs(si, _opts), do: si
end
