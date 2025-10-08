defmodule Brain.LIFG.Reanalysis do
  @moduledoc ~S"""
  Minimal reanalysis for Stage-1.

  If the top candidate is vetoed (`veto?: true`), promote the runner-up.
  Emits telemetry: [:brain, :lifg, :reanalysis] with %{token_index, from, to}.
  """

  @spec maybe_promote(map()) :: map()
  def maybe_promote(%{ranked: [top, second | _]} = choice) do
    if Map.get(top, :veto?, false) do
      :telemetry.execute([:brain, :lifg, :reanalysis], %{}, %{
        token_index: choice.token_index,
        from: top.id,
        to: second.id
      })

      Map.merge(choice, %{
        chosen_id: second.id,
        scores: Map.get(second, :scores, %{}),
        total:  Map.get(second, :total, 0.0),
        audit: Map.merge(choice.audit || %{}, %{reanalysis: %{from: top.id, to: second.id}})
      })
    else
      choice
    end
  end

  def maybe_promote(choice), do: choice
end

