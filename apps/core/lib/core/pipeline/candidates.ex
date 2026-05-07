defmodule Core.Pipeline.Candidates do
  @moduledoc """
  Candidate hygiene stages for `Core.SemanticInput`.

  Responsibilities:
  - reset transient sense candidates between pipeline phases
  - record trace metadata about candidate resets
  """

  @spec reset(map(), term()) :: map()
  def reset(%{} = si, tag) do
    sc = Map.get(si, :sense_candidates, %{})
    dropped = if is_map(sc), do: map_size(sc), else: 0

    si
    |> Map.put(:sense_candidates, %{})
    |> Core.Pipeline.Trace.append(
      :reset_sense_candidates,
      decision: :reset,
      reason: :phase_boundary,
      scores: %{dropped: dropped},
      meta: %{where: tag, dropped: dropped}
    )
  end

  def reset(si, _tag), do: si
end
