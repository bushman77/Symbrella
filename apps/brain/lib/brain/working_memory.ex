# lib/brain/working_memory.ex
defmodule Brain.WorkingMemory do
  @moduledoc """
  Pure working-memory utilities (no process state).

  WM item shape (newest-first list):
    %{
      id: term(),
      source: atom() | binary() | nil,
      activation: float(),
      score: float(),
      inserted_at: ms(),
      last_bump: ms(),
      payload: map()
    }
  """

  @type wm_item :: map()
  @type cfg :: %{
          capacity: pos_integer(),
          decay_ms: pos_integer(),
          gate_threshold: number(),
          merge_duplicates?: boolean()
        }

  @spec normalize(map(), non_neg_integer(), keyword()) :: wm_item()
  def normalize(%{} = cand, now_ms, opts \\ []) do
    act =
      Keyword.get(opts, :activation, (cand[:activation_snapshot] || cand[:score] || 0.0) * 1.0)

    score = (cand[:score] || cand[:activation_snapshot] || 0.0) * 1.0

    %{
      id:
        cand[:id] || cand[:chosen_id] || cand[:lemma] || cand[:word] || cand[:phrase] ||
          make_ref(),
      source: cand[:source] || cand[:reason] || :unknown,
      activation: clamp01(act),
      score: clamp01(score),
      inserted_at: now_ms,
      last_bump: now_ms,
      payload: cand
    }
  end

  @spec upsert([wm_item()], wm_item(), cfg()) :: [wm_item()]
  def upsert(wm, item, %{merge_duplicates?: true}) do
    case Enum.split_with(wm, &same_identity?(&1, item)) do
      {[], rest} ->
        [item | rest]

      {[existing | tail_dups], rest} ->
        merged = %{
          existing
          | activation: max(existing.activation, item.activation),
            last_bump: max(existing.last_bump, item.last_bump),
            score: max(existing.score, item.score),
            payload: Map.merge(existing.payload || %{}, item.payload || %{})
        }

        [merged | tail_dups ++ rest]
        |> Enum.reject(&(&1 != merged and same_identity?(&1, merged)))
    end
  end

  def upsert(wm, item, _cfg), do: [item | wm]

  @spec decay([wm_item()], non_neg_integer(), pos_integer()) :: [wm_item()]
  def decay(wm, now_ms, decay_ms) when is_integer(decay_ms) and decay_ms > 0 do
    Enum.map(wm, fn it ->
      age = now_ms - (it[:last_bump] || it[:inserted_at] || now_ms)
      factor = :math.pow(0.5, age / decay_ms)
      %{it | activation: clamp01(it.activation * factor)}
    end)
  end

  def decay(wm, _now, _decay_ms), do: wm

  @spec trim([wm_item()], pos_integer()) :: [wm_item()]
  def trim(wm, cap) when is_integer(cap) and cap > 0, do: Enum.take(wm, cap)
  def trim(wm, _), do: wm

  @spec remove([wm_item()], any()) :: {[wm_item()], non_neg_integer()}
  def remove(wm, id) when not is_function(id) do
    {kept, _dropped} = Enum.split_with(wm, fn it -> it.id != id end)
    {kept, length(wm) - length(kept)}
  end

  def remove(wm, fun) when is_function(fun, 1) do
    {kept, _dropped} = Enum.split_with(wm, fn it -> fun.(it) == false end)
    {kept, length(wm) - length(kept)}
  end

  defp same_identity?(a, b), do: a.id == b.id
  defp clamp01(x) when is_number(x), do: x |> max(0.0) |> min(1.0)
end
