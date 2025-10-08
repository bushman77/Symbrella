defmodule Brain.Hippocampus.Window do
  @moduledoc """
  Rolling window utils for Hippocampus (append-or-refresh head; trim).
  """

  alias Brain.Hippocampus.Dup

  @type episode :: %{slate: map(), meta: map(), norms: MapSet.t()}

  @spec append_or_refresh_head([{non_neg_integer(), episode()}], episode(), pos_integer()) ::
          [{non_neg_integer(), episode()}]
  def append_or_refresh_head(window, %{} = new_ep, keep) when is_integer(keep) and keep > 0 do
    now = System.system_time(:millisecond)

    case window do
      [{_at_head, ep_head} | tail] ->
        if MapSet.equal?(Map.get(new_ep, :norms, MapSet.new()), Map.get(ep_head, :norms, MapSet.new())) do
          # De-dup: refresh head timestamp; optionally bump dup_count in meta (see Dup)
          ep2 = Dup.bump_dup_count(ep_head)
          [{now, ep2} | tail]
        else
          [{now, new_ep} | window] |> Enum.take(keep)
        end

      [] ->
        [{now, new_ep}]
    end
  end

  @spec trim([{non_neg_integer(), episode()}], pos_integer()) :: [{non_neg_integer(), episode()}]
  def trim(window, keep) when is_integer(keep) and keep > 0, do: Enum.take(window, keep)
  def trim(window, _), do: window
end

