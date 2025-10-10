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
        if MapSet.equal?(
             Map.get(new_ep, :norms, MapSet.new()),
             Map.get(ep_head, :norms, MapSet.new())
           ) do
          # De-dup: refresh timestamp, adopt newest slate, merge meta (new wins), bump dup_count
          merged_meta = Map.merge(ep_head.meta || %{}, new_ep.meta || %{})
          refreshed = %{ep_head | slate: new_ep.slate, meta: merged_meta} |> Dup.bump_dup_count()
          trim([{now, refreshed} | tail], keep)
        else
          trim([{now, new_ep} | window], keep)
        end

      [] ->
        trim([{now, new_ep}], keep)
    end
  end

  @spec trim([{non_neg_integer(), episode()}], pos_integer()) ::
          [{non_neg_integer(), episode()}]
  def trim(window, keep) when is_integer(keep) and keep > 0, do: Enum.take(window, keep)
  def trim(window, _), do: window
end
