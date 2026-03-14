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
        if dedup_same_episode?(ep_head, new_ep) do
          refreshed =
            ep_head
            |> Dup.bump_dup_count()

          trim([{now, refreshed} | tail], keep)
        else
          trim([{now, new_ep} | window], keep)
        end

      [] ->
        trim([{now, new_ep}], keep)
    end
  end

  defp dedup_same_episode?(ep1, ep2) do
    MapSet.equal?(Map.get(ep1, :norms, MapSet.new()), Map.get(ep2, :norms, MapSet.new())) and
      dedup_scope_key(Map.get(ep1, :meta, %{})) == dedup_scope_key(Map.get(ep2, :meta, %{}))
  end

  defp dedup_scope_key(meta) when is_map(meta) do
    scope = meta[:scope] || meta["scope"]

    cond do
      is_map(scope) ->
        {:scope, Enum.sort(scope)}

      Map.has_key?(meta, :tenant) or Map.has_key?(meta, "tenant") ->
        {:tenant, meta[:tenant] || meta["tenant"]}

      Map.has_key?(meta, :conv_id) or Map.has_key?(meta, "conv_id") ->
        {:conv_id, meta[:conv_id] || meta["conv_id"]}

      true ->
        :global
    end
  end

  defp dedup_scope_key(_), do: :global

  @spec trim([{non_neg_integer(), episode()}], pos_integer()) ::
          [{non_neg_integer(), episode()}]
  def trim(window, keep) when is_integer(keep) and keep > 0, do: Enum.take(window, keep)
  def trim(window, _), do: window
end
