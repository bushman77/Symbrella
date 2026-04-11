defmodule Core.Brain.Hippocampus do
  @moduledoc """
  Hippocampus integration stages for `Core.SemanticInput`.
  """

  alias Core.Brain.Runtime

  @spec encode(map()) :: map()
  def encode(%{atl_slate: slate} = si) when is_map(slate) do
    if Runtime.pid_alive?(Brain.Hippocampus) do
      ep = Runtime.apply_if_exported(Brain.Hippocampus, :encode, [slate], nil)

      if is_map(ep) do
        Map.put(si, :episode, Map.take(ep, [:ts_ms, :token_count, :winner_count]))
      else
        si
      end
    else
      si
    end
  end

  def encode(si), do: si

  @spec persist(map(), keyword()) :: map()
  def persist(%{} = si, opts) when is_list(opts) do
    _ =
      Runtime.apply_if_exported(
        Brain.Hippocampus.Writer,
        :maybe_persist,
        [
          si,
          [
            persist: Keyword.get(opts, :persist_episodes),
            embedding: Keyword.get(opts, :episode_embedding),
            user_id: Keyword.get(opts, :user_id)
          ]
        ],
        :ok
      )

    si
  end

  def persist(si, _opts), do: si
end
