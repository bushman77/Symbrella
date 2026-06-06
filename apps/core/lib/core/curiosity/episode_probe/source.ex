defmodule Core.Curiosity.EpisodeProbe.Source do
  @moduledoc """
  Boundary source for episode-probe fallback reads.

  `Core.Curiosity.EpisodeProbe` decides whether and how to ask; this module owns
  optional reads from live Hippocampus and persisted `Db.Episode` rows.
  """

  @compile {:no_warn_undefined, Brain.Hippocampus}
  @compile {:no_warn_undefined, Db.Episode}

  @spec hippocampus_window(pos_integer()) :: list(map())
  def hippocampus_window(limit) when is_integer(limit) and limit > 0 do
    cond do
      not module_loaded?(Brain.Hippocampus) ->
        []

      not function_exported?(Brain.Hippocampus, :snapshot, 0) ->
        []

      true ->
        case Brain.Hippocampus.snapshot() do
          %{window: window} when is_list(window) ->
            window
            |> Enum.take(limit)
            |> Enum.map(fn {at, ep} -> %{score: 0.0, at: at, episode: ep} end)

          _ ->
            []
        end
    end
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  def hippocampus_window(_), do: []

  @spec recent_db_episodes(pos_integer()) :: list()
  def recent_db_episodes(limit) when is_integer(limit) and limit > 0 do
    cond do
      not module_loaded?(Db.Episode) ->
        []

      not function_exported?(Db.Episode, :recent, 1) ->
        []

      true ->
        Db.Episode.recent(limit)
    end
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  def recent_db_episodes(_), do: []

  defp module_loaded?(mod) when is_atom(mod), do: Code.ensure_loaded?(mod)
end
