defmodule Core.Brain.Hippocampus do
  @moduledoc """
  Hippocampus integration stages for `Core.SemanticInput`.
  """

  alias Core.Brain.Runtime

  @spec encode(map()) :: map()
  def encode(%{atl_slate: slate} = si) when is_map(slate) do
    if Runtime.pid_alive?(Brain.Hippocampus) do
      ep = Runtime.apply_if_exported(Brain.Hippocampus, :encode, [slate, episode_meta(si)], nil)

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

  defp episode_meta(%{} = si) do
    %{}
    |> put_if_present(:sentence, Map.get(si, :sentence))
    |> put_if_present(:intent, Map.get(si, :intent))
    |> put_if_present(:keyword, Map.get(si, :keyword))
    |> put_if_present(:confidence, Map.get(si, :confidence))
    |> put_if_present(:session_id, Map.get(si, :session_id))
    |> put_if_present(:appraisal, Map.get(si, :appraisal))
    |> put_if_present(:mood, Map.get(si, :mood))
    |> put_if_present(:self_model, self_model_meta(Map.get(si, :self_model)))
    |> put_if_present(:self_continuity, Map.get(si, :self_continuity))
    |> put_if_present(:response_text, Map.get(si, :response_text))
    |> put_if_present(:response_tone, Map.get(si, :response_tone))
    |> put_if_present(:response_meta, Map.get(si, :response_meta))
  end

  defp put_if_present(map, _key, nil), do: map
  defp put_if_present(map, _key, ""), do: map
  defp put_if_present(map, key, value), do: Map.put(map, key, value)

  defp self_model_meta(%Brain.SelfModel{} = model) do
    %{
      v: model.v,
      confidence: model.confidence,
      uncertainty: model.uncertainty,
      stability: model.stability,
      focus: model.focus,
      cognitive_load: model.cognitive_load,
      vigilance: model.vigilance,
      plasticity: model.plasticity,
      inhibition: model.inhibition,
      recent_error_count: length(List.wrap(model.recent_errors)),
      active_goal_count: length(List.wrap(model.active_goals))
    }
  end

  defp self_model_meta(_), do: nil

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
