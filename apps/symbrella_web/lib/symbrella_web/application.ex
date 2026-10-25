defmodule SymbrellaWeb.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    app_children = [
      SymbrellaWeb.Telemetry,
      SymbrellaWeb.ChatHistory,
      SymbrellaWeb.SocialBrainActivity
    ] ++ maybe_shadow_child() ++ [SymbrellaWeb.Endpoint]

    children = maybe_pubsub_child() ++ app_children

    opts = [strategy: :one_for_one, name: SymbrellaWeb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    SymbrellaWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  # Shadow mode is independent of the chat pipeline, disabled by default,
  # and requires the training archive so all neural inputs are real records.
  defp maybe_shadow_child do
    config = Application.get_env(:symbrella_web, SymbrellaWeb.NeuralShadow, [])
    archive = Application.get_env(:core, Core.TrainingArchive, [])

    if Keyword.get(config, :enabled?, false) == true and
         Keyword.get(archive, :enabled?, false) == true do
      [SymbrellaWeb.NeuralShadow]
    else
      []
    end
  end

  defp maybe_pubsub_child do
    case Process.whereis(Symbrella.PubSub) do
      nil -> [{Phoenix.PubSub, name: Symbrella.PubSub}]
      _pid -> []
    end
  end
end
