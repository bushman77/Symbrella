defmodule Brain.Bus do
  @moduledoc """
  Lightweight PubSub convenience for the Brain app.

  • Single source of truth for the PubSub name:
      Application.get_env(:brain, :pubsub, Symbrella.PubSub)

  • Always call through this module in Brain code to avoid hard-coding the bus name.
  """

  @type bus_name :: atom()

  @doc "Returns the configured PubSub name (defaults to Symbrella.PubSub)."
  @spec name() :: bus_name()
  def name, do: Application.get_env(:brain, :pubsub, Symbrella.PubSub)

  @doc "Subscribe the current process to a topic."
  @spec subscribe(String.t()) :: :ok | {:error, term()}
  def subscribe(topic) when is_binary(topic) do
    Phoenix.PubSub.subscribe(name(), topic)
  end

  @doc "Unsubscribe the current process from a topic."
  @spec unsubscribe(String.t()) :: :ok
  def unsubscribe(topic) when is_binary(topic) do
    Phoenix.PubSub.unsubscribe(name(), topic)
  end

  @doc "Broadcast a message to all subscribers of a topic."
  @spec broadcast(String.t(), term()) :: :ok | {:error, term()}
  def broadcast(topic, message) when is_binary(topic) do
    Phoenix.PubSub.broadcast(name(), topic, message)
  end

  @doc "Broadcast a message without echoing back to the given sender."
  @spec broadcast_from(pid(), String.t(), term()) :: :ok | {:error, term()}
  def broadcast_from(sender, topic, message) when is_pid(sender) and is_binary(topic) do
    Phoenix.PubSub.broadcast_from(name(), sender, topic, message)
  end
end

