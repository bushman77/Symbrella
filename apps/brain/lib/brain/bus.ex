defmodule Brain.Bus do
  @moduledoc """
  Lightweight PubSub convenience for the Brain app.

  • Single source of truth for the PubSub server name (compile-time):
      config :brain, :pubsub, Symbrella.PubSub

  • Always call through this module in Brain code to avoid hard-coding the bus name.
  """

  @type bus_name :: atom()

  # Bind the PubSub server name at compile time (with a sensible default)
  @pubsub Application.compile_env(:brain, :pubsub, Symbrella.PubSub)

  @doc "Returns the configured PubSub server name."
  @spec name() :: bus_name()
  def name, do: @pubsub

  @doc "Subscribe the current process to a topic."
  @spec subscribe(String.t()) :: :ok
  def subscribe(topic) when is_binary(topic) do
    Phoenix.PubSub.subscribe(@pubsub, topic)
  end

  @doc "Unsubscribe the current process from a topic."
  @spec unsubscribe(String.t()) :: :ok
  def unsubscribe(topic) when is_binary(topic) do
    Phoenix.PubSub.unsubscribe(@pubsub, topic)
  end

  @doc "Broadcast a message to all subscribers of a topic."
  @spec broadcast(String.t(), term()) :: :ok
  def broadcast(topic, message) when is_binary(topic) do
    Phoenix.PubSub.broadcast(@pubsub, topic, message)
  end

  @doc "Broadcast a message without echoing back to the given sender."
  @spec broadcast_from(pid(), String.t(), term()) :: :ok
  def broadcast_from(sender, topic, message) when is_pid(sender) and is_binary(topic) do
    Phoenix.PubSub.broadcast_from(@pubsub, sender, topic, message)
  end
end
