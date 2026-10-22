defmodule Llm.Moltbook.Adapter do
  @moduledoc """
  Opt-in Moltbook environment configuration.

  This module does not start processes, register agents, or publish anything.
  Credentials are read from MOLTBOOK_API_KEY at request time and are never
  included in configuration or status results.
  """

  @api_base "https://www.moltbook.com/api/v1"
  @defaults [
    enabled?: false,
    mode: :observe,
    api_base: @api_base,
    max_posts: 10,
    timeout_ms: 5_000
  ]

  @doc "Return validated, non-secret Moltbook settings."
  def config do
    case Application.get_env(:llm, :moltbook, []) do
      opts when is_list(opts) ->
        if Keyword.keyword?(opts) do
          opts = Keyword.merge(@defaults, opts)

          case validate(opts) do
            :ok -> {:ok, opts |> Map.new() |> Map.take([:enabled?, :mode, :api_base, :max_posts, :timeout_ms])}
            {:error, _} = error -> error
          end
        else
          {:error, {:invalid_moltbook_config, :settings}}
        end

      _ ->
        {:error, {:invalid_moltbook_config, :settings}}
    end
  end

  @doc """
  Local environment status only. This does not contact Moltbook or confirm
  whether the remote service accepts the key. Use Client.me/0 for that.
  """
  def status do
    with {:ok, config} <- config() do
      credentials_configured? = not is_nil(api_key())

      {:ok,
       %{
         enabled?: config.enabled?,
         mode: config.mode,
         api_base: config.api_base,
         max_posts: config.max_posts,
         credentials_configured?: credentials_configured?,
         ready?: config.enabled? and credentials_configured?
       }}
    end
  end

  @doc false
  def require_observe do
    case config() do
      {:ok, %{enabled?: false}} ->
        {:error, :moltbook_disabled}

      {:ok, %{mode: :observe} = config} ->
        if api_key(), do: {:ok, config}, else: {:error, :missing_api_key}

      {:ok, _config} ->
        {:error, :unsupported_moltbook_mode}

      {:error, _} = error ->
        error
    end
  end

  @doc false
  def api_key do
    case System.get_env("MOLTBOOK_API_KEY") do
      key when is_binary(key) ->
        case String.trim(key) do
          "" -> nil
          value -> value
        end

      _ ->
        nil
    end
  end

  defp validate(opts) do
    cond do
      Keyword.fetch!(opts, :enabled?) not in [true, false] ->
        {:error, {:invalid_moltbook_config, :enabled?}}

      Keyword.fetch!(opts, :mode) != :observe ->
        {:error, {:invalid_moltbook_config, :mode}}

      # Never redirect a bearer credential to another host or scheme.
      Keyword.fetch!(opts, :api_base) != @api_base ->
        {:error, {:invalid_moltbook_config, :api_base}}

      not valid_integer?(Keyword.fetch!(opts, :max_posts), 1, 50) ->
        {:error, {:invalid_moltbook_config, :max_posts}}

      not valid_integer?(Keyword.fetch!(opts, :timeout_ms), 1_000, 30_000) ->
        {:error, {:invalid_moltbook_config, :timeout_ms}}

      true ->
        :ok
    end
  end

  defp valid_integer?(value, min, max),
    do: is_integer(value) and value >= min and value <= max
end
