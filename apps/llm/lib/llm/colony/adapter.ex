defmodule Llm.Colony.Adapter do
  @moduledoc """
  Local, opt-in environment for observing The Colony.

  The registered agent key stays in the existing umbrella-root
  priv/.colony_api_key (or COLONY_API_KEY). No new directories are created.
  Credentials are never returned from status/0 or config/0.
  """

  import Bitwise

  @api_base "https://thecolony.ai/api/v1"
  @key_file Path.expand("../../../../../priv/.colony_api_key", __DIR__)
  @defaults [
    enabled?: false,
    mode: :observe,
    api_base: @api_base,
    max_posts: 10,
    timeout_ms: 5_000
  ]

  @doc "Validated, non-secret configuration."
  def config do
    case Application.get_env(:llm, :colony, []) do
      opts when is_list(opts) ->
        if Keyword.keyword?(opts) do
          config = Keyword.merge(@defaults, opts)

          with :ok <- validate(config) do
            {:ok,
             config
             |> Map.new()
             |> Map.take([:enabled?, :mode, :api_base, :max_posts, :timeout_ms])}
          end
        else
          {:error, {:invalid_colony_config, :settings}}
        end

      _ ->
        {:error, {:invalid_colony_config, :settings}}
    end
  end

  @doc """
  Local readiness only; no HTTP request or remote authentication check.
  Client.me/0 can check that the Colony accepts the credential.
  """
  def status do
    with {:ok, config} <- config() do
      credentials_configured? = match?({:ok, _key}, api_key())

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
    with {:ok, config} <- config() do
      cond do
        not config.enabled? -> {:error, :colony_disabled}
        config.mode != :observe -> {:error, :unsupported_colony_mode}
        true -> {:ok, config}
      end
    end
  end

  @doc false
  def api_key do
    # If set, even an empty environment variable takes precedence over the
    # file. This permits tests and operators to explicitly disable file use.
    case System.get_env("COLONY_API_KEY") do
      value when is_binary(value) -> validate_key(value)
      nil -> read_key_file()
    end
  end

  defp read_key_file do
    with {:ok, %{type: :regular, mode: mode}} <- File.stat(@key_file),
         true <- (mode &&& 0o077) == 0,
         {:ok, key} <- File.read(@key_file) do
      validate_key(key)
    else
      false -> {:error, :insecure_colony_key_file}
      _ -> {:error, :missing_colony_api_key}
    end
  end

  defp validate_key(value) do
    key = String.trim(value)

    if String.starts_with?(key, "col_") and byte_size(key) > 4 do
      {:ok, key}
    else
      {:error, :missing_colony_api_key}
    end
  end

  defp validate(config) do
    cond do
      Keyword.fetch!(config, :enabled?) not in [true, false] ->
        {:error, {:invalid_colony_config, :enabled?}}

      Keyword.fetch!(config, :mode) != :observe ->
        {:error, {:invalid_colony_config, :mode}}

      # Never send a key or JWT to a configurable/untrusted host.
      Keyword.fetch!(config, :api_base) != @api_base ->
        {:error, {:invalid_colony_config, :api_base}}

      not bounded_integer?(Keyword.fetch!(config, :max_posts), 1, 50) ->
        {:error, {:invalid_colony_config, :max_posts}}

      not bounded_integer?(Keyword.fetch!(config, :timeout_ms), 1_000, 30_000) ->
        {:error, {:invalid_colony_config, :timeout_ms}}

      true ->
        :ok
    end
  end

  defp bounded_integer?(value, min, max) do
    is_integer(value) and value >= min and value <= max
  end
end
