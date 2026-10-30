defmodule Llm.Colony.Bootstrap do
  @moduledoc """
  One-time, explicitly opted-in Colony credential provisioning at startup.

  An existing agent is never silently replaced. Set COLONY_REGISTER_USERNAME
  only when intentionally registering a NEW agent. The permanent API key is
  persisted to the existing umbrella-root priv/.colony_api_key with mode 0600
  BEFORE the remote registration is confirmed. JWT refresh stays in TokenCache.
  """

  use GenServer
  import Bitwise
  require Logger

  alias Llm.Colony.Adapter

  @base "https://thecolony.ai/api/v1"
  @username_pattern ~r/\A[a-z0-9-]{3,32}\z/

  def start_link(_opts), do: GenServer.start_link(__MODULE__, %{}, name: __MODULE__)

  @doc "Non-secret result of the startup bootstrap."
  def status, do: GenServer.call(__MODULE__, :status)

  @impl true
  def init(_) do
    if Application.get_env(:llm, :colony_bootstrap_enabled?, true) do
      {:ok, %{status: :starting}, {:continue, :bootstrap}}
    else
      {:ok, %{status: {:ok, :disabled}}}
    end
  end

  @impl true
  def handle_continue(:bootstrap, state) do
    result =
      try do
        ensure_key()
      rescue
        _ -> {:error, :bootstrap_failed}
      end

    case result do
      {:ok, :registered} ->
        Logger.info("[Colony] New agent credential registered and stored locally")

      {:ok, :environment_key_saved} ->
        Logger.info("[Colony] Existing environment credential saved to private key file")

      {:error, :registration_not_configured} ->
        Logger.warning(
          "[Colony] API key missing. Restore the existing agent key or explicitly set " <>
            "COLONY_REGISTER_USERNAME to create a new agent"
        )

      {:error, reason} ->
        Logger.warning("[Colony] Credential bootstrap unavailable: #{inspect(reason)}")

      _ ->
        :ok
    end

    {:noreply, %{state | status: result}}
  end

  @impl true
  def handle_call(:status, _from, state), do: {:reply, state.status, state}

  @doc """
  Ensure a permanent key is available. No registration occurs unless a new
  username was explicitly supplied in COLONY_REGISTER_USERNAME.

  Options permit isolated, network-free tests. Never return a credential.
  """
  def ensure_key(opts \\ []) when is_list(opts) do
    with {:ok, config} <- Adapter.config() do
      if config.enabled? do
        path = Keyword.get(opts, :key_file, Adapter.key_file())
        env_key = Keyword.get(opts, :api_key, System.get_env("COLONY_API_KEY"))

        case read_key(path) do
          {:ok, saved_key} ->
            case env_key do
              nil ->
                {:ok, :existing_key}

              candidate ->
                if valid_key(candidate) == {:ok, saved_key} do
                  {:ok, :existing_key}
                else
                  {:error, :credential_sources_conflict}
                end
            end

          :missing ->
            if is_binary(env_key) do
              with {:ok, key} <- valid_key(env_key),
                   :ok <- persist_key(path, key) do
                {:ok, :environment_key_saved}
              end
            else
              register_new(path, opts)
            end

          {:error, _} = error ->
            error
        end
      else
        {:ok, :disabled}
      end
    end
  end

  defp register_new(path, opts) do
    username = Keyword.get(opts, :username, System.get_env("COLONY_REGISTER_USERNAME"))

    if is_binary(username) and Regex.match?(@username_pattern, username) do
      display_name = Keyword.get(opts, :display_name, System.get_env("COLONY_REGISTER_DISPLAY_NAME", "Symbrella"))

      if is_binary(display_name) and String.length(display_name) in 1..100 do
        with {:ok, %{"api_key" => issued, "claim_token" => claim}} <-
               post("/auth/register/begin", %{
                 username: username,
                 display_name: display_name,
                 bio: "Local neuro-symbolic AI; observing Colony discussions without posting"
               }, opts),
             {:ok, key} <- valid_key(issued),
             true <- is_binary(claim) and byte_size(claim) > 0,
             :ok <- persist_key(path, key),
             {:ok, ^key} <- read_key(path),
             {:ok, _} <-
               post("/auth/register/confirm", %{
                 claim_token: claim,
                 key_fingerprint: binary_part(key, byte_size(key) - 6, 6)
               }, opts) do
          {:ok, :registered}
        else
          false -> {:error, :invalid_registration_response}
          {:ok, _other} -> {:error, :key_readback_mismatch}
          {:error, _} = error -> error
          _ -> {:error, :invalid_registration_response}
        end
      else
        {:error, :invalid_registration_display_name}
      end
    else
      if is_nil(username),
        do: {:error, :registration_not_configured},
        else: {:error, :invalid_registration_username}
    end
  end

  defp valid_key(candidate) when is_binary(candidate) do
    key = String.trim(candidate)

    if String.starts_with?(key, "col_") and byte_size(key) >= 10,
      do: {:ok, key},
      else: {:error, :invalid_colony_api_key}
  end

  defp valid_key(_), do: {:error, :invalid_colony_api_key}

  defp read_key(path) do
    case File.lstat(path) do
      {:ok, %File.Stat{type: :regular, mode: mode}} when (mode &&& 0o077) == 0 ->
        with {:ok, bytes} <- File.read(path) do
          valid_key(bytes)
        else
          {:error, _} -> {:error, :key_file_unreadable}
        end

      {:ok, _} ->
        {:error, :unsafe_key_file}

      {:error, :enoent} ->
        :missing

      {:error, _} ->
        {:error, :key_file_unreadable}
    end
  end

  # Create a mode-0600 temporary file, fsync it, and link it into place only
  # if the destination does not exist. Never overwrite another agent's key.
  defp persist_key(path, key) do
    temporary = path <> ".tmp." <> Integer.to_string(System.unique_integer([:positive, :monotonic]))

    case File.open(temporary, [:write, :exclusive, :binary]) do
      {:ok, io} ->
        result =
          with :ok <- File.chmod(temporary, 0o600),
               :ok <- IO.binwrite(io, key <> "\n"),
               :ok <- :file.sync(io) do
            :ok
          end

        close_result = File.close(io)

        outcome =
          with :ok <- result,
               :ok <- close_result do
            case File.ln(temporary, path) do
              :ok ->
                :ok

              {:error, :eexist} ->
                {:error, :key_file_already_exists}

              # Some Android/Termux mounts forbid hard links. Preserve the
              # exclusive-create (never overwrite) guarantee without requiring
              # hard-link support. Permissions are set BEFORE key bytes are
              # written to the newly created destination file.
              {:error, reason} when reason in [:eperm, :eacces, :enotsup, :enosys, :exdev] ->
                persist_key_without_links(path, key)

              {:error, _} ->
                {:error, :key_file_write_failed}
            end
          else
            {:error, _} -> {:error, :key_file_write_failed}
          end

        _ = File.rm(temporary)
        outcome

      {:error, _} ->
        {:error, :key_file_write_failed}
    end
  end

  # Fallback for filesystems without hard links. Exclusive creation prevents
  # silently replacing an existing identity; the file is permission-restricted
  # before any credential bytes are written. Delete only files created by us
  # if a write, sync, or close fails.
  defp persist_key_without_links(path, key) do
    case File.open(path, [:write, :exclusive, :binary]) do
      {:ok, io} ->
        result =
          with :ok <- File.chmod(path, 0o600),
               :ok <- IO.binwrite(io, key <> "\n"),
               :ok <- :file.sync(io) do
            :ok
          else
            _ -> {:error, :key_file_write_failed}
          end

        closed = File.close(io)

        case {result, closed} do
          {:ok, :ok} ->
            :ok

          _ ->
            _ = File.rm(path)
            {:error, :key_file_write_failed}
        end

      {:error, :eexist} ->
        {:error, :key_file_already_exists}

      {:error, _} ->
        {:error, :key_file_write_failed}
    end
  end

  defp post(path, payload, opts) do
    case Keyword.get(opts, :request) do
      fun when is_function(fun, 2) ->
        fun.(path, payload)

      _ ->
        case Req.post(
               url: @base <> path,
               json: payload,
               receive_timeout: 5_000,
               retry: false,
               redirect: false
             ) do
          {:ok, %Req.Response{status: status, body: %{} = body}} when status in 200..299 ->
            {:ok, body}

          {:ok, %Req.Response{status: 409}} ->
            {:error, :username_unavailable}

          {:ok, %Req.Response{status: status}} ->
            {:error, {:registration_http_status, status}}

          {:error, _} ->
            {:error, :registration_transport_error}
        end
    end
  end
end
