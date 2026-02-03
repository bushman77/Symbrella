defmodule Brain.Episodes.Writer do
  @moduledoc """
  Episodes persistence helpers (sync/async) with minimal telemetry.
  """
  require Logger
  alias Db.Episodes, as: Episodes

  @type mode :: :off | :sync | :async

  @spec normalize_episode_mode(any()) :: mode()
  def normalize_episode_mode(:off), do: :off
  def normalize_episode_mode(:sync), do: :sync
  def normalize_episode_mode(:async), do: :async
  def normalize_episode_mode(:on), do: :async      # ✅ add this
  def normalize_episode_mode(_other), do: :async

  @spec store(map(), [binary()], any()) :: :ok
  def store(si, tags, mode) do
    case normalize_episode_mode(mode) do
      :off -> :ok
      :sync -> do_store_episode(si, tags, async_embedding?: false)
      :async ->
        Task.start(fn -> do_store_episode(si, tags, async_embedding?: true) end)
        :ok
    end
  end

  defp do_store_episode(si, tags, opts) do
    async_embedding? = Keyword.get(opts, :async_embedding?, true)
    write_opts = [tags: tags] ++ if async_embedding?, do: [async_embedding: true], else: []

    result =
      try do
        Episodes.write_episode(si, write_opts)
      rescue
        e -> {:error, {:exception, Exception.message(e)}}
      catch
        kind, reason -> {:error, {kind, reason}}
      end

    case result do
      {:ok, _ep} ->
        :telemetry.execute([:brain, :episodes, :write], %{ok: 1}, %{mode: mode_tag(async_embedding?), tags: tags})

      {:error, reason} ->
        :telemetry.execute([:brain, :episodes, :write], %{error: 1}, %{reason: inspect(reason), mode: mode_tag(async_embedding?), tags: tags})
    end

    :ok
  end

  defp mode_tag(true), do: :async_embedding
  defp mode_tag(false), do: :sync_embedding
end
