# lib/brain/episodes/writer.ex
defmodule Brain.Episodes.Writer do
  @moduledoc """
  Episodes persistence helpers (sync/async) with minimal telemetry.
  """
  require Logger
  alias Db.Episodes, as: Episodes

  @type mode :: :off | :sync | :async

  @spec normalize_episode_mode(any()) :: mode()
  def normalize_episode_mode(:off),   do: :off
  def normalize_episode_mode(:sync),  do: :sync
  def normalize_episode_mode(_other), do: :async

  @spec store(map(), [binary()], mode()) :: :ok
  def store(_si, _tags, :off), do: :ok
  def store(si, tags, :sync),  do: do_store_episode(si, tags, async_embedding?: false)
  def store(si, tags, :async)  do
    Task.start(fn -> do_store_episode(si, tags, async_embedding?: true) end)
    :ok
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
        :telemetry.execute([:brain, :episodes, :write], %{ok: 1}, %{mode: mode_tag(async_embedding?)})

      {:error, reason} ->
        :telemetry.execute(
          [:brain, :episodes, :write],
          %{error: 1},
          %{reason: inspect(reason), mode: mode_tag(async_embedding?)}
        )
    end

    :ok
  end

  defp mode_tag(true),  do: :async
  defp mode_tag(false), do: :sync
end

