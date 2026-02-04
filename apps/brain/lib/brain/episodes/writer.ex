defmodule Brain.Episodes.Writer do
  @moduledoc """
  Episode persistence wrapper for Brain-side callers.

  - Respects `:brain, :episodes_persist` (defaults off unless configured).
  - Respects `:brain, :episodes_tags` for global tags.
  - Emits `[:brain, :episodes, :write]` telemetry for ok/fail/skipped.

  Public helpers:
    * normalize_episode_mode/1
    * async_embedding?/1
  """

  require Logger
  alias Db.Episodes

  @tele [:brain, :episodes, :write]

  @type mode :: :on | :off | :async | :async_embedding

  @spec store(map(), [String.t()], mode()) :: :ok
  def store(%{} = si_or_payload, tags \\ [], mode \\ :on) do
    persist? = episodes_persist?()
    tags = normalize_tags(tags)
    mode = normalize_episode_mode(mode)
    async_embedding? = async_embedding?(mode)

    cond do
      mode == :off ->
        :telemetry.execute(@tele, %{skipped: 1}, %{mode: :off, async_embedding?: false, tags: tags})
        :ok

      not persist? ->
        # Always emit telemetry so debugging doesn't go dark when persistence is off.
        :telemetry.execute(@tele, %{skipped: 1}, %{mode: mode, async_embedding?: async_embedding?, tags: tags})
        :ok

      mode in [:async, :async_embedding] ->
        Task.start(fn -> do_store(si_or_payload, tags, mode) end)
        :ok

      true ->
        _ = do_store(si_or_payload, tags, mode)
        :ok
    end
  end

  @spec async_embedding?(mode()) :: boolean()
  def async_embedding?(:async_embedding), do: true
  def async_embedding?(_), do: false

  @doc """
  Normalize episode writer mode values (atoms, booleans, strings) into a stable atom.
  """
  @spec normalize_episode_mode(any()) :: mode()
  def normalize_episode_mode(m) when m in [:on, :off, :async, :async_embedding], do: m
  def normalize_episode_mode(true), do: :on
  def normalize_episode_mode(false), do: :off
  def normalize_episode_mode("on"), do: :on
  def normalize_episode_mode("off"), do: :off
  def normalize_episode_mode("async"), do: :async
  def normalize_episode_mode("async_embedding"), do: :async_embedding
  def normalize_episode_mode(_), do: :on

  # ───────────────────────────────────────────────────────────────────
  # internals
  # ───────────────────────────────────────────────────────────────────

  defp do_store(%{} = si_or_payload, tags, mode) do
    async_embedding? = async_embedding?(mode)

    try do
      case Episodes.write_episode(si_or_payload, tags: tags, async_embedding: async_embedding?) do
        {:ok, _ep} ->
          :telemetry.execute(@tele, %{ok: 1}, %{mode: mode, async_embedding?: async_embedding?, tags: tags})
          :ok

        {:error, reason} ->
          Logger.warning(
            "[Episodes] write failed mode=#{mode} async_embedding?=#{async_embedding?} reason=#{inspect(reason)}"
          )

          :telemetry.execute(
            @tele,
            %{fail: 1},
            %{mode: mode, async_embedding?: async_embedding?, tags: tags, reason: inspect(reason)}
          )

          :error
      end
    rescue
      e ->
        Logger.warning(
          "[Episodes] crash mode=#{mode} async_embedding?=#{async_embedding?} err=#{Exception.message(e)}"
        )

        :telemetry.execute(
          @tele,
          %{fail: 1},
          %{mode: mode, async_embedding?: async_embedding?, tags: tags, reason: Exception.message(e)}
        )

        :error
    end
  end

  defp episodes_persist? do
    case Application.get_env(:brain, :episodes_persist, :off) do
      :on -> true
      true -> true
      "on" -> true
      _ -> false
    end
  end

  defp normalize_tags(tags) when is_list(tags) do
    base =
      Application.get_env(:brain, :episodes_tags, [])
      |> List.wrap()
      |> Enum.map(&to_string/1)

    (tags ++ base)
    |> Enum.map(&to_string/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end
end
