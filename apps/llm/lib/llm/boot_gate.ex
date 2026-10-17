defmodule Llm.BootGate do
  @moduledoc """
  Configurable local llama-server bootstrap.

  In the default degraded mode this GenServer starts quickly and boots the
  model in handle_continue, allowing independent symbolic services and the
  web interface to become available. Strict mode preserves fail-fast startup.
  """

  use GenServer

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @impl true
  def init(opts) do
    config = Application.get_env(:llm, __MODULE__, [])

    enabled? =
      Keyword.get_lazy(opts, :enabled?, fn ->
        Keyword.get(config, :enabled?, false)
      end)

    if enabled? do
      timeout = Keyword.get(opts, :timeout, Keyword.get(config, :timeout, 120_000))
      mode = Keyword.get(opts, :mode, Keyword.get(config, :mode, :degraded))

      case mode do
        :strict ->
          case Llm.start_llama(timeout: timeout) do
            {:ok, _info} -> {:ok, %{enabled?: true, mode: :strict, status: :ready}}
            {:error, reason} -> {:stop, {:llm_boot_gate_failed, reason}}
          end

        :degraded ->
          {:ok, %{enabled?: true, mode: :degraded, timeout: timeout, status: :starting},
           {:continue, :start_llm}}

        other ->
          {:stop, {:invalid_llm_boot_mode, other}}
      end
    else
      {:ok, %{enabled?: false, status: :disabled}}
    end
  end

  @impl true
  def handle_continue(:start_llm, %{timeout: timeout} = state) do
    case Llm.start_llama(timeout: timeout) do
      {:ok, _info} ->
        {:noreply, %{state | status: :ready}}

      {:error, reason} ->
        require Logger
        Logger.warning("[llm/boot] unavailable: #{inspect(reason)}; symbolic runtime remains active")
        {:noreply, %{state | status: {:unavailable, reason}}}
    end
  end
end
