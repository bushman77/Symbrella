defmodule Llm.BootGate do
  @moduledoc """
  Synchronous application boot gate for the local llama-server runner.

  `Llm` owns the external OS process. This worker exists only to make the
  umbrella supervisor wait until `Llm.start_llama/1` has completed successfully
  before later children are started.
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

      case Llm.start_llama(timeout: timeout) do
        {:ok, _info} -> {:ok, %{enabled?: true}}
        {:error, reason} -> {:stop, {:llm_boot_gate_failed, reason}}
      end
    else
      {:ok, %{enabled?: false}}
    end
  end
end
