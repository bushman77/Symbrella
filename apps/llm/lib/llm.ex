# apps/llm/lib/llm.ex
defmodule Llm do
  @moduledoc """
  GenServer + Tesla client for Ollama, strict and modular.

  • Daemon stays up; models load/unload on demand.
  • **No auto model load on boot** by default (`warm_on_boot?` = false).
  • Heartbeat will **not** auto-start the daemon unless we started it first.
  • Deterministic runner defaults via Llm.Const.
  """


  use GenServer
  require Logger
  alias Llm.Pos
  alias Llm.{Daemon, ModelControl, Inference}

  @type message :: %{required(:role) => String.t(), required(:content) => String.t()}

  @finch_default __MODULE__.Finch
  @default_base "http://localhost:11434"
  @default_model "smollm2:135m"

  @pos_call_timeout_default 60_000
  @strict_unload_verify_attempts 12
  @strict_unload_verify_sleep_ms 150
  @default_heartbeat_ms 15_000

  # ───────────── Public API ─────────────

  def hello(), do: :world

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @spec child_spec(keyword()) :: Supervisor.child_spec()
  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :name, __MODULE__),
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 5_000,
      type: :worker
    }
  end

  @spec start_ollama(String.t() | nil, keyword()) :: {:ok, map()} | {:error, term()}
  def start_ollama(model \\ nil, opts \\ []) do
    GenServer.call(
      server(opts),
      {:start_ollama, model || @default_model, opts},
      call_timeout(opts) + 60_000
    )
  end

  def stop_ollama(opts \\ []) do
    GenServer.call(server(opts), :stop_ollama, call_timeout(opts))
  end

  @spec start_model(String.t() | nil, keyword()) ::
          {:ok, %{served?: boolean(), pulled?: boolean(), warmed?: boolean()}} | {:error, term()}
  def start_model(model \\ nil, opts \\ []) do
    GenServer.call(
      server(opts),
      {:start_model, model || @default_model, opts},
      call_timeout(opts)
    )
  end

  @spec stop_model(String.t() | nil, keyword()) :: :ok | {:error, term()}
  def stop_model(model \\ nil, opts \\ []) do
    GenServer.call(server(opts), {:stop_model, model || @default_model, opts}, call_timeout(opts))
  end

  @spec ps(keyword()) :: {:ok, map()} | {:error, term()}
  def ps(opts \\ []) do
    GenServer.call(server(opts), :ps, call_timeout(opts))
  end

  @spec chat(String.t() | nil, String.t() | [message()], keyword()) ::
          {:ok, %{content: String.t(), raw: map()}} | {:error, term()}
  def chat(model \\ nil, prompt, opts \\ []) do
    GenServer.call(server(opts), {:chat, model, prompt, opts}, call_timeout(opts))
  end

  @spec generate(String.t() | nil, String.t(), keyword()) ::
          {:ok, %{response: String.t(), raw: map()}} | {:error, term()}
  def generate(model \\ nil, prompt, opts \\ []) when is_binary(prompt) do
    GenServer.call(server(opts), {:generate, model, prompt, opts}, call_timeout(opts))
  end

  @spec embeddings(String.t() | [String.t()], keyword()) ::
          {:ok, map() | [map()]} | {:error, term()}
  def embeddings(text_or_list, opts \\ []) do
    GenServer.call(server(opts), {:embeddings, text_or_list, opts}, call_timeout(opts))
  end

  @spec pos(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def pos(word, opts \\ []) when is_binary(word), do: Pos.run(word, opts)

  def config(opts \\ []) do
    GenServer.call(server(opts), :config, call_timeout(opts))
  end

  # ───────────── GenServer ─────────────

  @impl GenServer
  def init(opts) do
    env = Application.get_env(:llm, __MODULE__, [])
    boot = Keyword.merge(env, opts, fn _k, _v1, v2 -> v2 end)

    base_url = Keyword.get(boot, :base_url, System.get_env("OLLAMA_API_BASE") || @default_base)
    model = Keyword.get(boot, :model, @default_model)
    timeout = Keyword.get(boot, :timeout, @pos_call_timeout_default)
    temperature = Keyword.get(boot, :temperature, 0.2)
    pools = Keyword.get(boot, :pools, %{default: [size: 8, count: 1]})
    finch_name = Keyword.get(boot, :finch_name, @finch_default)

    auto_start_on_boot? = Keyword.get(boot, :auto_start_on_boot?, false)
    pull_on_boot? = Keyword.get(boot, :pull_on_boot?, true)
    # 🔻 Default changed: do NOT warm (and hence do not load) any model at boot
    warm_on_boot? = Keyword.get(boot, :warm_on_boot?, false)
    boot_model = Keyword.get(boot, :boot_model, model)
    boot_timeout = Keyword.get(boot, :boot_timeout, timeout)

    warm_on_restart? = Keyword.get(boot, :warm_on_restart?, false)
    pull_on_restart? = Keyword.get(boot, :pull_on_restart?, false)
    restart_model = Keyword.get(boot, :restart_model, model)

    auto_restart_on_crash? = Keyword.get(boot, :auto_restart_on_crash?, true)
    heartbeat_ms = Keyword.get(boot, :heartbeat_ms, @default_heartbeat_ms)

    {:ok, _} = Finch.start_link(name: finch_name, pools: pools)

    state = %{
      base_url: base_url,
      model: model,
      timeout: timeout,
      temperature: temperature,
      finch: finch_name,
      ollama_port: nil,
      served_by_us?: false,
      warmed_models: MapSet.new(),
      auto_start_on_boot?: auto_start_on_boot?,
      pull_on_boot?: pull_on_boot?,
      warm_on_boot?: warm_on_boot?,
      boot_model: boot_model,
      boot_timeout: boot_timeout,
      warm_on_restart?: warm_on_restart?,
      pull_on_restart?: pull_on_restart?,
      restart_model: restart_model,
      boot_done?: false,
      auto_restart_on_crash?: auto_restart_on_crash?,
      heartbeat_ms: heartbeat_ms,
      heartbeat_ref: nil,
      manual_stop?: false
    }

    if auto_start_on_boot? do
      {:ok, state, {:continue, :maybe_boot_autostart}}
    else
      {:ok, schedule_heartbeat(state)}
    end
  end

  @impl GenServer
  def handle_continue(:maybe_boot_autostart, %{boot_done?: true} = state),
    do: {:noreply, schedule_heartbeat(state)}

  @impl GenServer
  def handle_continue(:maybe_boot_autostart, state) do
    t = state.boot_timeout
    m = state.boot_model || state.model

    state2 =
      case Daemon.ensure_serving(state, t) do
        {:ok, st, _spawned?} ->
          _ = if state.pull_on_boot?, do: ModelControl.pull_model(m), else: true

          st2 =
            if state.warm_on_boot? do
              case ModelControl.maybe_warm_model(st, m, t) do
                {:ok, st3} ->
                  st3

                {:error, reason} ->
                  Logger.debug("Llm boot warm skipped: #{inspect(reason)}")
                  st

                _ ->
                  st
              end
            else
              st
            end

          st2

        {:error, st, reason} ->
          Logger.debug("Llm boot autostart skipped: #{inspect(reason)}")
          st
      end

    {:noreply, state2 |> Map.put(:boot_done?, true) |> schedule_heartbeat()}
  end

  @impl GenServer
  def terminate(_reason, state) do
    cancel_heartbeat(state)
    if state.served_by_us? and is_port(state.ollama_port), do: Port.close(state.ollama_port)
    :ok
  end

  @impl GenServer
  def handle_call(:config, _from, state), do: {:reply, state, state}

  @impl GenServer
  def handle_call(:stop_ollama, _from, state) do
    state1 = %{state | manual_stop?: true}

    cond do
      state1.served_by_us? and is_port(state1.ollama_port) ->
        Port.close(state1.ollama_port)

        {:reply, :ok,
         %{state1 | ollama_port: nil, served_by_us?: false, warmed_models: MapSet.new()}
         |> schedule_heartbeat()}

      true ->
        {:reply, {:error, :not_started_by_us}, schedule_heartbeat(state1)}
    end
  end

  # start_ollama/2 (defaults updated so it does NOT warm a model)
  @impl GenServer
  def handle_call({:start_ollama, model, opts}, _from, state) do
    pull? = Keyword.get(opts, :pull?, true)
    # 🔻 Default changed: do NOT warm here unless explicitly asked
    warm? = Keyword.get(opts, :warm?, false)
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    m = model || state.model

    case Daemon.ensure_serving(state, http_timeout) do
      {:ok, st2, spawned?} ->
        _ = if pull?, do: ModelControl.pull_model(m), else: true

        {warmed?, st3} =
          if warm? do
            case ModelControl.maybe_warm_model(st2, m, http_timeout) do
              {:ok, st} -> {true, st}
              _ -> {false, st2}
            end
          else
            {false, st2}
          end

        reply =
          {:ok,
           %{
             served?: spawned?,
             pulled?: pull?,
             warmed?: warmed?,
             base_url: st3.base_url,
             model: m
           }}

        {:reply, reply, schedule_heartbeat(st3)}

      {:error, st2, reason} ->
        {:reply, {:error, reason}, schedule_heartbeat(st2)}
    end
  end

  @impl GenServer
  def handle_call(:ps, _from, state) do
    reply = ModelControl.list_loaded_models(state, timeout: state.timeout)
    {:reply, reply, schedule_heartbeat(state)}
  end

  @impl GenServer
  def handle_call({:start_model, model, opts}, _from, state) do
    pull? = Keyword.get(opts, :pull?, true)
    warm? = Keyword.get(opts, :warm?, true)
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    m = model || state.model

    case Inference.ensure_autostart_and_warm(state, m,
           pull?: pull?,
           warm?: warm?,
           timeout: http_timeout
         ) do
      {:ok, st3, warmed?} ->
        {:reply, {:ok, %{served?: true, pulled?: true, warmed?: warmed?}},
         schedule_heartbeat(st3)}

      {:error, reason, st2} ->
        {:reply, {:error, reason}, schedule_heartbeat(st2)}
    end
  end

  @impl GenServer
  def handle_call({:stop_model, model, opts}, _from, state) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    m = model || state.model

    case ModelControl.unload_model_strict(state, m,
           timeout: http_timeout,
           attempts: @strict_unload_verify_attempts,
           sleep_ms: @strict_unload_verify_sleep_ms
         ) do
      :ok -> {:reply, :ok, schedule_heartbeat(drop_warmed(state, m))}
      {:error, reason} -> {:reply, {:error, reason}, schedule_heartbeat(state)}
    end
  end

  @impl GenServer
  def handle_call({:chat, model, prompt, opts}, _from, state) do
    case Inference.chat(model, prompt, opts, state) do
      {:ok, %{content: content, raw: raw}, st2} ->
        {:reply, {:ok, %{content: content, raw: raw}}, schedule_heartbeat(st2)}

      {:error, reason, st2} ->
        {:reply, {:error, reason}, schedule_heartbeat(st2)}
    end
  end

  @impl GenServer
  def handle_call({:generate, model, prompt, opts}, _from, state) do
    case Inference.generate(model, prompt, opts, state) do
      {:ok, %{response: resp, raw: raw}, st2} ->
        {:reply, {:ok, %{response: resp, raw: raw}}, schedule_heartbeat(st2)}

      {:error, reason, st2} ->
        {:reply, {:error, reason}, schedule_heartbeat(st2)}
    end
  end

  @impl GenServer
  def handle_call({:embeddings, text, opts}, _from, state) when is_binary(text) do
    case Inference.embeddings_one(text, opts, state) do
      {:ok, %{embeddings: vec, raw: raw}, st2} ->
        {:reply, {:ok, %{embeddings: vec, raw: raw}}, schedule_heartbeat(st2)}

      {:error, reason, st2} ->
        {:reply, {:error, reason}, schedule_heartbeat(st2)}
    end
  end

  @impl GenServer
  def handle_call({:embeddings, list, opts}, _from, state) when is_list(list) do
    case Inference.embeddings_batch(list, opts, state) do
      {:ok, out, st2} ->
        {:reply, {:ok, out}, schedule_heartbeat(st2)}

      {:error, reason, st2} ->
        {:reply, {:error, reason}, schedule_heartbeat(st2)}
    end
  end

  # ───────────── Internals ─────────────

  defp server(opts), do: Keyword.get(opts, :name, __MODULE__)
  defp call_timeout(opts), do: Keyword.get(opts, :timeout, @pos_call_timeout_default)

  defp drop_warmed(%{warmed_models: warmed} = state, model) do
    %{state | warmed_models: MapSet.delete(warmed, model)}
  end

  defp schedule_heartbeat(%{heartbeat_ms: ms} = state) do
    cancel_heartbeat(state)

    case is_integer(ms) and ms > 0 do
      true ->
        ref = Process.send_after(self(), :heartbeat, ms)
        %{state | heartbeat_ref: ref}

      false ->
        %{state | heartbeat_ref: nil}
    end
  end

  defp cancel_heartbeat(%{heartbeat_ref: nil}), do: :ok

  defp cancel_heartbeat(%{heartbeat_ref: ref}) when is_reference(ref) do
    Process.cancel_timer(ref, info: false)
    :ok
  end

  @impl true
  def handle_info(:heartbeat, state) do
    state2 =
      case {state.manual_stop?, state.auto_restart_on_crash?, state.served_by_us?} do
        # Respect manual stop
        {true, _, _} ->
          state

        # Only auto-restart if WE started the daemon before (crash recovery),
        # and it's currently unreachable.
        {false, true, true} ->
          if Daemon.reachable?(state, min(state.timeout, 3_000)) do
            state
          else
            Logger.debug("Llm heartbeat: service down; attempting restart…")

            case Daemon.ensure_serving(state, state.timeout) do
              {:ok, st2, _spawned?} ->
                _ =
                  if state.pull_on_restart?,
                    do: ModelControl.pull_model(state.restart_model || st2.model),
                    else: true

                if state.warm_on_restart? do
                  case ModelControl.maybe_warm_model(
                         st2,
                         state.restart_model || st2.model,
                         st2.timeout
                       ) do
                    {:ok, stx} -> stx
                    _ -> st2
                  end
                else
                  st2
                end

              {:error, st2, reason} ->
                Logger.debug("Llm heartbeat restart failed: #{inspect(reason)}")
                st2
            end
          end

        # Do nothing if we never started the daemon (no implicit autostart).
        _ ->
          state
      end

    {:noreply, schedule_heartbeat(state2)}
  end

  @impl true
  def handle_info({_port, {:data, _bin}}, state), do: {:noreply, state}

  @impl true
  def handle_info({port, {:exit_status, code}}, %{ollama_port: port} = state) do
    Logger.debug("Llm port exited with status #{code}")
    state1 = %{state | ollama_port: nil, served_by_us?: false, warmed_models: MapSet.new()}

    state2 =
      cond do
        state1.manual_stop? ->
          state1

        not state1.auto_restart_on_crash? ->
          state1

        true ->
          Logger.debug("Llm auto-restart on crash…")

          case Daemon.ensure_serving(state1, state1.timeout) do
            {:ok, st2, _spawned?} ->
              _ =
                if state1.pull_on_restart?,
                  do: ModelControl.pull_model(state1.restart_model || st2.model),
                  else: true

              if state1.warm_on_restart? do
                case ModelControl.maybe_warm_model(
                       st2,
                       state1.restart_model || st2.model,
                       st2.timeout
                     ) do
                  {:ok, stx} -> stx
                  _ -> st2
                end
              else
                st2
              end

            {:error, st2, reason} ->
              Logger.debug("Llm auto-restart failed: #{inspect(reason)}")
              st2
          end
      end

    {:noreply, schedule_heartbeat(state2)}
  end

  def handle_info(_msg, state), do: {:noreply, state}
end
