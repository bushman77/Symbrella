# apps/llm/lib/llm.ex
defmodule Llm do
  @moduledoc """
  Symbrella-owned llama.cpp runner.

  This GenServer *owns* the external `llama-server` OS process and exposes a small
  OpenAI-compatible HTTP client (Finch) for:
    - GET  /v1/models
    - POST /v1/chat/completions
    - POST /v1/embeddings

  Autostart:
    - If `auto_start_on_boot?: true`, we start `llama-server` in `handle_continue/2`.

  Safety / correctness:
    - We resolve `llama_server` via PATH (System.find_executable/1) for spawn_executable.
    - Heartbeat + Port exit_status handle crash/restart (with backoff).
    - Manual stop prevents auto-restart until explicitly started again.
  """

  use GenServer
  require Logger

  @type message :: %{required(:role) => String.t(), required(:content) => String.t()}

  @finch_default __MODULE__.Finch

  @default_host "127.0.0.1"
  @default_ctx 2048
  @default_threads 4
  @default_temperature 0.4

  @call_timeout_default 60_000
  @default_heartbeat_ms 15_000

  @ready_poll_attempts 80
  @ready_poll_sleep_ms 250

  @log_ring_max 200

  @backoff_min 250
  @backoff_max 10_000

  # ────────────────────────────────────────────────────────────────────────────
  # Public API
  # ────────────────────────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def child_spec(opts) do
    %{
      id: Keyword.get(opts, :name, __MODULE__),
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 5_000,
      type: :worker
    }
  end

  def start_llama(opts \\ []) do
    GenServer.call(server(opts), {:start_llama, opts}, call_timeout(opts) + 60_000)
  end

  def stop_llama(opts \\ []) do
    GenServer.call(server(opts), :stop_llama, call_timeout(opts))
  end

  def status(opts \\ []) do
    GenServer.call(server(opts), :status, call_timeout(opts))
  end

  def logs(opts \\ []) do
    GenServer.call(server(opts), :logs, call_timeout(opts))
  end

  def models(opts \\ []) do
    GenServer.call(server(opts), :models, call_timeout(opts))
  end

  def chat(prompt, opts \\ []) do
    GenServer.call(server(opts), {:chat, prompt, opts}, call_timeout(opts))
  end

  def embeddings(text_or_list, opts \\ []) do
    GenServer.call(server(opts), {:embeddings, text_or_list, opts}, call_timeout(opts))
  end

  # ────────────────────────────────────────────────────────────────────────────
  # GenServer
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def init(opts) do
    env = Application.get_env(:llm, __MODULE__, [])
    boot = Keyword.merge(env, opts, fn _k, _v1, v2 -> v2 end)

    finch_name = Keyword.get(boot, :finch_name, @finch_default)
    pools = Keyword.get(boot, :pools, %{default: [size: 8, count: 1]})

    case Finch.start_link(name: finch_name, pools: pools) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
      {:error, reason} -> Logger.warning("Llm Finch start failed: #{inspect(reason)}")
    end

    model_path = Keyword.get(boot, :model_path) || System.get_env("LLAMA_MODEL_PATH")

    llama_server =
      Keyword.get(boot, :llama_server, System.get_env("LLAMA_SERVER") || "llama-server")

    state = %{
      finch: finch_name,
      timeout: Keyword.get(boot, :timeout, @call_timeout_default),

      # runner config
      llama_server: llama_server,
      model_path: model_path,
      host: Keyword.get(boot, :host, @default_host),
      port: Keyword.get(boot, :port, 0),
      ctx: Keyword.get(boot, :ctx, @default_ctx),
      threads: Keyword.get(boot, :threads, @default_threads),
      temperature: Keyword.get(boot, :temperature, @default_temperature),

      # policy
      auto_start_on_boot?: Keyword.get(boot, :auto_start_on_boot?, false),
      allow_lazy_start?: Keyword.get(boot, :allow_lazy_start?, true),
      auto_restart_on_crash?: Keyword.get(boot, :auto_restart_on_crash?, true),
      heartbeat_ms: Keyword.get(boot, :heartbeat_ms, @default_heartbeat_ms),

      # runtime
      status: :stopped,         # :stopped | :starting | :ready | :crashed | :failed
      llama_port: nil,
      served_by_us?: false,
      endpoint: nil,
      manual_stop?: false,

      # restart control
      restart_backoff_ms: @backoff_min,
      restart_timer_ref: nil,
      last_exit_status: nil,

      # logs
      log_ring: :queue.new(),
      log_ring_size: 0,

      # heartbeat
      heartbeat_ref: nil
    }

    state = schedule_heartbeat(state)

    if state.auto_start_on_boot? do
      {:ok, state, {:continue, :boot_start}}
    else
      {:ok, state}
    end
  end

  @impl true
  def handle_continue(:boot_start, state) do
    case ensure_serving(state, state.timeout, allow_start?: true) do
      {:ok, st} ->
        {:noreply, st}

      {:error, st, reason} ->
        Logger.warning("Llm autostart failed: #{inspect(reason)}")
        {:noreply, st}
    end
  end

  @impl true
  def terminate(_reason, state) do
    cancel_heartbeat(state)
    cancel_restart_timer(state)

    if state.served_by_us? and is_port(state.llama_port) do
      Port.close(state.llama_port)
    end

    :ok
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Calls
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def handle_call(:status, _from, state) do
    reachable? =
      case state.endpoint do
        nil -> false
        _ -> reachable?(state, 750)
      end

    reply =
      %{
        status: state.status,
        served_by_us?: state.served_by_us?,
        manual_stop?: state.manual_stop?,
        endpoint: state.endpoint,
        model_path: state.model_path,
        ctx: state.ctx,
        threads: state.threads,
        reachable?: reachable?,
        last_exit_status: state.last_exit_status,
        restart_backoff_ms: state.restart_backoff_ms
      }

    {:reply, {:ok, reply}, state}
  end

  @impl true
  def handle_call(:logs, _from, state), do: {:reply, {:ok, logs_to_list(state)}, state}

  @impl true
  def handle_call(:stop_llama, _from, state) do
    state = %{state | manual_stop?: true}

    cond do
      state.served_by_us? and is_port(state.llama_port) ->
        Port.close(state.llama_port)

        {:reply, :ok,
         state
         |> drop_runner_state(:stopped)
         |> reset_backoff()}

      true ->
        {:reply, {:error, :not_started_by_us}, state}
    end
  end

  @impl true
  def handle_call({:start_llama, opts}, _from, state) do
    state =
      state
      |> apply_overrides(opts)
      |> Map.put(:manual_stop?, false)

    case ensure_serving(state, Keyword.get(opts, :timeout, state.timeout), allow_start?: true) do
      {:ok, st} ->
        {:reply, {:ok, %{endpoint: st.endpoint, model_path: st.model_path}}, st}

      {:error, st, reason} ->
        {:reply, {:error, reason}, st}
    end
  end

  @impl true
  def handle_call(:models, _from, state) do
    with {:ok, st} <- ensure_ready(state, min(state.timeout, 8_000)),
         {:ok, raw} <- http_get(st, "/v1/models") do
      {:reply, {:ok, raw}, st}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
      {:error, st, reason} -> {:reply, {:error, reason}, st}
    end
  end

  @impl true
  def handle_call({:chat, prompt, opts}, _from, state) do
    temperature = Keyword.get(opts, :temperature, state.temperature)
    timeout = Keyword.get(opts, :timeout, state.timeout)

    with {:ok, st} <- ensure_ready(state, timeout),
         messages <- normalize_messages(prompt),
         body <- %{
           "model" => "local",
           "messages" => messages,
           "temperature" => temperature,
           "stream" => false
         },
         {:ok, raw} <- http_post(st, "/v1/chat/completions", body, timeout: timeout),
         content <- extract_chat_content(raw) do
      {:reply, {:ok, %{content: content, raw: raw}}, st}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
      {:error, st, reason} -> {:reply, {:error, reason}, st}
    end
  end

  @impl true
  def handle_call({:embeddings, text, opts}, _from, state) when is_binary(text) do
    timeout = Keyword.get(opts, :timeout, state.timeout)

    with {:ok, st} <- ensure_ready(state, timeout),
         body <- %{"model" => "local", "input" => text},
         {:ok, raw} <- http_post(st, "/v1/embeddings", body, timeout: timeout),
         vec <- extract_embedding_one(raw) do
      {:reply, {:ok, %{embeddings: vec, raw: raw}}, st}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
      {:error, st, reason} -> {:reply, {:error, reason}, st}
    end
  end

  @impl true
  def handle_call({:embeddings, list, opts}, _from, state) when is_list(list) do
    timeout = Keyword.get(opts, :timeout, state.timeout)

    with {:ok, st} <- ensure_ready(state, timeout),
         body <- %{"model" => "local", "input" => list},
         {:ok, raw} <- http_post(st, "/v1/embeddings", body, timeout: timeout),
         out <- extract_embedding_batch(raw) do
      {:reply, {:ok, out}, st}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
      {:error, st, reason} -> {:reply, {:error, reason}, st}
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Heartbeat + Port messages
  # ────────────────────────────────────────────────────────────────────────────

  @impl true
  def handle_info(:heartbeat, state) do
    state =
      cond do
        state.manual_stop? ->
          state

        state.auto_restart_on_crash? and state.served_by_us? ->
          if reachable?(state, min(state.timeout, 2_500)) do
            state
          else
            Logger.debug("Llm heartbeat: llama-server unreachable; scheduling restart…")
            schedule_restart(state, :unreachable)
          end

        true ->
          state
      end

    {:noreply, schedule_heartbeat(state)}
  end

  @impl true
  def handle_info({port, {:data, bin}}, %{llama_port: port} = state) when is_binary(bin) do
    {:noreply, push_log(state, bin)}
  end

  @impl true
  def handle_info({port, {:exit_status, code}}, %{llama_port: port} = state) do
    Logger.debug("Llm llama-server exit_status=#{code}")

    state =
      state
      |> push_log("[llama-server exit_status=#{code}]")
      |> Map.put(:last_exit_status, code)
      |> drop_runner_state(:crashed)

    state =
      cond do
        state.manual_stop? ->
          reset_backoff(state)

        not state.auto_restart_on_crash? ->
          reset_backoff(state)

        true ->
          schedule_restart(state, {:exit_status, code})
      end

    {:noreply, state}
  end

  @impl true
  def handle_info(:restart_runner, state) do
    state = %{state | restart_timer_ref: nil}

    cond do
      state.manual_stop? ->
        {:noreply, reset_backoff(state)}

      true ->
        case ensure_serving(state, state.timeout, allow_start?: true) do
          {:ok, st} ->
            {:noreply, reset_backoff(st)}

          {:error, st, reason} ->
            Logger.debug("Llm restart attempt failed: #{inspect(reason)}")
            {:noreply, schedule_restart(st, {:restart_failed, reason})}
        end
    end
  end

  def handle_info(_msg, state), do: {:noreply, state}

  # ────────────────────────────────────────────────────────────────────────────
  # Runner lifecycle
  # ────────────────────────────────────────────────────────────────────────────

  defp ensure_ready(state, timeout_ms) do
    allow_start? = state.allow_lazy_start?
    ensure_serving(state, timeout_ms, allow_start?: allow_start?)
  end

  defp ensure_serving(state, _timeout_ms, allow_start?: false) do
    cond do
      state.status == :ready and reachable?(state, 1_000) ->
        {:ok, state}

      true ->
        {:error, state, :lazy_start_disabled}
    end
  end

  defp ensure_serving(state, _timeout_ms, allow_start?: true) do
    cond do
      state.status == :ready and reachable?(state, 1_000) ->
        {:ok, state}

      true ->
        do_spawn(%{state | status: :starting})
    end
  end

  defp do_spawn(state) do
    with :ok <- validate_model_path(state.model_path),
         {:ok, port_int, state} <- ensure_port_bound(state),
         {:ok, port} <- spawn_llama_server(state, port_int),
         endpoint <- "http://#{state.host}:#{port_int}",
         state <- %{state | llama_port: port, served_by_us?: true, endpoint: endpoint, status: :starting},
         {:ok, state} <- wait_ready(state) do
      {:ok, %{state | status: :ready}}
    else
      {:error, reason} ->
        {:error, drop_runner_state(state, :failed), reason}
    end
  end

  defp validate_model_path(nil), do: {:error, :missing_model_path}

  defp validate_model_path(path) when is_binary(path) do
    if File.exists?(path), do: :ok, else: {:error, {:model_not_found, path}}
  end

  defp ensure_port_bound(%{port: p} = state) when is_integer(p) and p > 0, do: {:ok, p, state}

  defp ensure_port_bound(state) do
    case free_local_port() do
      {:ok, p} -> {:ok, p, %{state | port: p}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp resolve_executable!(exe) when is_binary(exe) do
    cond do
      String.contains?(exe, "/") ->
        exe

      true ->
        System.find_executable(exe) ||
          raise "could not find executable on PATH: #{exe}"
    end
  end

  defp spawn_llama_server(state, port_int) do
    exe = resolve_executable!(state.llama_server)

    args =
      [
        "-m",
        state.model_path,
        "-c",
        Integer.to_string(state.ctx),
        "-t",
        Integer.to_string(state.threads),
        "--host",
        state.host,
        "--port",
        Integer.to_string(port_int)
      ]

    Logger.debug("Llm spawning: #{exe} #{Enum.join(args, " ")}")

port =
  Port.open(
    {:spawn_executable, to_charlist(exe)},
    [
      :binary,
      :exit_status,
      :use_stdio,
      :eof,
      :stderr_to_stdout,
      {:line, 16_384},
      args: Enum.map(args, &to_charlist/1)
    ]
  )

    {:ok, port}
  rescue
    e -> {:error, {:spawn_failed, e}}
  end

defp wait_ready(state) do
  wait_ready_loop(state, @ready_poll_attempts)
end

defp wait_ready_loop(_state, 0), do: {:error, :not_ready_timeout}

defp wait_ready_loop(state, n) when n > 0 do
  if reachable?(state, 1_250) do
    {:ok, state}
  else
    Process.sleep(@ready_poll_sleep_ms)
    wait_ready_loop(state, n - 1)
  end
end

  defp reachable?(%{endpoint: nil}, _timeout), do: false

  defp reachable?(state, timeout_ms) do
    case http_get(state, "/v1/models", timeout: timeout_ms) do
      {:ok, _} -> true
      _ -> false
    end
  end

  defp drop_runner_state(state, status) do
    cancel_restart_timer(state)
    %{state | llama_port: nil, endpoint: nil, status: status}
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Restart scheduling + backoff
  # ────────────────────────────────────────────────────────────────────────────

  defp schedule_restart(state, reason) do
    if is_reference(state.restart_timer_ref) do
      state
    else
      ms = state.restart_backoff_ms
      Logger.debug("Llm scheduling restart in #{ms}ms (#{inspect(reason)})")
      ref = Process.send_after(self(), :restart_runner, ms)
      bump_backoff(%{state | restart_timer_ref: ref})
    end
  end

  defp cancel_restart_timer(%{restart_timer_ref: nil}), do: :ok

  defp cancel_restart_timer(%{restart_timer_ref: ref}) when is_reference(ref) do
    Process.cancel_timer(ref, info: false)
    :ok
  end

  defp reset_backoff(state), do: %{state | restart_backoff_ms: @backoff_min, restart_timer_ref: nil}

  defp bump_backoff(state) do
    next = min(max(state.restart_backoff_ms * 2, @backoff_min), @backoff_max)
    %{state | restart_backoff_ms: next}
  end

  # ────────────────────────────────────────────────────────────────────────────
  # HTTP (Finch + Jason)
  # ────────────────────────────────────────────────────────────────────────────

  defp http_get(state, path, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, state.timeout)
    url = state.endpoint <> path

    req = Finch.build(:get, url, [{"accept", "application/json"}])

    case Finch.request(req, state.finch, receive_timeout: timeout) do
      {:ok, %{status: s, body: body}} when s in 200..299 ->
        decode_json(body)

      {:ok, %{status: s, body: body}} ->
        {:error, {:http_status, s, safe_body(body)}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp http_post(state, path, body_map, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, state.timeout)
    url = state.endpoint <> path

    json = Jason.encode!(body_map)

    req =
      Finch.build(:post, url, [{"content-type", "application/json"}, {"accept", "application/json"}], json)

    case Finch.request(req, state.finch, receive_timeout: timeout) do
      {:ok, %{status: s, body: body}} when s in 200..299 ->
        decode_json(body)

      {:ok, %{status: s, body: body}} ->
        {:error, {:http_status, s, safe_body(body)}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp decode_json(body) when is_binary(body) do
    case Jason.decode(body) do
      {:ok, map} -> {:ok, map}
      {:error, e} -> {:error, {:json_decode_failed, e, safe_body(body)}}
    end
  end

  defp safe_body(body) when is_binary(body) do
    if byte_size(body) > 2_000, do: binary_part(body, 0, 2_000) <> "…", else: body
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Response extraction
  # ────────────────────────────────────────────────────────────────────────────

  defp normalize_messages(prompt) when is_binary(prompt) do
    [%{"role" => "user", "content" => prompt}]
  end

  defp normalize_messages(list) when is_list(list) do
    Enum.map(list, fn
      %{role: r, content: c} -> %{"role" => r, "content" => c}
      %{"role" => r, "content" => c} -> %{"role" => r, "content" => c}
      other -> %{"role" => "user", "content" => inspect(other)}
    end)
  end

  defp extract_chat_content(%{"choices" => [%{"message" => %{"content" => c}} | _]}) when is_binary(c), do: c
  defp extract_chat_content(%{"choices" => [%{"text" => c} | _]}) when is_binary(c), do: c
  defp extract_chat_content(_), do: ""

  defp extract_embedding_one(%{"data" => [%{"embedding" => emb} | _]}) when is_list(emb), do: emb
  defp extract_embedding_one(%{"embedding" => emb}) when is_list(emb), do: emb
  defp extract_embedding_one(_), do: []

  defp extract_embedding_batch(%{"data" => data}) when is_list(data), do: data
  defp extract_embedding_batch(_), do: []

  # ────────────────────────────────────────────────────────────────────────────
  # Logging ring buffer
  # ────────────────────────────────────────────────────────────────────────────

  defp push_log(state, bin) when is_binary(bin) do
    line = bin |> String.slice(0, 4_000)

    q2 = :queue.in(line, state.log_ring)
    size2 = state.log_ring_size + 1

    {q3, size3} =
      if size2 > @log_ring_max do
        {{:value, _old}, qx} = :queue.out(q2)
        {qx, size2 - 1}
      else
        {q2, size2}
      end

    %{state | log_ring: q3, log_ring_size: size3}
  end

  defp logs_to_list(state), do: :queue.to_list(state.log_ring)

  # ────────────────────────────────────────────────────────────────────────────
  # Heartbeat
  # ────────────────────────────────────────────────────────────────────────────

  defp schedule_heartbeat(%{heartbeat_ms: ms} = state) do
    cancel_heartbeat(state)

    if is_integer(ms) and ms > 0 do
      ref = Process.send_after(self(), :heartbeat, ms)
      %{state | heartbeat_ref: ref}
    else
      %{state | heartbeat_ref: nil}
    end
  end

  defp cancel_heartbeat(%{heartbeat_ref: nil}), do: :ok

  defp cancel_heartbeat(%{heartbeat_ref: ref}) when is_reference(ref) do
    Process.cancel_timer(ref, info: false)
    :ok
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Misc
  # ────────────────────────────────────────────────────────────────────────────

  defp server(opts), do: Keyword.get(opts, :name, __MODULE__)
  defp call_timeout(opts), do: Keyword.get(opts, :timeout, @call_timeout_default)

  defp apply_overrides(state, opts) do
    state
    |> maybe_put(:model_path, Keyword.get(opts, :model_path))
    |> maybe_put(:llama_server, Keyword.get(opts, :llama_server))
    |> maybe_put(:host, Keyword.get(opts, :host))
    |> maybe_put(:port, Keyword.get(opts, :port))
    |> maybe_put(:ctx, Keyword.get(opts, :ctx))
    |> maybe_put(:threads, Keyword.get(opts, :threads))
    |> maybe_put(:temperature, Keyword.get(opts, :temperature))
    |> maybe_put(:timeout, Keyword.get(opts, :timeout))
  end

  defp maybe_put(state, _k, nil), do: state
  defp maybe_put(state, k, v), do: Map.put(state, k, v)

  defp free_local_port() do
    case :gen_tcp.listen(0, [:binary, active: false, ip: {127, 0, 0, 1}]) do
      {:ok, sock} ->
        {:ok, {_ip, port}} = :inet.sockname(sock)
        :gen_tcp.close(sock)
        {:ok, port}

      {:error, reason} ->
        {:error, reason}
    end
  end

@impl true
def stop(_state) do
  :ok
end 

end
