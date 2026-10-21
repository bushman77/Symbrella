import Config

# Read the model path from the current user's ~/.env without requiring the
# shell to source it. An exported variable takes precedence over the file.
# Only LLAMA_MODEL_PATH is read; the rest of ~/.env is left untouched.
home = System.user_home!()
home_env_model_path =
  case File.read(Path.join(home, ".env")) do
    {:ok, contents} ->
      contents
      |> String.split(~r/\r?\n/)
      |> Enum.reduce(nil, fn line, previous ->
        case Regex.run(
               ~r/^\s*(?:export\s+)?LLAMA_MODEL_PATH\s*=\s*(.*?)\s*$/,
               line,
               capture: :all_but_first
             ) do
          [value] -> value |> String.trim() |> String.trim("\"'")
          _ -> previous
        end
      end)

    {:error, _reason} ->
      nil
  end

model_path =
  [System.get_env("LLAMA_MODEL_PATH"), home_env_model_path]
  |> Enum.find(&(is_binary(&1) and String.trim(&1) != ""))
  |> case do
    nil -> nil
    path -> Path.expand(path, home)
  end

config :llm, Llm,
  model_path: model_path,
  chat_format: :alpaca,
  n_gpu_layers: 18,
  temperature: 0.75,
  llama_server: "llama-server",
  auto_start_on_boot?: true,
  allow_lazy_start?: true,
  auto_restart_on_crash?: true,
  host: "127.0.0.1",
  port: 8080,
  ctx: 4096,
  threads: 6,
  heartbeat_ms: 15_000

config :llm, Llm.BootGate,
  enabled?: true,
  timeout: 120_000

config :llm, :runner,
  host: "127.0.0.1",
  ctx: 4096,
  threads: 6,
  temperature: 0.75,
  call_timeout_ms: 60_000,
  heartbeat_ms: 15_000,
  ready_poll_attempts: 80,
  ready_poll_sleep_ms: 250,
  models_timeout_cap_ms: 8_000,
  ready_probe_timeout_ms: 1_250,
  log_ring_max: 200,
  backoff_min_ms: 250,
  backoff_max_ms: 10_000,
  line_buffer: 16_384,
  body_preview_chars: 2_000,
  log_line_chars: 4_000

config :llm, :generation,
  chat_model: "local",
  embedding_model: "local",
  stream?: false,
  temperature: 0.75,
  keep_alive: "10m",
  stable_runner_opts: %{
    num_ctx: 4096,
    top_k: 40,
    top_p: 0.9,
    repeat_penalty: 1.1,
    seed: 42,
    num_predict: 80
  }
