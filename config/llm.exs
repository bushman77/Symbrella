import Config

config :llm, Llm,
  model_path: Path.expand("~/models/Qwen2.5-0.5B-Instruct-Q4_K_M.gguf"),
  llama_server: "llama-server",
  auto_start_on_boot?: true,
  allow_lazy_start?: true,
  auto_restart_on_crash?: true,
  host: "127.0.0.1",
  port: 0,
  ctx: 2048,
  threads: 4,
  heartbeat_ms: 15_000

config :llm, Llm.BootGate,
  enabled?: true,
  timeout: 120_000

config :llm, :runner,
  host: "127.0.0.1",
  ctx: 2048,
  threads: 4,
  temperature: 0.4,
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
  temperature: 0.4,
  keep_alive: "10m",
  stable_runner_opts: %{
    num_ctx: 1024,
    top_k: 1,
    top_p: 1.0,
    repeat_penalty: 1.0,
    seed: 42,
    num_predict: 80
  }
