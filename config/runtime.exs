# config/runtime.exs
import Config

# Small-model web retrieval: keyless Wikimedia by default, optional SearXNG.
# Environment toggles never require changing code.
web_cfg = Application.get_env(:core, :web_retrieval, [])
web_enabled =
  case System.get_env("SYMBRELLA_WEB_RETRIEVAL") do
    nil -> Keyword.get(web_cfg, :enabled?, false)
    value -> String.downcase(value) in ["1", "true", "yes", "on"]
  end

web_provider =
  case System.get_env("SYMBRELLA_WEB_PROVIDER", "wikipedia") |> String.downcase() do
    "searxng" -> Core.Tools.WebRetrieval.Searxng
    _ -> Core.Tools.WebRetrieval.Wikipedia
  end

config :core, :web_retrieval,
  Keyword.merge(web_cfg, enabled?: web_enabled, provider: web_provider)

config :core, :searxng_url, System.get_env("SYMBRELLA_SEARXNG_URL")

# ───────── Brain env overrides (apply in all envs) ─────────

pmtg_mode =
  case System.get_env("PMTG_MODE", "boost") |> String.downcase() do
    "rerun" -> :rerun
    "none" -> :none
    _ -> :boost
  end

pmtg_margin =
  case Float.parse(System.get_env("PMTG_MARGIN_THRESHOLD", "0.15")) do
    {f, _} -> f
    _ -> 0.15
  end

pmtg_keep =
  case Integer.parse(System.get_env("PMTG_WINDOW_KEEP", "50")) do
    {i, _} -> i
    _ -> 50
  end

# Optional Stage-1 weight override.
#
# Preferred five-feature form:
#
#   LIFG_WTS=lex,context,rel,act,intent
#
# Example:
#
#   LIFG_WTS=0.20,0.40,0.20,0.10,0.10
#
# Legacy four-feature form remains accepted:
#
#   LIFG_WTS=lex,rel,act,intent
#
# In the legacy form context_fit remains disabled at 0.0.
lifg_weights =
  case System.get_env("LIFG_WTS") do
    nil ->
      nil

    csv ->
      parts =
        csv
        |> String.split(",")
        |> Enum.map(&String.trim/1)

      case Enum.map(parts, &Float.parse/1) do
        [
          {lex, ""},
          {context, ""},
          {rel, ""},
          {act, ""},
          {intent, ""}
        ] ->
          %{
            lex_fit: lex,
            context_fit: context,
            rel_prior: rel,
            activation: act,
            intent_bias: intent
          }

        [
          {lex, ""},
          {rel, ""},
          {act, ""},
          {intent, ""}
        ] ->
          %{
            lex_fit: lex,
            context_fit: 0.0,
            rel_prior: rel,
            activation: act,
            intent_bias: intent
          }

        _ ->
          nil
      end
  end

# Optional: override scores output mode ("all" | "top2" | "none")
lifg_scores_mode =
  case System.get_env("LIFG_SCORES_MODE", "") |> String.downcase() do
    "all" -> :all
    "top2" -> :top2
    "none" -> :none
    _ -> nil
  end

# Optional: gate threshold
lifg_min_score =
  case Float.parse(System.get_env("LIFG_MIN_SCORE", "0.35")) do
    {f, _} -> f
    _ -> 0.35
  end

log_cognition_pipeline? =
  case System.get_env("LOG_COGNITION_PIPELINE", "false") |> String.downcase() do
    value when value in ["1", "true", "yes", "on"] -> true
    _ -> false
  end

log_lifg_stage1? =
  case System.get_env("LOG_LIFG_STAGE1", "false") |> String.downcase() do
    value when value in ["1", "true", "yes", "on"] -> true
    _ -> false
  end

cognition_idle_status_ms =
  case Integer.parse(System.get_env("COGNITION_IDLE_STATUS_MS", "0")) do
    {n, _} when n >= 0 -> n
    _ -> 0
  end

cognition_idle_threshold_ms =
  case Integer.parse(System.get_env("COGNITION_IDLE_THRESHOLD_MS", "")) do
    {n, _} when n >= 0 -> n
    _ -> nil
  end

drive_loop_runtime_config =
  [idle_status_interval_ms: cognition_idle_status_ms]
  |> then(fn cfg ->
    if is_integer(cognition_idle_threshold_ms) do
      Keyword.put(cfg, :idle_threshold_ms, cognition_idle_threshold_ms)
    else
      cfg
    end
  end)

# ───────── Episodic runtime configuration ─────────

# Episodic attach master switch.
#
# EPISODES_MODE:
#   off
#   on
#   sync
#   async
#   async_embedding
episodes_mode =
  case System.get_env("EPISODES_MODE", "async_embedding") |> String.downcase() do
    "off" -> :off
    "on" -> :on
    "sync" -> :on
    "async" -> :async
    "async_embedding" -> :async_embedding
    _ -> :async_embedding
  end

# Explicit persistence switch.
#
# Defaults to persistence being enabled whenever episodic mode itself
# is enabled.
episodes_persist =
  case System.get_env("EPISODES_PERSIST") do
    nil ->
      episodes_mode != :off

    value ->
      case String.downcase(value) do
        "1" -> true
        "true" -> true
        "yes" -> true
        "on" -> true
        _ -> false
      end
  end

episodes_tags =
  System.get_env("EPISODES_TAGS", "auto,lifg")
  |> String.split(",", trim: true)
  |> Enum.map(&String.trim/1)
  |> Enum.reject(&(&1 == ""))

# ───────── Brain runtime configuration ─────────
#
# Keep the common Brain configuration in one place. More specialized
# configuration, such as hippocampal DB recall defaults, lives below.

config :brain,
  self_names: ["symbrella"],
  pmtg_mode: pmtg_mode,
  pmtg_margin_threshold: pmtg_margin,
  pmtg_window_keep: pmtg_keep,
  hippo_meta_dup_count: true,
  lifg_min_score: lifg_min_score,

  # Episodes
  episodes_mode: episodes_mode,
  episodes_persist: episodes_persist,
  episodes_tags: episodes_tags,
  log_cognition_pipeline?: log_cognition_pipeline?,
  log_lifg_stage1?: log_lifg_stage1?,

  # LIFG
  lifg_stage1_mwe_fallback: true

config :brain, Brain.DriveLoop, drive_loop_runtime_config

# Apply optional Stage-1 runtime overrides only when explicitly supplied.
if lifg_weights do
  config :brain, :lifg_stage1_weights, lifg_weights
end

if lifg_scores_mode do
  config :brain, :lifg_stage1_scores_mode, lifg_scores_mode
end

# ───────── Hippocampal DB / hybrid recall ─────────

hippo_recall_source =
  case System.get_env("HIPPO_RECALL_SOURCE", "") |> String.downcase() do
    "db" -> :db
    "hybrid" -> :hybrid
    "memory" -> :memory
    _ -> :memory
  end

hippo_recall_k =
  case Integer.parse(System.get_env("HIPPO_RECALL_K", "")) do
    {n, _} when n > 0 -> n
    _ -> 8
  end

hippo_recall_min_sim =
  case Float.parse(System.get_env("HIPPO_RECALL_MIN_SIM", "")) do
    {f, _} when f >= 0.0 -> f
    _ -> 0.35
  end

hippo_recall_half_life_s =
  case Integer.parse(System.get_env("HIPPO_RECALL_HALF_LIFE_S", "")) do
    {n, _} when n > 0 -> n
    _ -> 3600
  end

config :brain, :hippo_db_defaults,
  recall_source: hippo_recall_source,
  recall_k: hippo_recall_k,
  recall_min_sim: hippo_recall_min_sim,
  recall_half_life_s: hippo_recall_half_life_s

# Opt-in complete raw state snapshots. These may contain private conversation
# text, memory data, session identifiers and every nested SemanticInput field.
# SYMBRELLA_STATE_DUMP=final records end-to-end outputs; =all records every stage.
state_dump_mode =
  case System.get_env("SYMBRELLA_STATE_DUMP", "off") |> String.downcase() do
    "all" -> :all
    "final" -> :final
    _ -> :off
  end

config :core, Core.StateDump,
  mode: state_dump_mode,
  path: System.get_env("SYMBRELLA_STATE_DUMP_PATH", "tmp/state_dumps/symbrella_raw.log")

# Recording is controlled by config/config.exs; runtime must not override it
# with a CLI environment toggle. Development chat completions append locally
# without a special launch command. Production and tests stay disabled.
training_archive_config = Application.get_env(:core, Core.TrainingArchive, [])
training_archive_enabled? = Keyword.get(training_archive_config, :enabled?, false) == true
training_archive_dir = Keyword.get(training_archive_config, :dir, "tmp/training")

# Experimental read-only inference from an existing residual Axon checkpoint.
# OFF by default, requires the training archive, never changes cognition.
neural_shadow_enabled? =
  case System.get_env("SYMBRELLA_NEURAL_SHADOW", "off") |> String.downcase() do
    value when value in ["1", "true", "yes", "on"] -> true
    _ -> false
  end

neural_shadow_dir = training_archive_dir

config :symbrella_web, SymbrellaWeb.NeuralShadow,
  enabled?: training_archive_enabled? and neural_shadow_enabled?,
  dir: neural_shadow_dir,
  checkpoint:
    System.get_env(
      "SYMBRELLA_NEURAL_SHADOW_CHECKPOINT",
      Path.join(neural_shadow_dir, "state_transition_residual_v4_pilot.etf")
    )


config :brain, Brain.SelfCalibration.Logger,
  enabled?: true,
  path: "priv/self_calibration/samples.jsonl"

# ───────── Logger runtime overrides ─────────

log_level =
  case System.get_env("LOG_LEVEL", "info") |> String.downcase() do
    "debug" -> :debug
    "warn" -> :warn
    "error" -> :error
    _ -> :info
  end

if config_env() != :test do
  config :logger, level: log_level
end

# ───────── Ecto Repo runtime overrides ─────────

# Silence SQL query spam unless DB_LOG=true.
db_log =
  case System.get_env("DB_LOG", "false") |> String.downcase() do
    value when value in ["1", "true", "yes"] -> true
    _ -> false
  end

repo_overrides =
  []
  |> then(fn acc ->
    case System.get_env("DATABASE_URL") do
      nil ->
        acc

      url ->
        Keyword.put(acc, :url, url)
    end
  end)
  |> then(fn acc ->
    case Integer.parse(System.get_env("POOL_SIZE", "")) do
      {n, _} ->
        Keyword.put(acc, :pool_size, n)

      _ ->
        acc
    end
  end)
  |> Keyword.put(:log, db_log)

if repo_overrides != [] do
  config :db, Db, repo_overrides
end

# ───────── Web interface access control ─────────
# Local development stays accessible until credentials are explicitly set.
# Production always requires an operator-supplied account.
web_user = System.get_env("SYMBRELLA_WEB_AUTH_USERNAME")
web_password = System.get_env("SYMBRELLA_WEB_AUTH_PASSWORD")

web_auth =
  cond do
    is_binary(web_user) and byte_size(web_user) > 0 and is_binary(web_password) and
        byte_size(web_password) >= 16 ->
      [enabled?: true, username: web_user, password: web_password]

    config_env() != :prod and is_nil(web_user) and is_nil(web_password) ->
      [enabled?: false]

    true ->
      raise """
      Symbrella web authentication requires SYMBRELLA_WEB_AUTH_USERNAME and
      SYMBRELLA_WEB_AUTH_PASSWORD (at least 16 characters). Both must be set.
      Production may not start without authentication.
      """
  end

config :symbrella_web, :web_auth, web_auth

# ───────── Phoenix / prod only ─────────

if config_env() == :prod do
  database_url =
    System.get_env("DATABASE_URL") ||
      raise "Production requires DATABASE_URL; refusing development DB defaults"

  config :db, Db, url: database_url

  public_host =
    System.get_env("SYMBRELLA_HOST") ||
      raise "Production requires SYMBRELLA_HOST"

  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      Generate one with: mix phx.gen.secret
      """

  port = String.to_integer(System.get_env("PORT") || "4000")

  config :symbrella_web, SymbrellaWeb.Endpoint,
    url: [host: public_host, scheme: "https", port: 443],
    check_origin: ["https://" <> public_host],
    force_ssl: [rewrite_on: [:x_forwarded_proto]],
    server: true,
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: port
    ],
    secret_key_base: secret_key_base

  config :swoosh, :api_client, Swoosh.ApiClient.Req
  config :swoosh, local: false
end

# ───────── LLM startup ─────────

if config_env() != :test do
  config :llm, Llm,
    # Synchronous startup is owned by Llm.BootGate below.
    # Leave Llm's own async handle_continue autostart disabled
    # to avoid a duplicate boot path.
    auto_start_on_boot?: false

  config :llm, Llm.BootGate,
    # Degraded mode does not block the web and symbolic brain while a model loads.
    # Set SYMBRELLA_LLM_BOOT_STRICT=1 to preserve fail-fast startup.
    enabled?: true,
    mode:
      if(System.get_env("SYMBRELLA_LLM_BOOT_STRICT", "false") in ["1", "true", "yes"],
        do: :strict,
        else: :degraded
      ),
    timeout: 600_000
end
