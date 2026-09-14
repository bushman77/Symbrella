# config/runtime.exs
import Config

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

  # LIFG
  lifg_stage1_mwe_fallback: true

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

# ───────── Phoenix / prod only ─────────

if config_env() == :prod do
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      Generate one with: mix phx.gen.secret
      """

  port = String.to_integer(System.get_env("PORT") || "4000")

  config :symbrella_web, SymbrellaWeb.Endpoint,
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
    # Blocks application startup here; Phoenix starts only after
    # :symbrella finishes the LLM boot sequence.
    enabled?: true,
    timeout: 600_000
end
