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

# Optional: override Stage-1 weights via "LIFG_WTS=lex,rel,act,prag"
lifg_weights =
  case System.get_env("LIFG_WTS") do
    nil ->
      nil

    csv ->
      parts = String.split(csv, ",")

      case Enum.map(parts, &Float.parse/1) do
        [{lex, _}, {rel, _}, {act, _}, {prag, _}] ->
          %{lex_fit: lex, rel_prior: rel, activation: act, intent_bias: prag}

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

# NEW: episodic attach master switch (EPISODES_MODE=on|off; default on)
episodes_mode =
  case System.get_env("EPISODES_MODE", "on") |> String.downcase() do
    "off" -> :off
    _ -> :on
  end

# NEW: optional defaults for DB/Hybrid recall (all overridable per request)
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

config :brain,
  pmtg_mode: pmtg_mode,
  pmtg_margin_threshold: pmtg_margin,
  pmtg_window_keep: pmtg_keep,
  hippo_meta_dup_count: true,
  lifg_min_score: lifg_min_score,
  # NEW:
  episodes_mode: episodes_mode,
  lifg_stage1_mwe_fallback: true

if lifg_weights, do: config(:brain, :lifg_stage1_weights, lifg_weights)
if lifg_scores_mode, do: config(:brain, :lifg_stage1_scores_mode, lifg_scores_mode)

# NEW: defaults for DB/Hybrid episodic recall
config :brain, :hippo_db_defaults,
  recall_source: hippo_recall_source,      # :memory | :db | :hybrid
  recall_k: hippo_recall_k,
  recall_min_sim: hippo_recall_min_sim,
  recall_half_life_s: hippo_recall_half_life_s

# ───────── Logger runtime overrides ─────────
log_level =
  case System.get_env("LOG_LEVEL", "info") |> String.downcase() do
    "debug" -> :debug
    "warn" -> :warn
    "error" -> :error
    _ -> :info
  end

config :logger, level: log_level

# ───────── Ecto Repo runtime overrides ─────────
# Silence SQL query spam unless DB_LOG=true
db_log =
  case System.get_env("DB_LOG", "false") |> String.downcase() do
    s when s in ["1", "true", "yes"] -> true
    _ -> false
  end

repo_overrides =
  []
  |> then(fn acc ->
    case System.get_env("DATABASE_URL") do
      nil -> acc
      url -> Keyword.put(acc, :url, url)
    end
  end)
  |> then(fn acc ->
    case Integer.parse(System.get_env("POOL_SIZE", "")) do
      {n, _} -> Keyword.put(acc, :pool_size, n)
      _ -> acc
    end
  end)
  |> Keyword.put(:log, db_log)

if repo_overrides != [], do: config(:db, Db, repo_overrides)

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
    http: [ip: {0, 0, 0, 0, 0, 0, 0, 0}, port: port],
    secret_key_base: secret_key_base

  config :swoosh, :api_client, Swoosh.ApiClient.Req
  config :swoosh, local: false
end

config :llm, Llm,
  auto_start_on_boot?: true,   # keep daemon up
  pull_on_boot?: true,         # optional: prefetch model
  warm_on_boot?: false,        # <- critical: DO NOT warm on boot
  warm_on_restart?: false,
  pull_on_restart?: false

