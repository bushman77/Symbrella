import Config

# --- Brain env overrides (apply in all envs) ---------------------------------
pmtg_mode =
  case System.get_env("PMTG_MODE", "boost") do
    "rerun" -> :rerun
    "none"  -> :none
    _       -> :boost
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
    nil -> nil
    csv ->
      case String.split(csv, ",") |> Enum.map(&Float.parse/1) do
        [{lex,_},{rel,_},{act,_},{prag,_}] ->
          %{lex_fit: lex, rel_prior: rel, activation: act, intent_bias: prag}
        _ -> nil
      end
  end

# Optional: override scores output mode ("all" | "top2" | "none")
lifg_scores_mode =
  case String.downcase(System.get_env("LIFG_SCORES_MODE", "")) do
    "all"  -> :all
    "top2" -> :top2
    "none" -> :none
    _ -> nil
  end

config :brain,
  pmtg_mode: pmtg_mode,
  pmtg_margin_threshold: pmtg_margin,
  pmtg_window_keep: pmtg_keep,
  hippo_meta_dup_count: true

if lifg_weights, do: config(:brain, :lifg_stage1_weights, lifg_weights)
if lifg_scores_mode, do: config(:brain, :lifg_stage1_scores_mode, lifg_scores_mode)

# --- Phoenix / prod ----------------------------------------------------------
if config_env() == :prod do
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise """
      environment variable SECRET_KEY_BASE is missing.
      You can generate one by calling: mix phx.gen.secret
      """

  config :symbrella_web, SymbrellaWeb.Endpoint,
    http: [
      ip: {0, 0, 0, 0, 0, 0, 0, 0},
      port: String.to_integer(System.get_env("PORT") || "4000")
    ],
    secret_key_base: secret_key_base

  # Optional DNS cluster
  config :symbrella, :dns_cluster_query, System.get_env("DNS_CLUSTER_QUERY")
end
# config/runtime.exs
lifg_min_score = String.to_float(System.get_env("LIFG_MIN_SCORE") || "0.35")
config :brain, :lifg_min_score, lifg_min_score

