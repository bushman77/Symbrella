# apps/symbrella_web/config/dev.exs
import Config

config :symbrella_web, SymbrellaWeb.Endpoint,
  server: true,
  http: [ip: {0, 0, 0, 0}, port: 4000],
  url: [host: "localhost", port: 4000],
  code_reloader: true,
  debug_errors: true,
  check_origin: false,
  secret_key_base: "0123456789012345678901234567890123456789012345678901234567890123",
  # No watchers (removed)
  live_reload: [
    patterns: [
      ~r"priv/static/.*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"lib/symbrella_web/(controllers|live|components)/.*(ex|heex)$",
      ~r"priv/gettext/.*(po)$"
    ]
  ]

# Dev-only routes (LiveDashboard/mailbox)
config :symbrella_web, :dev_routes, true

# Dev ergonomics
config :logger, :default_formatter, format: "[$level] $message\n"
config :phoenix, :plug_init_mode, :runtime

config :phoenix_live_view,
  debug_heex_annotations: true,
  debug_attributes: true,
  enable_expensive_runtime_checks: true

# Disable Swoosh API client in dev
config :swoosh, :api_client, false

# Helpful stacktraces in dev
config :phoenix, :stacktrace_depth, 20

config :core, :tokenizer_defaults,
  mode: :words,
  emit_chargrams: false

config :brain, :lifg_weights, prime: 0.05

