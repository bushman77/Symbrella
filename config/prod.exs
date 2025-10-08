import Config

# Static cache manifest (phx.digest)
config :symbrella_web, SymbrellaWeb.Endpoint,
  url: [host: "example.com", port: 80],
  cache_static_manifest: "priv/static/cache_manifest.json"

# Swoosh API Client in prod
config :swoosh, :api_client, Swoosh.ApiClient.Req
config :swoosh, local: false

# Keep prod logs at info
config :logger, level: :info

