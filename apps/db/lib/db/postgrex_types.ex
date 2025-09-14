# Safe define: include pgvector’s codec + Ecto’s default codecs, use Jason for JSON.
# If pgvector isn’t compiled/installed yet, we fall back gracefully.

if Code.ensure_loaded?(Pgvector.Extensions.Vector) do
  Postgrex.Types.define(
    Db.PostgrexTypes,
    [Pgvector.Extensions.Vector] ++ Ecto.Adapters.Postgres.extensions(),
    json: Jason
  )
else
  Postgrex.Types.define(
    Db.PostgrexTypes,
    Ecto.Adapters.Postgres.extensions(),
    json: Jason
  )
end

