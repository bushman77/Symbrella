defmodule Db.Migrations.IndexUserSpatialObservations do
  use Ecto.Migration

  @moduledoc """
  Targeted index for Hippocampus user-reported spatial events. Preserves
  owner separation and cross-session lookup without scanning unrelated rows.
  """

  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    execute("""
    CREATE INDEX CONCURRENTLY IF NOT EXISTS episodes_user_spatial_lookup_idx
      ON episodes ((meta ->> 'owner'), (meta ->> 'object_key'), inserted_at DESC, id DESC)
      WHERE (meta ->> 'kind') = 'user_reported_spatial'
        AND (meta ->> 'subject') = 'user'
    """)
  end

  def down do
    execute("DROP INDEX CONCURRENTLY IF EXISTS episodes_user_spatial_lookup_idx")
  end
end
