defmodule Db.Migrations.IndexDurableUserFacts do
  use Ecto.Migration

  @moduledoc """
  Partial index for exact user-fact lookup across the full episodes table.
  Only explicit user-owned fact rows participate in the index.
  """

  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    execute("""
    CREATE INDEX CONCURRENTLY IF NOT EXISTS episodes_user_fact_lookup_idx
      ON episodes ((meta ->> 'key'), inserted_at DESC, id DESC)
      WHERE (meta ->> 'kind') = 'fact'
        AND COALESCE(meta ->> 'subject', 'user') = 'user'
    """)
  end

  def down do
    execute("DROP INDEX CONCURRENTLY IF EXISTS episodes_user_fact_lookup_idx")
  end
end
