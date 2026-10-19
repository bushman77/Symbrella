defmodule Db.Migrations.IndexUserEpisodicEvents do
  use Ecto.Migration

  @moduledoc """
  Owner-scoped exact episodic relation lookup and per-entity chronology.
  """

  @disable_ddl_transaction true
  @disable_migration_lock true

  def up do
    execute("""
    CREATE INDEX CONCURRENTLY IF NOT EXISTS episodes_user_event_lookup_idx
      ON episodes ((meta ->> 'owner'), (meta ->> 'entity_key'),
                   (meta ->> 'relation'), inserted_at DESC, id DESC)
      WHERE (meta ->> 'kind') = 'user_reported_event'
        AND (meta ->> 'subject') = 'user'
    """)
  end

  def down do
    execute("DROP INDEX CONCURRENTLY IF EXISTS episodes_user_event_lookup_idx")
  end
end
