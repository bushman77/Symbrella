defmodule Db.Migrations.RequireEpisodeIntent do
  use Ecto.Migration

  def up do
    execute("""
    UPDATE episodes
    SET intent = 'unknown'
    WHERE intent IS NULL OR btrim(intent) = ''
    """)

    execute("ALTER TABLE episodes ALTER COLUMN intent SET DEFAULT 'unknown'")
    execute("ALTER TABLE episodes ALTER COLUMN intent SET NOT NULL")
  end

  def down do
    execute("ALTER TABLE episodes ALTER COLUMN intent DROP NOT NULL")
    execute("ALTER TABLE episodes ALTER COLUMN intent DROP DEFAULT")
  end
end
