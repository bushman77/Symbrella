defmodule Db.Migrations.DropEpisodeSiPayload do
  use Ecto.Migration

  def change do
    alter table(:episodes) do
      remove(:si, :jsonb, null: false, default: fragment("'{}'::jsonb"))
    end
  end
end
