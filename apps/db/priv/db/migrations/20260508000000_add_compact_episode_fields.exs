defmodule Db.Migrations.AddCompactEpisodeFields do
  use Ecto.Migration

  def change do
    alter table(:episodes) do
      add(:session_id, :text)
      add(:conversation_id, :text)
      add(:source, :text)
      add(:role, :text)
      add(:sentence, :text)
      add(:normalized_text, :text)
      add(:intent, :text)
      add(:confidence, :float)
      add(:winners, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:affect, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:uncertainty, :float)
      add(:meta, :jsonb, null: false, default: fragment("'{}'::jsonb"))
    end

    create(index(:episodes, [:session_id]))
    create(index(:episodes, [:conversation_id]))
    create(index(:episodes, [:source]))
    create(index(:episodes, [:role]))
    create(index(:episodes, [:intent]))
    create(index(:episodes, [:normalized_text]))
  end
end
