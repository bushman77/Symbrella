defmodule Db.Migrations.CreateCognitiveIssues do
  use Ecto.Migration

  def change do
    create table(:cognitive_issues) do
      add :fingerprint, :string, null: false
      add :kind, :string, null: false
      add :status, :string, null: false, default: "open"
      add :subject_key, :string, null: false
      add :reference_year, :integer
      add :priority, :float, null: false, default: 0.5
      add :evidence, :map, null: false, default: %{}
      add :next_action, :string, null: false
      add :resolution, :map
      add :resolved_at, :utc_datetime_usec

      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:cognitive_issues, [:fingerprint])
    create index(:cognitive_issues, [:status, :priority, :inserted_at])
    create index(:cognitive_issues, [:subject_key, :reference_year])
  end
end
