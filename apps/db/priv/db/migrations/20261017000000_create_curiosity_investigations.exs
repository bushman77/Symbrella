defmodule Db.Migrations.CreateCuriosityInvestigations do
  use Ecto.Migration

  def change do
    create table(:curiosity_investigations) do
      add :issue_id, references(:cognitive_issues, on_delete: :restrict), null: false
      add :worker, :string, null: false, default: "default"
      add :status, :string, null: false, default: "started"
      add :reason, :string
      add :acquisition, :string, null: false, default: "not_attempted"
      add :acquisition_reason, :string
      add :acquired_proof_id, :bigint
      add :parent_proof_ids, {:array, :bigint}, null: false, default: []
      add :origin, :string
      add :provider_attempts, {:array, :map}, null: false, default: []
      add :duration_ms, :integer
      add :completed_at, :utc_datetime_usec

      timestamps(type: :utc_datetime_usec)
    end

    create index(:curiosity_investigations, [:issue_id, :inserted_at, :id])
    create index(:curiosity_investigations, [:status, :inserted_at])
  end
end
