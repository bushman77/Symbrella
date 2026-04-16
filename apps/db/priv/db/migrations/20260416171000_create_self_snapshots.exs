defmodule Db.Migrations.CreateSelfSnapshots do
  use Ecto.Migration

  def change do
    create table(:self_snapshots) do
      add(:scope, :string, null: false, default: "runtime")
      add(:self_model_v, :integer, null: false, default: 1)
      add(:snapshot_v, :integer, null: false, default: 1)
      add(:snapshot, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:source, :string, null: false, default: "runtime")

      timestamps(type: :naive_datetime_usec)
    end

    create(index(:self_snapshots, [:scope, :inserted_at]))
    create(index(:self_snapshots, [:snapshot_v]))
    create(index(:self_snapshots, [:self_model_v]))
  end
end
