defmodule Db.Migrations.CreateConsolidatedConclusions do
  use Ecto.Migration

  def change do
    create table(:consolidated_conclusions) do
      add :fingerprint, :string, null: false
      add :pair_key, :string, null: false
      add :kind, :string, null: false
      add :reference_year, :integer, null: false
      add :premises, {:array, :map}, null: false, default: []
      add :derivation, :map, null: false, default: %{}
      add :source_urls, {:array, :string}, null: false, default: []
      add :provenance, :string, null: false

      timestamps(type: :utc_datetime_usec)
    end

    create unique_index(:consolidated_conclusions, [:fingerprint])
    create index(:consolidated_conclusions, [:pair_key, :reference_year])
  end
end
