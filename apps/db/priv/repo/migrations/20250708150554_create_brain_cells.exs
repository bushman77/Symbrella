defmodule Db.Migrations.CreateBrainCells do
  use Ecto.Migration

  # Run in an earlier migration:
  # def change, do: execute("CREATE EXTENSION IF NOT EXISTS vector")

  def change do
    create table(:brain_cells, primary_key: false) do
      add :id, :text, primary_key: true

      add :word, :text, null: false
      add :pos,  :text, null: false
      add :definition, :text
      add :example, :text
      add :gram_function, :text

      add :synonyms,       {:array, :text}, default: []
      add :antonyms,       {:array, :text}, default: []
      add :semantic_atoms, {:array, :text}, default: []

      add :type,   :string
      add :status, :string, null: false, default: "inactive"

      add :activation,           :float, null: false, default: 0.0
      add :modulated_activation, :float, null: false, default: 0.0
      add :dopamine,             :float, null: false, default: 0.0
      add :serotonin,            :float, null: false, default: 0.0

      add :position, {:array, :float} # float8[]

      # jsonb[] default must be expressed via fragment
      add :connections, {:array, :map},
        null: false,
        default: fragment("'{}'::jsonb[]")

      add :last_dose_at,   :utc_datetime_usec
      add :last_substance, :string
      add :token_id, :bigint

      # pgvector column (requires extension enabled earlier)
      add :embedding, :vector, size: 1536

      timestamps()
    end

    create index(:brain_cells, [:word])
    create index(:brain_cells, [:token_id])

    create constraint(
      :brain_cells,
      :brain_cells_status_check,
      check: "status IN ('inactive','active','dormant','decayed')"
    )
  end
end

