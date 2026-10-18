defmodule Db.Migrations.AddNumericIdToBrainCells do
  use Ecto.Migration

  @moduledoc """
  Add an Nx-compatible, signed 32-bit address for each existing BrainCell.

  The human-readable `brain_cells.id` stays the primary key and is never
  rewritten. PostgreSQL assigns distinct integer identity values to existing
  rows and generates the value for future inserts.

  IMPORTANT: Adding an identity column to a populated table and building its
  unique index take a table lock. Run during a maintenance window with a
  database backup; do not run while serving active traffic.
  """

  def up do
    execute("""
    ALTER TABLE brain_cells
      ADD COLUMN numeric_id integer GENERATED ALWAYS AS IDENTITY
    """)

    # IDENTITY guarantees generation and NOT NULL, but not uniqueness.
    # Keep the string primary key; enforce distinct numeric addresses here.
    create unique_index(:brain_cells, [:numeric_id],
             name: :brain_cells_numeric_id_index)
  end

  def down do
    drop index(:brain_cells, [:numeric_id],
           name: :brain_cells_numeric_id_index)

    execute("ALTER TABLE brain_cells DROP COLUMN numeric_id")
  end
end
