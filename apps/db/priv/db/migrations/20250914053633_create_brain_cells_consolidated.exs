defmodule Db.Migrations.CreateBrainCellsConsolidated do
  use Ecto.Migration

  def up do
    execute("CREATE EXTENSION IF NOT EXISTS citext")
    execute("CREATE EXTENSION IF NOT EXISTS pg_trgm")

    create table(:brain_cells, primary_key: false) do
      add :id, :string, primary_key: true

      add :word, :citext, null: false
      add :norm, :citext, null: false
      add :pos,  :string

      add :definition, :text
      add :example, :text
      add :gram_function, :string

      add :synonyms, {:array, :text}, null: false, default: []
      add :antonyms, {:array, :text}, null: false, default: []
      add :semantic_atoms, {:array, :text}, null: false, default: []

      add :type, :string
      add :status, :string, null: false, default: "inactive"

      add :activation, :float, null: false, default: 0.0
      add :modulated_activation, :float, null: false, default: 0.0
      add :dopamine, :float, null: false, default: 0.0
      add :serotonin, :float, null: false, default: 0.0

      add :position, {:array, :float}
      add :connections, {:array, :map}, null: false, default: []
      add :last_dose_at, :utc_datetime_usec
      add :last_substance, :string
      add :token_id, :bigint

      timestamps()  # => :naive_datetime by default
    end

    # Speed indexes
    create index(:brain_cells, [:word], name: :brain_cells_word_index)
    create index(:brain_cells, [:norm], name: :brain_cells_norm_index)

    execute("""
    CREATE INDEX IF NOT EXISTS brain_cells_word_trgm_idx
    ON brain_cells USING GIN (word gin_trgm_ops)
    """)

    execute("""
    CREATE INDEX IF NOT EXISTS brain_cells_synonyms_gin_idx
    ON brain_cells USING GIN (synonyms)
    """)

    execute("""
    CREATE INDEX IF NOT EXISTS brain_cells_antonyms_gin_idx
    ON brain_cells USING GIN (antonyms)
    """)

    # ❌ Removed: unique “lexical key” index on (norm, pos, type, gram_function)

    # Backfill norm for any seeded rows
    execute("""
    UPDATE brain_cells
       SET norm = citext(
         regexp_replace(trim(lower(word::text)), '\\s+', ' ', 'g')
       );
    """)

    # Optional vector column + index (unchanged)
    execute("""
    DO $$
    BEGIN
      IF EXISTS (SELECT 1 FROM pg_available_extensions WHERE name = 'vector') THEN
        CREATE EXTENSION IF NOT EXISTS vector;

        IF NOT EXISTS (
          SELECT 1 FROM information_schema.columns
          WHERE table_name = 'brain_cells' AND column_name = 'embedding'
        ) THEN
          EXECUTE 'ALTER TABLE brain_cells ADD COLUMN embedding vector(768)';
        END IF;

        IF EXISTS (
          SELECT 1 FROM information_schema.columns
          WHERE table_name = 'brain_cells' AND column_name = 'embedding'
        ) THEN
          CREATE INDEX IF NOT EXISTS brain_cells_embedding_idx
          ON brain_cells USING ivfflat (embedding vector_l2_ops) WITH (lists = 100);
        END IF;
      END IF;
    END
    $$;
    """)
  end

  def down do
    execute("DROP INDEX IF EXISTS brain_cells_embedding_idx")
    execute("DROP INDEX IF EXISTS brain_cells_antonyms_gin_idx")
    execute("DROP INDEX IF EXISTS brain_cells_synonyms_gin_idx")
    execute("DROP INDEX IF EXISTS brain_cells_word_trgm_idx")
    execute("DROP INDEX IF EXISTS brain_cells_lexical_key_uniq") # harmless if it never existed

    drop_if_exists index(:brain_cells, [:norm], name: :brain_cells_norm_index)
    drop_if_exists index(:brain_cells, [:word], name: :brain_cells_word_index)

    drop table(:brain_cells)

    execute("DROP EXTENSION IF EXISTS vector")
    execute("DROP EXTENSION IF EXISTS pg_trgm")
    execute("DROP EXTENSION IF EXISTS citext")
  end
end

