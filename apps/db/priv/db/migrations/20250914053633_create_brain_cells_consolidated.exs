# apps/db/priv/db/migrations/20250914053633_create_brain_cells_consolidated.exs
defmodule Db.Migrations.CreateBrainCellsConsolidated do
  use Ecto.Migration

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle
  )

  # POS whitelist as comma-separated quoted list for SQL ANY(ARRAY[…])
  defp allowed_pos_sql do
    @allowed_pos
    |> Enum.map(&("'#{&1}'"))
    |> Enum.join(", ")
  end

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

      # gram_function as array (text[]), default [], NOT NULL
      add :gram_function, {:array, :text}, null: false, default: []

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

      timestamps()
    end

    # ---------------- Indexes ----------------
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

    # GIN index for array membership on gram_function
    execute("""
    CREATE INDEX IF NOT EXISTS brain_cells_gram_function_gin_idx
    ON brain_cells USING GIN (gram_function)
    """)

    # ---------------- Data hygiene / normalization ----------------
    # Normalize norm for any pre-seeded rows
    execute("""
    UPDATE brain_cells
       SET norm = citext(
         regexp_replace(trim(lower(word::text)), '\\s+', ' ', 'g')
       );
    """)

    # (Optional) Trim id and drop trailing pipes if any leaked in from prior seeds
    # Safe only if `id` is not referenced by FKs elsewhere.
    execute("""
    UPDATE brain_cells
       SET id = regexp_replace(btrim(id), '\\|+$', '', 'g')
     WHERE id ~ '\\|$' OR id <> btrim(id);
    """)

    # ---------------- Hardening constraints (prevents UNK/seed leaks) ----------------
    # 1) POS cannot be NULL or 'unk' and must be in allowed set
    execute("""
    ALTER TABLE brain_cells
    ADD CONSTRAINT brain_cells_pos_valid
      CHECK (pos IS NOT NULL AND pos <> 'unk' AND pos = ANY(ARRAY[#{allowed_pos_sql()}]));
    """)

    # 2) ID must not contain placeholder segments and must be trimmed
    execute("""
    ALTER TABLE brain_cells
    ADD CONSTRAINT brain_cells_id_no_seed_or_unk
      CHECK (id = btrim(id) AND id NOT LIKE '%|unk|%' AND id NOT LIKE '%|seed|%');
    """)

    # 3) ID shape: text|pos[|suffix] — suffix is alnum/underscore; no trailing pipe
    execute("""
    ALTER TABLE brain_cells
    ADD CONSTRAINT brain_cells_id_shape
      CHECK (
        btrim(id) ~ '^[^|]+\\|(noun|verb|adjective|adverb|interjection|phrase|proper_noun|pronoun|determiner|preposition|conjunction|numeral|particle)(\\|[A-Za-z0-9_]+)?$'
      );
    """)

    # 4) POS inside the ID must match the pos column exactly
    execute("""
    ALTER TABLE brain_cells
    ADD CONSTRAINT brain_cells_id_pos_matches_column
      CHECK (split_part(btrim(id), '|', 2) = pos);
    """)

    # 5) POS-aware allowlist for gram_function (no subqueries; uses array containment)
    execute("""
    ALTER TABLE brain_cells
    ADD CONSTRAINT brain_cells_gram_function_allowed_by_pos
      CHECK (
        CASE
          WHEN pos = 'noun' THEN gram_function <@ ARRAY[
            'countable','uncountable','plural-only','usually plural'
          ]::text[]
          WHEN pos = 'verb' THEN gram_function <@ ARRAY[
            'transitive','intransitive','ditransitive','ambitransitive',
            'copular','auxiliary','modal','ergative','impersonal'
          ]::text[]
          WHEN pos = 'adjective' THEN gram_function <@ ARRAY[
            'attributive-only','predicative-only','postpositive','comparative-only'
          ]::text[]
          ELSE TRUE
        END
      );
    """)

    # ---------------- Optional vector column + index ----------------
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
    execute("DROP INDEX IF EXISTS brain_cells_gram_function_gin_idx")

    # Drop hardening constraints (if present)
    execute("ALTER TABLE brain_cells DROP CONSTRAINT IF EXISTS brain_cells_gram_function_allowed_by_pos;")
    execute("ALTER TABLE brain_cells DROP CONSTRAINT IF EXISTS brain_cells_id_pos_matches_column;")
    execute("ALTER TABLE brain_cells DROP CONSTRAINT IF EXISTS brain_cells_id_shape;")
    execute("ALTER TABLE brain_cells DROP CONSTRAINT IF EXISTS brain_cells_id_no_seed_or_unk;")
    execute("ALTER TABLE brain_cells DROP CONSTRAINT IF EXISTS brain_cells_pos_valid;")

    drop_if_exists index(:brain_cells, [:norm], name: :brain_cells_norm_index)
    drop_if_exists index(:brain_cells, [:word], name: :brain_cells_word_index)

    drop table(:brain_cells)

    execute("DROP EXTENSION IF EXISTS vector")
    execute("DROP EXTENSION IF EXISTS pg_trgm")
    execute("DROP EXTENSION IF EXISTS citext")
  end
end

