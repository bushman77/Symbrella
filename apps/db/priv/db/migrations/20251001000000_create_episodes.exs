defmodule Db.Migrations.CreateEpisodes do
  use Ecto.Migration

  @moduledoc """
  Creates `episodes` table for Hippocampus persistent slate storage.

  Notes
  -----
  • `tokens` is a text[] (GIN index) used for fast overlap queries via the `&&` operator.
  • `si` stores the raw SemanticInput snapshot as jsonb.
  • `embedding` uses the `vector` type (pgvector). We create an HNSW ANN index by default
    (good for streaming inserts). If you prefer IVFFLAT for huge bulk loads, switch to
    the commented IVFFLAT snippet instead and build it post-load.
  • We attempt to create the `vector` extension. If your DB user cannot, run it manually.
  """

  def change do
    # Ensure pgvector extension exists (no-op if already installed)
    execute(
      "CREATE EXTENSION IF NOT EXISTS vector",
      "DROP EXTENSION IF EXISTS vector"
    )

    create table(:episodes) do
      add :user_id, :uuid

      # normalized tokens / phrases used for overlap queries
      add :tokens, {:array, :text}, null: false, default: []

      # optional convenience count (also computed in app changeset)
      add :token_count, :integer, null: false, default: 0

      # raw semantic input snapshot (jsonb)
      add :si, :jsonb, null: false, default: fragment("'{}'::jsonb")

      # optional tags (labels like "auto", "lifg")
      add :tags, {:array, :text}, default: []

      # Embedding column (pgvector). Set `size:` to match your embedding model (e.g. 1536).
      add :embedding, :vector, size: 1536

      timestamps(type: :naive_datetime_usec)
    end

    # Array + misc indexes
    create index(:episodes, [:tokens], using: :gin)
    create index(:episodes, [:tags], using: :gin)
    create index(:episodes, [:user_id])
    create index(:episodes, [:inserted_at])

    # ANN index for embedding (HNSW). Partial index avoids indexing NULLs.
    execute(
      """
      CREATE INDEX IF NOT EXISTS episodes_embedding_hnsw
      ON episodes
      USING hnsw (embedding vector_cosine_ops)
      WHERE embedding IS NOT NULL
      """,
      "DROP INDEX IF EXISTS episodes_embedding_hnsw"
    )

    # If you prefer IVFFLAT for very large/bulk datasets, comment out the HNSW block above
    # and use this (build post-load; tune `lists` as needed):
    #
    # execute(
    #   """
    #   CREATE INDEX IF NOT EXISTS episodes_embedding_ivfflat
    #   ON episodes
    #   USING ivfflat (embedding vector_cosine_ops)
    #   WITH (lists = 100)
    #   WHERE embedding IS NOT NULL
    #   """,
    #   "DROP INDEX IF EXISTS episodes_embedding_ivfflat"
    # )
  end
end

