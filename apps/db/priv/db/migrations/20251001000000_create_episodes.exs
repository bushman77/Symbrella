defmodule Db.Migrations.CreateEpisodes do
  use Ecto.Migration

  @moduledoc """
  Creates `episodes` table for Hippocampus persistent slate storage.

  Notes:
  - `tokens` is a text[] (GIN index) used for fast overlap queries with the
    `&&` operator.
  - `si` stores the raw SemanticInput snapshot as jsonb.
  - `embedding` column uses the `vector` type (pgvector). This migration adds
    the column and creates an HNSW ANN index by default (good for streaming
    inserts). If you prefer IVFFLAT (bulk/large-scale), uncomment the
    IVFFLAT index line and create it after bulk-loading.

  - The migration attempts to create the `vector` extension (if available).
    If your DB user cannot create extensions, either run the extension setup as
    a superuser or remove the `execute/2` call.
  """

  def change do
    # Ensure pgvector extension exists (no-op if already installed)
    execute("CREATE EXTENSION IF NOT EXISTS vector", "DROP EXTENSION IF EXISTS vector")

    create table(:episodes) do
      add :user_id, :uuid

      # normalized tokens / phrases used for overlap queries
      add :tokens, {:array, :text}, null: false, default: []
      add :token_count, :integer, null: false, default: 0

      # raw semantic input snapshot (jsonb)
      add :si, :jsonb, null: false, default: fragment("'{}'::jsonb")

      # optional tags (labels like "important", "intent:purchase")
      add :tags, {:array, :text}, default: []

      # Embedding column (pgvector). Set `size:` to match your embedding model.
      add :embedding, :vector, size: 1536

      timestamps(type: :naive_datetime_usec)
    end

    # Index recommendations
    create index(:episodes, [:tokens], using: :gin)
    create index(:episodes, [:tags], using: :gin)
    create index(:episodes, [:user_id])
    create index(:episodes, [:inserted_at])

    # Create an ANN index for the embedding column.
    # HNSW is a sensible default for dynamic inserts and immediate availability.
    execute(
      "CREATE INDEX IF NOT EXISTS episodes_embedding_hnsw ON episodes USING hnsw (embedding vector_cosine_ops)",
      "DROP INDEX IF EXISTS episodes_embedding_hnsw"
    )

    # If you prefer IVFFLAT (better for very large bulk-loaded datasets),
    # comment out the HNSW execute above and uncomment the following execute.
    # Note: IVFFLAT performs best when built after bulk-loading and requires
    # tuning the `lists` parameter (cluster count).
    # execute(
    #   "CREATE INDEX episodes_embedding_ivfflat ON episodes USING ivfflat (embedding vector_cosine_ops) WITH (lists = 100)",
    #   "DROP INDEX IF EXISTS episodes_embedding_ivfflat"
    # )
  end
end

