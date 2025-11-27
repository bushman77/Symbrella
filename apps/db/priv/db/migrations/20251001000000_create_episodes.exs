defmodule Db.Migrations.CreateEpisodes do
  use Ecto.Migration

  @moduledoc """
  Creates `episodes` table for Hippocampus persistent slate storage.

  Notes
  -----
  • `tokens` is a text[] (GIN index) used for fast overlap queries via the `&&` operator.
  • `si` stores the raw SemanticInput snapshot as jsonb.
  • `embedding` uses the `vector` type (pgvector). We create an HNSW ANN index by default.
  • We create the pgvector extension if missing. Rollback is a NO-OP (do not drop extensions).
  """

  def change do
    # Ensure pgvector extension exists (no-op if already installed).
    # Rollback is intentionally a no-op to avoid breaking other vector users.
    execute("CREATE EXTENSION IF NOT EXISTS vector", "SELECT 1")

    create table(:episodes) do
      add :user_id, :uuid

      add :tokens, {:array, :text}, null: false, default: []
      add :token_count, :integer, null: false, default: 0

      add :si, :jsonb, null: false, default: fragment("'{}'::jsonb")
      add :tags, {:array, :text}, default: []

      # pgvector column. Keep size aligned with your embedder output dimension.
      add :embedding, :vector, size: 1536

      timestamps(type: :naive_datetime_usec)
    end

    create index(:episodes, [:tokens], using: :gin)
    create index(:episodes, [:tags], using: :gin)
    create index(:episodes, [:user_id])
    create index(:episodes, [:inserted_at])

    execute(
      """
      CREATE INDEX IF NOT EXISTS episodes_embedding_hnsw
      ON episodes
      USING hnsw (embedding vector_cosine_ops)
      WHERE embedding IS NOT NULL
      """,
      "DROP INDEX IF EXISTS episodes_embedding_hnsw"
    )
  end
end

