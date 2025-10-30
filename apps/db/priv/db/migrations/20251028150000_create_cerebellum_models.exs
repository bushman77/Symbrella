defmodule Db.Repo.Migrations.CreateCerebellumModels do
  use Ecto.Migration

  def change do
    create table(:cerebellum_models) do
      add :scope, :string, null: false
      # Stable hash of any “context” term (see Brain.Cerebellum.context_key/1)
      add :context_key, :binary, null: false

      # Keep arrays for portability. If you prefer pgvector:
      #   add :weights, :vector, size: 5
      # and adjust the schema type accordingly.
      add :weights, {:array, :float}, null: false

      add :count_seen, :integer, null: false, default: 0
      add :ema_error, :float, null: false, default: 0.0
      add :feature_schema, :integer, null: false, default: 1

      timestamps()
    end

    create unique_index(:cerebellum_models, [:scope, :context_key])
  end
end

