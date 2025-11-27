defmodule Db.Migrations.CreateCerebellumModels do
  use Ecto.Migration

  def change do
    create table(:cerebellum_models) do
      add :scope, :string, null: false
      add :context_key, :binary, null: false

      add :weights, {:array, :float}, null: false

      add :count_seen, :integer, null: false, default: 0
      add :ema_error, :float, null: false, default: 0.0
      add :feature_schema, :integer, null: false, default: 1

      timestamps(type: :naive_datetime_usec)
    end

    create unique_index(:cerebellum_models, [:scope, :context_key])
  end
end

