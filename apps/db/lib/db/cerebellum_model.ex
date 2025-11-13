defmodule Db.CerebellumModel do
  @moduledoc """
  Compact forward-model parameters for Cerebellum, keyed by (scope, context_key).

  • `scope` — where this model applies, e.g. "lifg_stage1"
  • `context_key` — 32-byte binary hash of a stable context term (see Brain.Cerebellum.context_key/1)
  • `weights` — small float vector (default 5 dims) used to nudge candidate scores
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "cerebellum_models" do
    field(:scope, :string)
    field(:context_key, :binary)
    field(:weights, {:array, :float})
    field(:count_seen, :integer, default: 0)
    field(:ema_error, :float, default: 0.0)
    field(:feature_schema, :integer, default: 1)
    timestamps()
  end

  def changeset(model, attrs) do
    model
    |> cast(attrs, [:scope, :context_key, :weights, :count_seen, :ema_error, :feature_schema])
    |> validate_required([:scope, :context_key, :weights])
    |> unique_constraint([:scope, :context_key])
  end
end
