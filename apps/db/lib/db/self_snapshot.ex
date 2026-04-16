defmodule Db.SelfSnapshot do
  @moduledoc """
  Durable self-model snapshot storage.

  Brain owns continuity policy. This schema only stores the snapshot payload and
  minimal version metadata needed for Brain.SelfContinuity to validate restores.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "self_snapshots" do
    field(:scope, :string, default: "runtime")
    field(:self_model_v, :integer, default: 1)
    field(:snapshot_v, :integer, default: 1)
    field(:snapshot, :map, default: %{})
    field(:source, :string, default: "runtime")

    timestamps(type: :naive_datetime_usec)
  end

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(snapshot, attrs) do
    snapshot
    |> cast(attrs, [:scope, :self_model_v, :snapshot_v, :snapshot, :source])
    |> validate_required([:scope, :self_model_v, :snapshot_v, :snapshot, :source])
    |> validate_number(:self_model_v, greater_than: 0)
    |> validate_number(:snapshot_v, greater_than: 0)
  end
end
