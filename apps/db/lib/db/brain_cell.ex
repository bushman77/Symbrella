defmodule Db.BrainCell do
  use Ecto.Schema
  import Ecto.Changeset

  @statuses ~w(inactive active dormant decayed)
  @primary_key {:id, :string, autogenerate: false}

  schema "brain_cells" do
    field :word, :string
    field :pos, :string
    field :definition, :string
    field :example, :string
    field :gram_function, :string

    field :synonyms, {:array, :string}, default: []
    field :antonyms, {:array, :string}, default: []
    field :semantic_atoms, {:array, :string}, default: []

    field :type, :string
    field :status, :string, default: "inactive"

    field :activation, :float, default: 0.0
    field :modulated_activation, :float, default: 0.0
    field :dopamine, :float, default: 0.0
    field :serotonin, :float, default: 0.0

    field :position, {:array, :float}

    # jsonb[] in DB; list of maps in Ecto
    field :connections, {:array, :map}, default: []

    field :last_dose_at, :utc_datetime_usec
    field :last_substance, :string
    field :token_id, :integer  # maps to BIGINT in Postgres

    # pgvector Ecto type (db app owns the dep)
    field :embedding, Pgvector.Ecto.Vector

    timestamps()
  end

  @doc false
  def changeset(cell, attrs) do
    cell
    |> cast(attrs, [
      :id, :word, :pos, :definition, :example, :gram_function,
      :synonyms, :antonyms, :semantic_atoms,
      :type, :status,
      :activation, :modulated_activation, :dopamine, :serotonin,
      :position, :connections,
      :last_dose_at, :last_substance,
      :token_id, :embedding
    ])
    |> validate_required([:id, :word, :pos, :status])
    |> validate_inclusion(:status, @statuses)
  end
end

