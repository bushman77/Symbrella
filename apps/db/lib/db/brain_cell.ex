defmodule Db.BrainCell do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :string, autogenerate: false}
  @timestamps_opts [type: :naive_datetime]
  @derive {Jason.Encoder,
           only: [:id, :word, :norm, :pos, :type, :gram_function,
                  :definition, :example, :synonyms, :antonyms,
                  :semantic_atoms, :status, :activation,
                  :modulated_activation, :dopamine, :serotonin,
                  :position, :connections, :last_dose_at,
                  :last_substance, :token_id]}

  schema "brain_cells" do
    field :word, :string
    field :norm, :string
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
    field :connections, {:array, :map}, default: []
    field :last_dose_at, :utc_datetime_usec
    field :last_substance, :string
    field :token_id, :integer
    # field :embedding, Pgvector.Ecto.Vector

    timestamps()
  end

  def changeset(cell, attrs) do
    cell
    |> cast(attrs, __schema__(:fields))
    |> validate_required([:word, :norm])
    # (no unique_constraint here; PK :id handles uniqueness)
  end
end

