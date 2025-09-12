# apps/db/lib/db/brain_cell.ex
defmodule Db.BrainCell do
  @moduledoc "Ecto schema for brain_cells (DB layer only)."
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :string, autogenerate: false}
  @foreign_key_type :string

  schema "brain_cells" do
    # Identity
    field :word, :string
    field :pos,  :string

    # Lexical payload
    field :definition, :string
    field :example,    :string
    field :function,   :string
    field :synonyms,       {:array, :string}, default: []
    field :antonyms,       {:array, :string}, default: []
    field :semantic_atoms, {:array, :string}, default: []

    # Connections: list of maps like %{"to" => id, "strength" => float}
    field :connections, {:array, :map}, default: []

    # ML: stable numeric id per lemma (embedding index)
    field :token_id, :integer

    # Embeddings via pgvector
    field :embedding, Pgvector.Ecto.Vector
    field :embedding_model, :string
    field :embedding_updated_at, :utc_datetime_usec

    # Classification & status
    field :type,   Ecto.Enum, values: [:noun, :verb, :concept, :phrase, :emotion, :synapse]
    field :status, Ecto.Enum, values: [:inactive, :active, :dormant, :decayed], default: :inactive

    # Neurochemistry / activations
    field :activation,           :float, default: 0.0
    field :modulated_activation, :float, default: 0.0
    field :dopamine,             :float, default: 0.0
    field :serotonin,            :float, default: 0.0

    # Spatial
    field :position, {:array, :float}, default: [0.0, 0.0, 0.0]

    # Dosing meta
    field :last_dose_at,   :utc_datetime_usec
    field :last_substance, :string

    timestamps()
  end

  def changeset(cell, attrs) do
    cell
    |> cast(attrs, [
      :id, :word, :pos,
      :definition, :example, :function,
      :synonyms, :antonyms, :semantic_atoms,
      :connections,
      :token_id, :embedding, :embedding_model, :embedding_updated_at,
      :type, :status,
      :activation, :modulated_activation, :dopamine, :serotonin,
      :position, :last_dose_at, :last_substance
    ])
    |> validate_length(:definition, max: 20_000)
    |> validate_length(:example, max: 10_000)
    # Keep aligned with your DB CHECK constraint name
    |> check_constraint(:status, name: :brain_cells_status_check)
  end
end

