defmodule Db.BrainCell do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :string, autogenerate: false}
  @statuses ~w(inactive active dormant decayed)

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
    field :connections, {:array, :map}, default: []

    field :last_dose_at, :utc_datetime_usec
    field :last_substance, :string
    field :token_id, :integer

    # pgvector (optional, created conditionally in migration)
    field :embedding, Pgvector.Ecto.Vector

    timestamps()
  end

  @doc false
  def changeset(cell, attrs) do
    cell
    |> cast(attrs, __schema__(:fields))
    |> validate_required([:id, :word, :status])
    |> validate_inclusion(:status, @statuses)
    # NOTE: word is indexed but NOT unique (allows multiple senses per word)
    # |> unique_constraint(:word, name: :brain_cells_word_index)  # ← removed
  end
end

