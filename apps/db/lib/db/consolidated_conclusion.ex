defmodule Db.ConsolidatedConclusion do
  @moduledoc """
  Persistent, provenance-bearing symbolic deductions, separate from lexical
  BrainCells, chat transcripts and hippocampal episodic observations.

  A row is a *source-supported derived assertion*, not a timeless world fact.
  Reuse requires re-derivation by Core and a compatible query.
  """
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "consolidated_conclusions" do
    field :fingerprint, :string
    field :pair_key, :string
    field :kind, :string
    field :reference_year, :integer
    field :premises, {:array, :map}, default: []
    field :derivation, :map, default: %{}
    field :source_urls, {:array, :string}, default: []
    field :provenance, :string
    timestamps(type: :utc_datetime_usec)
  end

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(row, attrs) do
    row
    |> cast(attrs, [
      :fingerprint, :pair_key, :kind, :reference_year, :premises,
      :derivation, :source_urls, :provenance
    ])
    |> validate_required([
      :fingerprint, :pair_key, :kind, :reference_year, :premises,
      :derivation, :source_urls, :provenance
    ])
    |> validate_length(:fingerprint, is: 64)
    |> validate_length(:pair_key, max: 64)
    |> validate_number(:reference_year, greater_than_or_equal_to: 1900)
    |> validate_length(:premises, is: 2)
    |> validate_length(:source_urls, is: 2)
    |> unique_constraint(:fingerprint)
  end
end
