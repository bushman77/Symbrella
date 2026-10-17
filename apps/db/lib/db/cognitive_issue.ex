defmodule Db.CognitiveIssue do
  @moduledoc """
  First-class epistemic uncertainty. An unresolved issue is NOT a fact.
  Durable issues can later be selected by Curiosity without granting Curiosity
  authority to publish conclusions or silently rewrite verified knowledge.
  """
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  @kinds ~w(contradictory_premises missing_dated_evidence invalid_derivation missing_reasoning_link)
  @actions ~w(recheck_sources find_dated_sources review_derivation find_linking_premise)
  @statuses ~w(open investigating resolved blocked)

  schema "cognitive_issues" do
    field :fingerprint, :string
    field :kind, :string
    field :status, :string, default: "open"
    field :subject_key, :string
    field :reference_year, :integer
    field :priority, :float, default: 0.5
    field :evidence, :map, default: %{}
    field :next_action, :string
    field :resolution, :map
    field :resolved_at, :utc_datetime_usec

    timestamps(type: :utc_datetime_usec)
  end

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(issue, attrs) do
    issue
    |> cast(attrs, [
      :fingerprint, :kind, :status, :subject_key, :reference_year,
      :priority, :evidence, :next_action, :resolution, :resolved_at
    ])
    |> validate_required([
      :fingerprint, :kind, :status, :subject_key, :next_action, :priority
    ])
    |> validate_length(:fingerprint, is: 64)
    |> validate_length(:subject_key, is: 64)
    |> validate_inclusion(:kind, @kinds)
    |> validate_inclusion(:status, @statuses)
    |> validate_inclusion(:next_action, @actions)
    |> validate_number(:priority, greater_than_or_equal_to: 0.0, less_than_or_equal_to: 1.0)
    |> unique_constraint(:fingerprint)
  end
end
