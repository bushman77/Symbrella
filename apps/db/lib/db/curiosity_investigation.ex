defmodule Db.CuriosityInvestigation do
  @moduledoc """
  An operational audit event, never a knowledge assertion.

  One row per actual Curiosity issue attempt, persisted before execution and
  updated on completion. Raw user messages, place names, provider excerpts
  and rejected source URLs must not be stored here.
  """
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "curiosity_investigations" do
    field :issue_id, :integer
    field :worker, :string, default: "default"
    field :status, :string, default: "started"
    field :reason, :string
    field :acquisition, :string, default: "not_attempted"
    field :acquisition_reason, :string
    field :acquired_proof_id, :integer
    field :parent_proof_ids, {:array, :integer}, default: []
    field :origin, :string
    field :provider_attempts, {:array, :map}, default: []
    field :duration_ms, :integer
    field :completed_at, :utc_datetime_usec

    timestamps(type: :utc_datetime_usec)
  end

  @statuses ~w(started resolved unresolved skipped failed disabled)
  @acquisitions ~w(not_attempted captured not_captured)
  @workers ~w(default isolated)
  @providers ~w(wikidata statistics_canada wikipedia other)
  @provider_statuses ~w(source_supported unavailable partial_support error unknown)
  defp code_pattern, do: ~r/\A[a-z][a-z0-9_]*\z/

  def changeset(event, attrs) do
    event
    |> cast(attrs, [
      :issue_id, :worker, :status, :reason, :acquisition,
      :acquisition_reason, :acquired_proof_id, :parent_proof_ids,
      :origin, :provider_attempts, :duration_ms, :completed_at
    ])
    |> validate_required([:issue_id, :worker, :status, :acquisition])
    |> validate_number(:issue_id, greater_than: 0)
    |> validate_inclusion(:status, @statuses)
    |> validate_inclusion(:acquisition, @acquisitions)
    |> validate_inclusion(:worker, @workers)
    |> validate_length(:reason, max: 100)
    |> validate_length(:acquisition_reason, max: 100)
    |> validate_length(:origin, max: 100)
    |> validate_format(:reason, code_pattern())
    |> validate_format(:origin, code_pattern())
    |> validate_format(:acquisition_reason, code_pattern())
    |> validate_change(:provider_attempts, fn :provider_attempts, attempts ->
      if Enum.all?(attempts, &valid_provider_attempt?/1),
        do: [], else: [provider_attempts: "must contain only fixed provider/status codes"]
    end)
    |> validate_number(:acquired_proof_id, greater_than: 0)
    |> validate_number(:duration_ms, greater_than_or_equal_to: 0)
    |> validate_length(:parent_proof_ids, max: 2)
    |> validate_length(:provider_attempts, max: 4)
    |> foreign_key_constraint(:issue_id)
  end

  defp valid_provider_attempt?(%{"provider" => provider, "status" => status} = attempt),
    do: map_size(attempt) == 2 and provider in @providers and
      status in @provider_statuses
  defp valid_provider_attempt?(_), do: false
end
