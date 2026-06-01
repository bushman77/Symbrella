defmodule Db.AgencyEvent do
  @moduledoc """
  Durable record of Symbrella acting as an inspectable agent.

  Each row captures one decision boundary: what input was seen, what action was
  selected, which self-state shaped the choice, and what immediate outcome was
  produced. Higher-level learning can derive counterfactuals from this ledger.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  schema "agency_events" do
    field(:agency_v, :integer, default: 1)
    field(:session_id, :string, default: "global")
    field(:actor, :string, default: "symbrella")
    field(:source, :string, default: "core_response")
    field(:status, :string, default: "observed")
    field(:action, :string)
    field(:input, :map, default: %{})
    field(:decision, :map, default: %{})
    field(:reasons, :map, default: %{})
    field(:self_model, :map, default: %{})
    field(:self_state, :map, default: %{})
    field(:outcome, :map, default: %{})
    field(:reflection, :map, default: %{})

    timestamps(type: :naive_datetime_usec)
  end

  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(event, attrs) do
    event
    |> cast(attrs, [
      :agency_v,
      :session_id,
      :actor,
      :source,
      :status,
      :action,
      :input,
      :decision,
      :reasons,
      :self_model,
      :self_state,
      :outcome,
      :reflection
    ])
    |> validate_required([:agency_v, :session_id, :actor, :source, :status, :action])
    |> validate_number(:agency_v, greater_than: 0)
  end
end
