defmodule Db.Migrations.CreateAgencyEvents do
  use Ecto.Migration

  def change do
    create table(:agency_events) do
      add(:agency_v, :integer, null: false, default: 1)
      add(:session_id, :string, null: false, default: "global")
      add(:actor, :string, null: false, default: "symbrella")
      add(:source, :string, null: false, default: "core_response")
      add(:status, :string, null: false, default: "observed")
      add(:action, :string, null: false)
      add(:input, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:decision, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:reasons, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:self_model, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:self_state, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:outcome, :jsonb, null: false, default: fragment("'{}'::jsonb"))
      add(:reflection, :jsonb, null: false, default: fragment("'{}'::jsonb"))

      timestamps(type: :naive_datetime_usec)
    end

    create(index(:agency_events, [:session_id, :inserted_at]))
    create(index(:agency_events, [:source]))
    create(index(:agency_events, [:action]))
    create(index(:agency_events, [:agency_v]))
  end
end
