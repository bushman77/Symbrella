defmodule Core.Response.AgencyLedger do
  @moduledoc """
  Records response decisions into the durable self-agency ledger.

  The ledger is an autobiographical substrate: it stores the action Symbrella
  selected, the self-state that shaped that choice, and the immediate outcome.
  """

  alias Core.Response.AgencyEventBuilder

  @spec record_response(String.t(), String.t(), map(), map()) ::
          :disabled | {:ok, Db.AgencyEvent.t()} | {:error, Ecto.Changeset.t()}
  def record_response(user_text, assistant_text, features, meta)
      when is_map(features) and is_map(meta) do
    if enabled?() do
      user_text
      |> AgencyEventBuilder.response_attrs(assistant_text, features, meta)
      |> Db.AgencyEvents.create_event()
    else
      :disabled
    end
  end

  def record_response(_user_text, _assistant_text, _features, _meta), do: :disabled

  @spec record_command(map() | struct(), map(), keyword()) ::
          :disabled | {:ok, Db.AgencyEvent.t()} | {:error, Ecto.Changeset.t()}
  def record_command(command, result, opts \\ [])

  def record_command(command, result, opts) when is_map(result) and is_list(opts) do
    if enabled?() do
      command
      |> AgencyEventBuilder.command_attrs(result, opts)
      |> Db.AgencyEvents.create_event()
    else
      :disabled
    end
  end

  def record_command(_command, _result, _opts), do: :disabled

  defp enabled? do
    Application.get_env(:core, :agency_ledger_enabled?, true) == true
  end
end
