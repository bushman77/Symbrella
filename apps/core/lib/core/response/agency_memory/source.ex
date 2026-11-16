defmodule Core.Response.AgencyMemory.Source do
  @moduledoc """
  Boundary source for recent agency ledger events.

  `Core.Response.AgencyMemory` summarizes policy pressure; this module owns the
  `Db.AgencyEvents` read.
  """

  @spec recent(term(), keyword()) :: list(map())
  def recent(session_id, opts \\ []) do
    limit = Keyword.get(opts, :limit)

    if Code.ensure_loaded?(Db.AgencyEvents) and function_exported?(Db.AgencyEvents, :recent, 1) do
      # Diagnostic accountability receipts are not behavioral outcomes.
      # Keep them out of the bounded self-state learning-pressure window.
      Db.AgencyEvents.recent(
        session_id: session_id,
        limit: limit,
        exclude_sources: ["lexical_role_accountability"]
      )
    else
      []
    end
  rescue
    _ -> []
  catch
    _, _ -> []
  end
end
