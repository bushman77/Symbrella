defmodule SymbrellaWeb.EpisodesData do
  @moduledoc """
  Web data boundary for persisted episode reads.

  LiveViews should consume plain lists from this module instead of reaching
  directly into Db contexts.
  """

  @compile {:no_warn_undefined, Db.Episode}

  @spec list_all() :: list()
  def list_all do
    if Code.ensure_loaded?(Db.Episode) and function_exported?(Db.Episode, :list_all, 0) do
      Db.Episode.list_all()
    else
      []
    end
  end
end
