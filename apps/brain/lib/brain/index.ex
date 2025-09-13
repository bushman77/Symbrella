defmodule Brain.Index do
  @moduledoc """
  Minimal index shim.

  `ids_for_phrase/1` should return all active BrainCell IDs that begin with
  the given phrase (e.g., "kick the bucket|VERB|0"). For now, this scaffold
  returns an empty list.
  """

  @spec ids_for_phrase(String.t()) :: [String.t()]
  def ids_for_phrase(_phrase), do: []
end

