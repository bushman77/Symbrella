defmodule Core.PhraseRepo.DbSource do
  @moduledoc """
  Ecto/Db source for phrase existence checks against `Db.BrainCell.norm`.
  """

  import Ecto.Query, warn: false

  alias Db.BrainCell

  @spec exists?(String.t()) :: boolean()
  def exists?(norm) when is_binary(norm) do
    query = from(b in BrainCell, where: b.norm == ^norm, select: 1)
    Db.exists?(query)
  rescue
    _ -> false
  catch
    _, _ -> false
  end
end
