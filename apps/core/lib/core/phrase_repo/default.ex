defmodule Core.PhraseRepo.Default do
  @moduledoc "DB-backed phrase existence (uses Db.BrainCell.norm)."
  @behaviour Core.PhraseRepo

  import Ecto.Query, warn: false
  alias Db
  alias Db.BrainCell

  @impl true
  def exists?(phrase) when is_binary(phrase) do
    norm = normalize(phrase)
    query = from(b in BrainCell, where: b.norm == ^norm, select: 1)
    Db.exists?(query)
  rescue
    e ->
      require Logger
      Logger.error("PhraseRepo.exists?/1 error: #{inspect(e)}")
      false
  end

  defp normalize(p) do
    p
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end
end
