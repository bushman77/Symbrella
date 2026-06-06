defmodule Core.PhraseRepo.Default do
  @moduledoc """
  DB-backed boundary adapter for phrase existence checks.

  Pure MWE/token logic depends on the `Core.PhraseRepo` behaviour; this default
  implementation delegates to the named Ecto/Db source for `Db.BrainCell.norm`.
  """
  @behaviour Core.PhraseRepo

  alias Core.PhraseRepo.DbSource

  @impl true
  def exists?(phrase) when is_binary(phrase) do
    norm = normalize(phrase)
    DbSource.exists?(norm)
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
