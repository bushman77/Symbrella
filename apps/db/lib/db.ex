defmodule Db do
  @moduledoc """
  Minimal Repo + a single existence check by `word` (no data fetch).
  """

  use Ecto.Repo,
    otp_app: :symbrella,
    adapter: Ecto.Adapters.Postgres

  alias Brain.Cell

  import Ecto.Query, only: [from: 2]

  @doc """
  Returns true if any BrainCell exists with the given `word`.

  Uses a lightweight `SELECT 1 LIMIT 1` so no row data is loaded.
  """
  @spec word_exists?(String.t()) :: boolean()
  def word_exists?(word) when is_binary(word) do
    # If you're on Ecto >= 3.10, you can simplify to:
    #   exists?(from(b in Cell, where: b.word == ^word))
    query =
      from b in Cell,
        where: b.word == ^word,
        select: 1,
        limit: 1

    case __MODULE__.one(query) do
      1 -> true
      _ -> false
    end
  end
end

