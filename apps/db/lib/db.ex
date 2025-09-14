defmodule Db do
  @moduledoc """
  Lightweight Ecto Repo plus a minimal query helper used by Core/Brain.

  Design goals:
  - Keep the surface area tiny and stable.
  - Provide existence checks without pulling rows.
  - Avoid doctests in this module to keep CI noise-free.
  """

  use Ecto.Repo,
    otp_app: :symbrella,
    adapter: Ecto.Adapters.Postgres

  import Ecto.Query, only: [from: 2]

  # If your BrainCell schema lives under a different namespace,
  # adjust this alias accordingly (e.g., `alias Symbrella.Db.BrainCell`).
  alias Db.BrainCell

  @doc """
  Returns `true` if any BrainCell row exists with an exact `word` match.

  This performs a minimal `SELECT 1` and **does not** load full rows.
  """
  @spec word_exists?(binary()) :: boolean()
  def word_exists?(word) when is_binary(word) and byte_size(word) > 0 do
    q =
      from b in BrainCell,
        where: b.word == ^word,
        select: 1

    __MODULE__.exists?(q)
  end

  def word_exists?(_), do: false

  @doc false
  def hello, do: :world
end

