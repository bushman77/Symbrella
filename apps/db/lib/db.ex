defmodule Db do
  use Ecto.Repo,
    otp_app: :symbrella,
    adapter: Ecto.Adapters.Postgres

  import Ecto.Query, only: [from: 2]

  def word_exists?(word) when is_binary(word) do
    q = from b in Db.BrainCell, where: b.word == ^word, select: 1
    exists?(q) # Ecto ≥ 3.10; else: aggregate(q, :count) > 0
  end

  def id_exists?(id) when is_binary(id) do
    q = from b in Db.BrainCell, where: b.id == ^id, select: 1
    exists?(q)
  end
end

