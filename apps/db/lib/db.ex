defmodule Db do
  @moduledoc """
  Umbrella-wide Repo. One DB, one config source.

  Crash-proof helpers:
  - `table_exists?/1` uses to_regclass (no ERROR logs)
  - `word_exists?/1` bails if table doesn't exist
  """

  use Ecto.Repo,
    otp_app: :db,                               # <<<<<< IMPORTANT
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/repo"

  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell

  @doc """
  Cheap, non-raising table presence check (respects search_path).
  """
  @spec table_exists?(binary()) :: boolean()
  def table_exists?(table \\ "brain_cells") when is_binary(table) do
    case Ecto.Adapters.SQL.query(__MODULE__, "SELECT to_regclass($1)", [table]) do
      {:ok, %{rows: [[nil]]}} -> false
      {:ok, %{rows: [[_]]}} -> true
      _ -> false
    end
  rescue
    _ -> false
  catch
    _, _ -> false
  end

  @spec word_exists?(binary()) :: boolean()
  def word_exists?(word) when is_binary(word) and byte_size(word) > 0 do
    if table_exists?("brain_cells") do
      q = from b in BrainCell, where: b.word == ^word, select: 1
      try do
        __MODULE__.exists?(q)
      rescue
        _ -> false
      catch
        _, _ -> false
      end
    else
      false
    end
  end

  def word_exists?(_), do: false
end

