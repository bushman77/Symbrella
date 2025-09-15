defmodule Db do
  @moduledoc """
  Umbrella-wide Repo. One DB, one config source.

  Crash-proof helpers:
  - table_exists?/1 uses to_regclass (no ERROR logs)
  - word_exists?/1 bails if table doesn't exist
  - senses_for_word/1, senses_for_words/1 (lightweight maps)
  - senses_for_word_full/1, senses_for_words_full/1 (return full %Db.BrainCell{})
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/repo"

  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell

  @spec table_exists?(binary()) :: boolean()
  def table_exists?(table \\ "brain_cells") when is_binary(table) do
    case Ecto.Adapters.SQL.query(__MODULE__, "SELECT to_regclass($1)", [table]) do
      {:ok, %{rows: [[nil]]}} -> false
      {:ok, %{rows: [[_]]}}   -> true
      _                       -> false
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

  # ——— existing lightweight senses (map selects) ———

  @doc "Return ALL senses (POS + indices) for a given word (lightweight map)."
  def senses_for_word(word) when is_binary(word) and byte_size(word) > 0 do
    if table_exists?("brain_cells") do
      from(b in BrainCell,
        where: b.word == ^word,
        select: %{id: b.id, word: b.word, pos: b.pos, type: b.type},
        order_by: [asc: b.pos, asc: b.id]
      )
      |> all()
    else
      []
    end
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  def senses_for_word(_), do: []

  @doc "Batched lightweight: word => [%{id, word, pos, type}]"
  def senses_for_words(words) when is_list(words) do
    uniq =
      words
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.downcase/1)
      |> Enum.uniq()

    cond do
      uniq == [] -> %{}
      not table_exists?("brain_cells") -> %{}
      true ->
        rows =
          from(b in BrainCell,
            where: b.word in ^uniq,
            select: %{id: b.id, word: b.word, pos: b.pos, type: b.type},
            order_by: [asc: b.word, asc: b.pos, asc: b.id]
          )
          |> all()

        Enum.group_by(rows, & &1.word)
    end
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  # ——— NEW: full schema fetches ———

  @doc "Return ALL senses as full %Db.BrainCell{} rows for a given word."
  def senses_for_word_full(word) when is_binary(word) and byte_size(word) > 0 do
    if table_exists?("brain_cells") do
      from(b in BrainCell,
        where: b.word == ^word,
        order_by: [asc: b.pos, asc: b.id]
      )
      |> all()
    else
      []
    end
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  def senses_for_word_full(_), do: []

  @doc "Batched FULL: word => [%Db.BrainCell{}]. One query, grouped by word."
  def senses_for_words_full(words) when is_list(words) do
    uniq =
      words
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&String.downcase/1)
      |> Enum.uniq()

    cond do
      uniq == [] -> %{}
      not table_exists?("brain_cells") -> %{}
      true ->
        rows =
          from(b in BrainCell,
            where: b.word in ^uniq,
            order_by: [asc: b.word, asc: b.pos, asc: b.id]
          )
          |> all()

        Enum.group_by(rows, & &1.word)
    end
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end
end

