defmodule Db.LTMPosDiversityTest do
  use ExUnit.Case, async: true

  alias Db.BrainCell

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    :ok
  end

  test "POS-balanced bounded read retains an attested verb when noun senses dominate" do
    word = "zzcolonyposprobe"

    # A real persisted verb sense is mandatory. Nothing synthesizes
    # dictionary entries on behalf of the consumer.
    insert_sense(word, "verb", 1)

    for i <- 1..10 do
      insert_sense(word, "noun", i)
    end

    si = %{tokens: [%{index: 4, phrase: word}]}

    assert {:ok, ordinary} = Db.ltm(si, limit_per_norm: 8)
    assert {:ok, balanced} = Db.ltm(si, limit_per_norm: 8, pos_diverse?: true)

    assert length(ordinary.rows) == 8
    assert length(balanced.rows) == 8
    assert MapSet.new(Enum.map(balanced.rows, & &1.pos)) ==
             MapSet.new(["noun", "verb"])
    assert Enum.all?(balanced.rows, &(&1.token_index == 4))
    assert Enum.all?(balanced.rows, &(&1.norm == word))
    assert balanced.db_hits == MapSet.new([word])
    assert balanced.missing_norms == []
  end

  test "POS-balanced read abstains from fabricating a missing verb sense" do
    word = "zzcolonynounonly"

    for i <- 1..10 do
      insert_sense(word, "noun", i)
    end

    assert {:ok, result} =
             Db.ltm(%{tokens: [%{index: 0, phrase: word}]},
               limit_per_norm: 8,
               pos_diverse?: true
             )

    assert length(result.rows) == 8
    assert Enum.all?(result.rows, &(&1.pos == "noun"))
    assert result.db_hits == MapSet.new([word])
  end

  defp insert_sense(word, pos, index) do
    attrs = %{
      id: "#{word}|#{pos}|#{index}",
      word: word,
      pos: pos
    }

    %BrainCell{}
    |> BrainCell.changeset(attrs)
    |> Db.insert!()
  end
end
