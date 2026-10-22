defmodule Db.LtmBoundedQueryTest do
  use ExUnit.Case, async: false

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Db)
    :ok
  end

  test "bounded LTM lookup compiles its window subquery and returns full BrainCell fields" do
    input = %{
      tokens: [
        %{phrase: "the", index: 0},
        %{phrase: "checker", index: 1}
      ]
    }

    # This executes the window-subquery SQL even if the test dictionary is
    # empty. The previous %{b: b, rn: over(...)} projection raised
    # Ecto.SubQueryError at query compilation before touching the database.
    assert {:ok, %{rows: rows, missing_norms: missing, db_hits: %MapSet{} = hits}} =
             Db.ltm(input, limit_per_norm: 8)

    assert is_list(missing)
    assert %MapSet{} = hits
    assert Enum.count(rows, &(&1.norm == "the")) <= 8
    assert Enum.count(rows, &(&1.norm == "checker")) <= 8

    assert Enum.all?(rows, fn row ->
             is_binary(row.id) and
               is_binary(row.norm) and
               is_integer(row.token_index) and
               row.token_index in [0, 1] and
               is_binary(row.pos) and
               is_list(row.semantic_atoms)
           end)
  end

  test "per-norm one-result cap and unlimited lookup both remain valid" do
    input = %{tokens: [%{phrase: "the", index: 0}]}

    assert {:ok, %{rows: capped}} = Db.ltm(input, limit_per_norm: 1)
    assert length(capped) <= 1

    assert {:ok, %{rows: unlimited}} = Db.ltm(input, limit_per_norm: :all)
    assert length(unlimited) >= length(capped)
  end
end
