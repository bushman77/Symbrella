defmodule DbTest do
  use ExUnit.Case, async: true

  test "exports word_exists?/1" do
    assert function_exported?(Db, :word_exists?, 1)
  end

  test "word_exists?/1 guards invalid inputs without DB calls" do
    refute Db.word_exists?(nil)
    refute Db.word_exists?("")
    refute Db.word_exists?(123)
  end
end

