defmodule Db.JSONL.IOTest do
  use ExUnit.Case, async: true

  alias Db.JSONL.IO

  test "default dictionary path is the umbrella-root priv directory" do
    expected = Path.expand("priv/english.jsonl")

    assert IO.default_path() == expected
    assert IO.default_paths() == [expected]
    assert IO.resolve_path([]) == expected
  end

  test "explicit dictionary paths remain supported" do
    other = Path.expand("user-selected-corpus.jsonl")
    assert IO.resolve_path(path: other) == other
  end
end
