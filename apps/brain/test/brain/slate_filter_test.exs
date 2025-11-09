defmodule Brain.LIFG.SlateFilterTest do
  use ExUnit.Case, async: true

  test "drops impossible POS but keeps phrases" do
    slate = %{
      0 => [%{id: "A|noun|0"}, %{id: "a|verb|0"}, %{id: "eat|verb|2"}],
      1 => [%{id: "a banana|phrase|fallback"}]
    }

    got = Brain.LIFG.SlateFilter.sanitize_map(slate)
    assert got[0] == [%{id: "eat|verb|2"}]
    assert got[1] == [%{id: "a banana|phrase|fallback"}]
  end
end

