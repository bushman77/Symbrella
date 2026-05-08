defmodule Brain.ATL.AttachLifgPairsTest do
  use ExUnit.Case, async: true

  alias Brain.ATL

  test "returns input unchanged when atl_slate is nil" do
    si = %{
      tokens: [],
      atl_slate: nil,
      evidence: nil,
      trace: nil
    }

    assert ATL.attach_lifg_pairs(si, []) == si
  end

  test "returns input unchanged when tokens are nil and winners are missing" do
    si = %{
      tokens: nil,
      atl_slate: %{},
      evidence: nil,
      trace: nil
    }

    assert ATL.attach_lifg_pairs(si, []) == si
  end
end
