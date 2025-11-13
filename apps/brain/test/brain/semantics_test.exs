defmodule Brain.SemanticsTest do
  use ExUnit.Case, async: true
  alias Brain.Semantics

  describe "bias_for/1" do
    test "returns correct bias for known lemmas" do
      assert Semantics.bias_for("intend") == 0.12
      assert Semantics.bias_for("read") == 0.08
    end

    test "returns 0.0 for unknown or nil lemmas" do
      assert Semantics.bias_for("flibberflop") == 0.0
      assert Semantics.bias_for(nil) == 0.0
    end
  end

  describe "regions_for/1" do
    test "returns region list for known lemmas" do
      assert Semantics.regions_for("intend") == [:lifg]
      assert Enum.sort(Semantics.regions_for("mean")) == [:atl, :lifg]
    end

    test "returns empty list for unknown lemmas" do
      assert Semantics.regions_for("xyz") == []
    end
  end
end
