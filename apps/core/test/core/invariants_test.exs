defmodule Core.InvariantsTest do
  use ExUnit.Case, async: true
  alias Core.Invariants
  alias Brain.LIFG.{Guard, BoundaryGuard}
  alias Core.MWE.Injector

  test "no char-grams + boundary-only (with allowed MWE)" do
    sentence = "kick the bucket"

    tokens = [
      %{phrase: "kick"},
      %{phrase: "the"},
      %{phrase: "bucket"},
      # should be dropped by Guard
      %{phrase: "ck t"},
      # allowed MWE
      %{phrase: "kick the bucket", mw: true}
    ]

    clean =
      tokens
      |> Guard.sanitize()
      |> BoundaryGuard.sanitize(sentence)

    assert :ok == Invariants.assert_no_chargrams!(clean)
    assert :ok == Invariants.assert_boundary_only_or_mwe!(clean, sentence)
  end

  test "spans sorted after injection (words first, then MWEs)" do
    # base tokens carry spans {i, i+1}
    base = [
      %{phrase: "new", span: {0, 1}},
      %{phrase: "york", span: {1, 2}},
      %{phrase: "city", span: {2, 3}},
      %{phrase: "mayor", span: {3, 4}}
    ]

    repo = MapSet.new(["new york", "new york city"])
    out = Injector.inject(base, exists?: fn p -> MapSet.member?(repo, p) end, max_n: 4)

    # words first (0..3), then MWEs starting at 0: {0,2} then {0,3}
    assert :ok == Invariants.assert_sorted_spans!(out)
  end

  test "boundary invariant fails for substring token" do
    sentence = "bank by the river"
    # substring inside 'bank'
    bad = [%{phrase: "ank"}]

    assert_raise ArgumentError, fn ->
      Invariants.assert_boundary_only_or_mwe!(bad, sentence)
    end
  end
end
