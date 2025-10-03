defmodule Brain.BoundaryGuardTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.BoundaryGuard

  test "keeps MWEs even if they cross boundaries" do
    sent = "kick-off meeting"
    toks = [%{index: 0, phrase: "kick-off", span: {0, 8}, mw: true}]
    assert [%{phrase: "kick-off"}] = BoundaryGuard.sanitize(toks, sent)
  end

  test "drops tokens not aligned to word boundaries when sentence is present" do
    sent = "hello"
    toks = [%{index: 0, phrase: "he", span: {1, 2}}] # mid-word slice
    assert [] = BoundaryGuard.sanitize(toks, sent)
  end

  test "unicode letters respected" do
    sent = "café noir"
    toks = [%{index: 0, phrase: "café", span: {0, 4}}]
    assert [%{phrase: "café"}] = BoundaryGuard.sanitize(toks, sent)
  end
end

