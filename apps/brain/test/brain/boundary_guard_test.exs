defmodule Brain.LIFG.BoundaryGuardTest do
  use ExUnit.Case, async: true
  alias Brain.LIFG.Stage1.Guard, as: SGuard

  @sent "Kick the ball"

  test "boundary_ok?: true for aligned tokens at start/end boundaries" do
    # "Kick" spans 0..4, next char is space → boundary
    assert SGuard.boundary_ok?(@sent, {0, 4}, false)
    # "ball" ends at string end → boundary
    assert SGuard.boundary_ok?(@sent, {9, 13}, false)
  end

  test "boundary_ok?: false for mid-word unless mw: true" do
    # "ck t" lands mid-word (1..5) → not boundary unless mw=true
    refute SGuard.boundary_ok?(@sent, {1, 5}, false)
    assert SGuard.boundary_ok?(@sent, {1, 5}, true)
  end

  test "boundary_ok?: punctuation right after end still counts as boundary" do
    sent = "Hello, world"
    # "Hello" is 0..5, char at 5 is "," (not letter/number) → boundary
    assert SGuard.boundary_ok?(sent, {0, 5}, false)
  end
end
