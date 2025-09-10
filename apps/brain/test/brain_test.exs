defmodule BrainTest do
  use ExUnit.Case
  doctest Brain

  test "greets the world" do
    assert Brain.hello() == :world
  end
end
