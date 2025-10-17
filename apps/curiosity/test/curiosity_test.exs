defmodule CuriosityTest do
  use ExUnit.Case
  doctest Curiosity

  test "greets the world" do
    assert Curiosity.hello() == :world
  end
end
