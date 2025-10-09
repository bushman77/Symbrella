defmodule LlmTest do
  use ExUnit.Case
  doctest Llm

  test "greets the world" do
    assert Llm.hello() == :world
  end
end
