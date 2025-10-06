defmodule Brain.NoSelfCallsTest do
  use ExUnit.Case, async: true

  test "no GenServer self-calls from handlers" do
    candidates = [
      Path.expand("../../lib/brain.ex", __ENV__.file),
      Path.expand("../../../lib/brain.ex", __ENV__.file),
      Path.join([File.cwd!(), "apps/brain/lib/brain.ex"])
    ]

    brain_path =
      Enum.find(candidates, &File.exists?/1) ||
        flunk("Could not locate brain.ex in any of: #{inspect(candidates)}")

    src = File.read!(brain_path)

    refute src =~ ~r/def handle_(call|cast|info)\(.*?\) do.*?GenServer\.call\(@name/s,
           "Found GenServer.call(@name, ...) inside a handler"

    refute src =~ ~r/def handle_(call|cast|info)\(.*?\) do.*?GenServer\.cast\(@name/s,
           "Found GenServer.cast(@name, ...) inside a handler"
  end
end

