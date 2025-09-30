defmodule Brain.Chain do
  @moduledoc """
  Fan-out of post-LIFG signals to brain regions.
  """

  @regions [Brain.ATL, Brain.PMTG, Brain.AG, Brain.MTL, Brain.ACC, Brain.BG]

  @doc "Broadcast a stimulus (usually the SI after LIFG) to all regions."
  def fanout(:post_lifg, si) do
    Enum.each(@regions, fn mod ->
      if pid = mod.whereis() do
        GenServer.cast(pid, {:stimulus, si})
      end
    end)

    :ok
  end

  @doc "Quick state dump of all regions."
  def snapshot() do
    for mod <- @regions, into: %{} do
      name = mod |> Module.split() |> List.last() |> String.to_atom()
      state =
        case mod.whereis() do
          nil -> :not_running
          _pid -> mod.snapshot()
        end

      {name, state}
    end
  end
end

