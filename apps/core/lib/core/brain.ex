defmodule Core.Brain do
  @moduledoc "Core-side facade to avoid depending on the :brain app."

  @spec whereis(String.t()) :: pid() | nil
  def whereis(_id), do: nil

  def active_cells(phrase), do: GenServer.call(Brain, :active_cells, phrase)

  def stm(si), do: GenServer.call(Brain, {:stm, si})

  def ltm(si), do: GenServer.call(Db, {:ltm, si})
end

