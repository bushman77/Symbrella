defmodule Core.Brain do
  @moduledoc "Core-side facade to avoid depending on the :brain app."

  @spec whereis(String.t()) :: pid() | nil
  def whereis(_id), do: nil
end

