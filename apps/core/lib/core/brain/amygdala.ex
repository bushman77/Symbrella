defmodule Core.Brain.Amygdala do
  @moduledoc """
  Amygdala reaction stage for `Core.SemanticInput`.
  """

  @spec react(map(), keyword()) :: map()
  def react(%{} = si, opts) when is_list(opts) do
    if Code.ensure_loaded?(Brain.Amygdala) and function_exported?(Brain.Amygdala, :react, 2) do
      case Brain.Amygdala.react(si, opts) do
        %{} = emotion -> Map.put(si, :emotion, emotion)
        _ -> si
      end
    else
      si
    end
  end

  def react(si, _opts), do: si
end
