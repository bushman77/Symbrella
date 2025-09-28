defmodule Core.Text do
  @moduledoc """
  Minimal text utilities.

  normalize/1:
    - Unicode NFC
    - trim ends
    - collapse all whitespace runs to a single space
    - (case-preserving; add `String.downcase/1` if you want lowercase keys)
  """

  @spec normalize(String.t()) :: String.t()
  def normalize(raw) when is_binary(raw) do
    raw
    |> :unicode.characters_to_nfc_binary()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end
end
