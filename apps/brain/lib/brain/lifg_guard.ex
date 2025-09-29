defmodule Brain.LIFG.Guard do
  @moduledoc ~S"""
  Hard guard for the LIFG path: ensure no character n-grams reach LIFG.

  Policy:
  - Upstream should tokenize in word mode with `emit_chargrams: false`.
  - As a safety net, drop any token whose `phrase` contains whitespace and is not marked `mw: true`.
    These are almost always character n-grams or boundary substrings that shouldn't be disambiguated.
  """

  require Logger

  @spec sanitize([map()]) :: [map()]
  def sanitize(tokens) when is_list(tokens) do
    Enum.reject(tokens, &is_chargram?/1)
  end

  @doc """
  A token counts as a char-gram if its :phrase contains whitespace and it's not a multiword (`mw: true`).
  Treats nil/false as "not multiword".
  """
  @spec is_chargram?(map()) :: boolean()
  def is_chargram?(%{phrase: phrase} = t) when is_binary(phrase) do
    # Coerce to boolean so `not` never sees nil
    mw? = Map.get(t, :mw, false) == true
    # Any unicode whitespace flags as spaced
    spaced? = Regex.match?(~r/\s/u, phrase)
    spaced? and not mw?
  end

  def is_chargram?(_), do: false
end
