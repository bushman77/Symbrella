defmodule Core.Intent.Types do
  @moduledoc """
  Canonical intent atoms and helpers.
  """

  @type intent ::
          :greet
          | :define
          | :translate
          | :ask_info
          | :code
          | :brain_introspect
          | :none

  @intents [
    :greet,
    :define,
    :translate,
    :ask_info,
    :code,
    :brain_introspect,
    :none
  ]

  @doc "All supported intent atoms."
  @spec known() :: [intent()]
  def known, do: @intents

  @doc """
  Normalize arbitrary intent-ish input to a canonical atom.
  Unknowns map to :none.
  """
  @spec normalize(term()) :: intent()
  def normalize(x) when is_atom(x) and x in @intents, do: x
  def normalize("greet"), do: :greet
  def normalize("define"), do: :define
  def normalize("translation" <> _), do: :translate
  def normalize("translate"), do: :translate
  def normalize("ask_info"), do: :ask_info
  def normalize("code"), do: :code
  def normalize("brain_introspect"), do: :brain_introspect
  def normalize(_), do: :none
end

