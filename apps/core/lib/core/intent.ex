defmodule Core.Intent do
  @moduledoc """
  Canonical intent vocabulary for Symbrella.

  This module is the single source of truth for which intents are valid.
  Other modules (e.g., normalizers, UI, classifiers) should call `allowed/0`
  or `valid?/1` rather than hard-coding the list.
  """

  @allowed_intents ~w(
    ask
    tell
    affirm
    deny
    greet
    bye
    meta
    why
    help
    debug
  )a

  @doc "Return the whitelist of allowed intents as atoms."
  @spec allowed() :: [atom()]
  def allowed, do: @allowed_intents

  @doc "True if the given atom is a valid, allowed intent."
  @spec valid?(term()) :: boolean()
  def valid?(intent) when is_atom(intent), do: intent in @allowed_intents
  def valid?(_), do: false
end

