defmodule Core.Intent.Types do
  @moduledoc """
  Canonical intent atoms and helpers.
  """

  @type intent ::
          :greet
          | :define
          | :translate
          | :ask_info
          | :ask
          | :question
          | :smalltalk
          | :code
          | :brain_introspect
          | :affirm
          | :deny
          | :bye
          | :command
          | :correction
          | :debug
          | :feedback
          | :health_support
          | :environment_observation
          | :help
          | :memory_write
          | :statement
          | :tell
          | :none

  @intents [
    :greet,
    :define,
    :translate,
    :ask_info,
    :ask,
    :question,
    :smalltalk,
    :code,
    :brain_introspect,
    :affirm,
    :deny,
    :bye,
    :command,
    :correction,
    :debug,
    :feedback,
    :health_support,
    :environment_observation,
    :help,
    :memory_write,
    :statement,
    :tell,
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
  def normalize("ask"), do: :ask
  def normalize("question"), do: :question
  def normalize("smalltalk"), do: :smalltalk
  def normalize("ask_info"), do: :ask_info
  def normalize("code"), do: :code
  def normalize("brain_introspect"), do: :brain_introspect
  def normalize("affirm"), do: :affirm
  def normalize("yes"), do: :affirm
  def normalize("deny"), do: :deny
  def normalize("no"), do: :deny
  def normalize("bye"), do: :bye
  def normalize("goodbye"), do: :bye
  def normalize("command"), do: :command
  def normalize("correction"), do: :correction
  def normalize("debug"), do: :debug
  def normalize("feedback"), do: :feedback
  def normalize("health_support"), do: :health_support
  def normalize("health support"), do: :health_support
  def normalize("environment_observation"), do: :environment_observation
  def normalize("environment observation"), do: :environment_observation
  def normalize("help"), do: :help
  def normalize("memory_write"), do: :memory_write
  def normalize("statement"), do: :statement
  def normalize("tell"), do: :tell
  def normalize(_), do: :none
end
