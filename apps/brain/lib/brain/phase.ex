defmodule Brain.Phase do
  @moduledoc """
  Zero-coupling facade for **phase-aware gating**.

  Usage:
    if Brain.Phase.allow?(:external), do: do_disambiguation(), else: {:defer, :phase}
    if Brain.Phase.allow?(:internal), do: run_replay(),        else: :skip
  """

  @external_phases [:external]
  @internal_phases [:internal, :integrate]

  @doc "Returns the current phase (starts the clock if needed)."
  def current, do: Brain.CycleClock.phase()

  @doc """
  Allow predicate for roles or exact phases.

  • :external => allowed in [:external]
  • :internal => allowed in [:internal, :integrate]
  • any atom  => allowed only if current phase equals that atom
  """
  def allow?(:external), do: current() in @external_phases
  def allow?(:internal), do: current() in @internal_phases
  def allow?(phase) when is_atom(phase), do: current() == phase
  def allow?(_), do: true
end
