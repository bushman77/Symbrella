defmodule Brain.ACC do
  @moduledoc """
  Anterior Cingulate Cortex (conflict monitoring).
  Tracks uncertainty/margin; could drive control gain and learning signals.
  """
  use Brain.Region, region: :acc
end

