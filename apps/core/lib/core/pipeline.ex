defmodule Core.Pipeline do
  @moduledoc """
  Namespace for Core semantic pipeline stages.

  Stage modules under this namespace should own local semantic transforms,
  not top-level orchestration and not Brain runtime integration.

  Examples:
  - `Core.Pipeline.LTM`
  - `Core.Pipeline.Candidates`
  - `Core.Pipeline.Evidence`
  """
end
