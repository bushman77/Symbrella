defmodule Brain.SelfCalibration.Sample do
  @moduledoc """
  Versioned training/evaluation sample for future self-state calibration.

  This is not the ML model. It is the stable data contract that turns runtime
  evidence into features and labels.
  """
  @type t :: %__MODULE__{
          features: map(),
          labels: map(),
          raw: map(),
          source: :runtime | :test | :reviewed | :synthetic,
          meta: map(),
          v: pos_integer()
        }

  defstruct features: %{},
            labels: %{},
            raw: %{},
            source: :runtime,
            meta: %{},
            v: 1
end
