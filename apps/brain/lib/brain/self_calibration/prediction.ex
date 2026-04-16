defmodule Brain.SelfCalibration.Prediction do
  @moduledoc """
  Advisory calibration output for bounded self-state fields.

  Prediction is advisory. It does not overwrite Brain.SelfModel unless an
  explicit blending policy accepts it.
  """

  @type t :: %__MODULE__{
          confidence: float(),
          uncertainty: float(),
          stability: float(),
          source: atom(),
          model_version: String.t() | nil,
          feature_schema_v: pos_integer(),
          meta: map(),
          v: pos_integer()
        }

  defstruct confidence: 0.5,
            uncertainty: 0.5,
            stability: 0.5,
            source: :baseline,
            model_version: nil,
            feature_schema_v: 1,
            meta: %{},
            v: 1
end
