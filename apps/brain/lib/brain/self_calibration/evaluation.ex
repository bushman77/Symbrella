defmodule Brain.SelfCalibration.Evaluation do
  @moduledoc """
  Evaluation result for advisory self-calibration predictions.

  Evaluation compares predicted bounded self-state fields against observed
  labels. It is diagnostic only; it does not alter Brain.SelfModel.
  """

  @type t :: %__MODULE__{
          confidence_error: float(),
          uncertainty_error: float(),
          stability_error: float(),
          mae: float(),
          source: atom(),
          model_version: String.t() | nil,
          feature_schema_v: pos_integer(),
          meta: map(),
          v: pos_integer()
        }

  defstruct confidence_error: 0.0,
            uncertainty_error: 0.0,
            stability_error: 0.0,
            mae: 0.0,
            source: :baseline,
            model_version: nil,
            feature_schema_v: 1,
            meta: %{},
            v: 1
end
