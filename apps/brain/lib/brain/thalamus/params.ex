defmodule Brain.Thalamus.Params do
  @moduledoc false

  defstruct ofc_weight: 1.0, acc_alpha: 0.0, min_score: 0.0

  @type t :: %__MODULE__{
          ofc_weight: number(),
          acc_alpha: number(),
          min_score: number()
        }

  @doc "Update params from a keyword list or map."
  @spec set_params(t(), keyword() | map()) :: t()
  def set_params(p \\ %__MODULE__{}, kv)

  def set_params(%__MODULE__{} = p, kv) when is_list(kv),
    do: set_params(p, Map.new(kv))

  def set_params(%__MODULE__{} = p, %{} = m) do
    %__MODULE__{
      ofc_weight: clamp(Map.get(m, :ofc_weight, p.ofc_weight)),
      acc_alpha: clamp(Map.get(m, :acc_alpha, p.acc_alpha)),
      min_score: clamp(Map.get(m, :min_score, p.min_score))
    }
  end

  # keep it simple; inputs are expected numeric already
  defp clamp(v) when is_integer(v), do: (v / 1) |> clamp()
  defp clamp(v) when is_float(v), do: v |> max(0.0) |> min(1.0)
  defp clamp(_), do: 0.0
end
