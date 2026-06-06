defmodule Core.Agency.Decision do
  @moduledoc """
  Canonical agency decision shape for Core-owned pipeline handoffs.

  Action selection can keep returning simple maps for compatibility, but Core
  should pass this struct between response planning, command building, ledger
  recording, reflection, and memory pressure.
  """

  @type t :: %__MODULE__{
          v: pos_integer(),
          trace_id: String.t(),
          intent: atom() | nil,
          selected_action: atom() | nil,
          candidates: [map()],
          reasons: [atom() | String.t()],
          self_state: map(),
          confidence: float(),
          uncertainty: float(),
          risk: atom(),
          permission: map(),
          expected_outcome: map(),
          actual_outcome: map() | nil,
          reflection: map() | nil,
          source: atom(),
          selector: map()
        }

  defstruct v: 1,
            trace_id: nil,
            intent: nil,
            selected_action: nil,
            candidates: [],
            reasons: [],
            self_state: %{},
            confidence: 0.0,
            uncertainty: 0.0,
            risk: :low,
            permission: %{required?: false, status: :not_required},
            expected_outcome: %{},
            actual_outcome: nil,
            reflection: nil,
            source: :action_selection,
            selector: %{}

  @spec from_action_result(map(), map()) :: t()
  def from_action_result(result, ctx \\ %{}) when is_map(result) and is_map(ctx) do
    selected = map_get(result, :selected)
    selected_candidate = map_get(result, :selected_candidate, %{})
    safety = map_get(result, :safety, %{})
    self_model = map_get(ctx, :self_model, %{})
    self_monitor = map_get(ctx, :self_monitor, %{})
    confidence = result |> map_get(:confidence, 0.0) |> number() |> clamp01()

    %__MODULE__{
      trace_id: trace_id(ctx),
      intent: map_get(ctx, :intent),
      selected_action: selected,
      candidates: result |> map_get(:candidates, []) |> List.wrap(),
      reasons: reasons(selected_candidate, safety, self_monitor),
      self_state: self_state(self_model, self_monitor),
      confidence: confidence,
      uncertainty: uncertainty(self_model, confidence),
      risk: risk(selected, safety),
      permission: permission(selected, safety),
      expected_outcome: expected_outcome(selected),
      source: :action_selection,
      selector: %{
        version: map_get(result, :version),
        safety_gate: map_get(result, :safety_gate),
        safety: safety,
        selected_candidate: selected_candidate
      }
    }
  end

  @spec to_map(t() | map()) :: map()
  def to_map(%__MODULE__{} = decision), do: Map.from_struct(decision)
  def to_map(%{} = decision), do: decision
  def to_map(_), do: %{}

  defp reasons(selected_candidate, safety, self_monitor) do
    [
      map_get(selected_candidate, :reason),
      map_get(safety, :reason)
      | List.wrap(map_get(self_monitor, :recovery_suggestions, []))
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  defp self_state(self_model, self_monitor) do
    %{
      confidence: map_get(self_model, :confidence),
      uncertainty: map_get(self_model, :uncertainty),
      focus: map_get(self_model, :focus),
      monitor_status: map_get(self_monitor, :status),
      recovery_suggestions: List.wrap(map_get(self_monitor, :recovery_suggestions, []))
    }
    |> Enum.reject(fn {_key, value} -> is_nil(value) or value == [] end)
    |> Map.new()
  end

  defp uncertainty(self_model, confidence) do
    case map_get(self_model, :uncertainty) do
      value when is_number(value) -> clamp01(value)
      _ -> clamp01(1.0 - confidence)
    end
  end

  defp risk(:refuse_or_redirect, _safety), do: :high
  defp risk(:safe_support, _safety), do: :medium
  defp risk(:store_memory, _safety), do: :medium
  defp risk(_action, %{decision: :redirected}), do: :high
  defp risk(_action, _safety), do: :low

  defp permission(:store_memory, _safety), do: %{required?: true, status: :required}
  defp permission(_action, %{decision: :redirected}), do: %{required?: false, status: :blocked}
  defp permission(_action, _safety), do: %{required?: false, status: :not_required}

  defp expected_outcome(:ask_clarifying_question), do: %{kind: :user_clarifies}
  defp expected_outcome(:store_memory), do: %{kind: :durable_memory_requested}
  defp expected_outcome(:self_check), do: %{kind: :self_state_reviewed}
  defp expected_outcome(:safe_support), do: %{kind: :supportive_response}
  defp expected_outcome(:refuse_or_redirect), do: %{kind: :unsafe_request_redirected}
  defp expected_outcome(:observe_silently), do: %{kind: :no_external_action}
  defp expected_outcome(_), do: %{kind: :response_delivered}

  defp trace_id(ctx) do
    session = map_get(ctx, :session_id, "global")
    unique = System.unique_integer([:positive, :monotonic])
    "agency|#{session}|#{unique}"
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(%_struct{} = struct, key, default),
    do: struct |> Map.from_struct() |> map_get(key, default)

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
