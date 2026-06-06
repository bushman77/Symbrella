defmodule Core.Agency.Command do
  @moduledoc """
  Explicit command contract for agency actions.

  Commands are the boundary between "Symbrella selected an action" and
  "Symbrella is allowed to mutate internal state or the outside world".
  """

  alias Core.Agency.Decision

  @type type ::
          :write_memory
          | :run_self_check
          | :set_goal
          | :complete_goal
          | :observe_environment

  @type t :: %__MODULE__{
          v: pos_integer(),
          id: String.t(),
          type: type(),
          source: atom(),
          reason: atom() | String.t() | nil,
          risk: atom(),
          requires_permission?: boolean(),
          payload: map(),
          decision_trace_id: String.t() | nil,
          status: atom()
        }

  defstruct v: 1,
            id: nil,
            type: nil,
            source: :action_selection,
            reason: nil,
            risk: :low,
            requires_permission?: false,
            payload: %{},
            decision_trace_id: nil,
            status: :proposed

  @spec from_decision(Decision.t() | map()) :: [t()]
  def from_decision(%Decision{} = decision), do: decision |> Decision.to_map() |> from_decision()

  def from_decision(%{} = decision) do
    selected = map_get(decision, :selected_action)

    case selected do
      :store_memory ->
        [
          new(:write_memory, decision,
            reason: first_reason(decision, :selected_store_memory),
            risk: :medium,
            requires_permission?: true,
            payload: %{
              intent: map_get(decision, :intent),
              expected_outcome: map_get(decision, :expected_outcome, %{})
            }
          )
        ]

      :self_check ->
        [
          new(:run_self_check, decision,
            reason: first_reason(decision, :selected_self_check),
            risk: :low,
            requires_permission?: false,
            payload: %{self_state: map_get(decision, :self_state, %{})}
          )
        ]

      _ ->
        []
    end
  end

  def from_decision(_), do: []

  @spec new(type(), Decision.t() | map(), keyword()) :: t()
  def new(type, decision, opts \\ []) when is_atom(type) and is_list(opts) do
    decision = Decision.to_map(decision)

    %__MODULE__{
      id: command_id(type),
      type: type,
      source: Keyword.get(opts, :source, :action_selection),
      reason: Keyword.get(opts, :reason),
      risk: Keyword.get(opts, :risk, map_get(decision, :risk, :low)),
      requires_permission?: Keyword.get(opts, :requires_permission?, false),
      payload: Keyword.get(opts, :payload, %{}),
      decision_trace_id: map_get(decision, :trace_id),
      status: :proposed
    }
  end

  @spec to_map(t() | map()) :: map()
  def to_map(%__MODULE__{} = command), do: Map.from_struct(command)
  def to_map(%{} = command), do: command
  def to_map(_), do: %{}

  defp first_reason(decision, default) do
    decision
    |> map_get(:reasons, [])
    |> List.wrap()
    |> List.first()
    |> case do
      nil -> default
      reason -> reason
    end
  end

  defp command_id(type) do
    unique = System.unique_integer([:positive, :monotonic])
    "agency|command|#{type}|#{unique}"
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(%_struct{} = struct, key, default),
    do: struct |> Map.from_struct() |> map_get(key, default)

  defp map_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default
end
