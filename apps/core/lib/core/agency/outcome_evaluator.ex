defmodule Core.Agency.OutcomeEvaluator do
  @moduledoc """
  Evidence-backed, advisory evaluation of an agency decision's actual outcome.

  Command execution is not proof that an intended outcome occurred. The first
  supported outcome is :self_state_reviewed: it requires a matching executed
  self-check command AND an independent read of the live Brain self-model.

  The observation is deliberately limited to bounded diagnostics. No learned
  policy, confidence, goal completion, or memory authority is changed here.
  """

  alias Core.Agency.Decision

  @version 1

  @spec evaluate(Decision.t() | map(), [map()], [map()], keyword()) :: map()
  def evaluate(decision, commands, results, opts \\ [])

  def evaluate(decision, commands, results, opts)
      when is_map(decision) and is_list(commands) and is_list(results) and is_list(opts) do
    decision = Decision.to_map(decision)
    trace_id = map_get(decision, :trace_id)
    expected_kind = decision |> map_get(:expected_outcome, %{}) |> map_get(:kind)

    {status, reason, observation} =
      cond do
        map_get(decision, :selected_action) != :self_check or
            expected_kind != :self_state_reviewed ->
          {:unverified, :unsupported_outcome, %{}}

        not is_binary(trace_id) or trace_id == "" ->
          {:unverified, :missing_decision_trace, %{}}

        true ->
          verify_self_check(trace_id, commands, results, opts)
      end

    %{
      v: @version,
      decision_trace_id: trace_id,
      expected_outcome: %{kind: expected_kind},
      status: status,
      reason: reason,
      observation: observation,
      verified?: status == :verified,
      next_time_adjustment: adjustment(status)
    }
  end

  def evaluate(_decision, _commands, _results, _opts) do
    %{
      v: @version,
      decision_trace_id: nil,
      expected_outcome: %{},
      status: :unverified,
      reason: :invalid_input,
      observation: %{},
      verified?: false,
      next_time_adjustment: :seek_independent_evidence
    }
  end

  @doc """
  Create a bounded reflection from the decision trace and verified receipt.

  An unverified outcome never appears under what_worked, and mere execution
  cannot produce a positive learning signal.
  """
  @spec reflection(Decision.t() | map(), map()) :: map()
  def reflection(decision, evaluation) when is_map(decision) and is_map(evaluation) do
    decision = Decision.to_map(decision)
    status = map_get(evaluation, :status)
    reason = map_get(evaluation, :reason)

    %{
      v: @version,
      decision_trace_id: map_get(evaluation, :decision_trace_id),
      what_i_did: map_get(decision, :selected_action),
      why_i_did_it: decision |> map_get(:reasons, []) |> List.wrap() |> Enum.take(8),
      what_worked: if(status == :verified, do: [:live_self_state_observed], else: []),
      what_failed: if(status == :failed, do: [reason], else: []),
      outcome_status: status,
      verification_reason: reason,
      next_time_adjustment: map_get(evaluation, :next_time_adjustment),
      verified?: status == :verified
    }
  end

  def reflection(_decision, _evaluation), do: %{}

  defp verify_self_check(trace_id, commands, results, opts) do
    command =
      Enum.find(commands, fn command ->
        is_map(command) and
          map_get(command, :type) == :run_self_check and
          map_get(command, :decision_trace_id) == trace_id and
          is_binary(map_get(command, :id)) and
          map_get(command, :id) != ""
      end)

    case command do
      nil ->
        {:unverified, :missing_matching_command, %{}}

      _ ->
        command_id = map_get(command, :id)

        result =
          Enum.find(results, fn result ->
            is_map(result) and map_get(result, :command_id) == command_id and
              map_get(result, :type) == :run_self_check
          end)

        evaluate_execution(result, opts)
    end
  end

  defp evaluate_execution(nil, _opts), do: {:unverified, :missing_execution_receipt, %{}}

  defp evaluate_execution(result, opts) do
    case map_get(result, :status) do
      :executed ->
        # Do not trust result.outcome.self_state: the existing executor merely
        # copies that field from the selected command's payload.
        verify_live_snapshot(opts)

      :failed ->
        {:failed, :command_failed, %{}}

      :rejected ->
        {:failed, :command_rejected, %{}}

      :deferred ->
        {:unverified, :command_deferred, %{}}

      _ ->
        {:unverified, :unknown_execution_status, %{}}
    end
  end

  defp verify_live_snapshot(opts) do
    reader = Keyword.get(opts, :snapshot_reader, &Brain.Introspection.snapshot/0)

    if is_function(reader, 0) do
      try do
        case reader.() do
          %Brain.SelfModel{} = model ->
            if valid_snapshot?(model) do
              {:verified, :live_self_state_observed,
               %{
                 source: :brain_introspection,
                 self_model_v: model.v,
                 observed_at_ms: model.updated_at_ms,
                 confidence: model.confidence,
                 uncertainty: model.uncertainty,
                 cognitive_load: model.cognitive_load
               }}
            else
              {:unverified, :invalid_live_snapshot, %{}}
            end

          _ ->
            {:unverified, :invalid_live_snapshot, %{}}
        end
      rescue
        _ -> {:unverified, :snapshot_unavailable, %{}}
      catch
        :exit, _ -> {:unverified, :snapshot_unavailable, %{}}
        :throw, _ -> {:unverified, :snapshot_unavailable, %{}}
      end
    else
      {:unverified, :invalid_snapshot_reader, %{}}
    end
  end

  defp valid_snapshot?(%Brain.SelfModel{} = model) do
    model.v == 1 and is_integer(model.updated_at_ms) and model.updated_at_ms > 0 and
      Enum.all?(
        [model.confidence, model.uncertainty, model.stability, model.cognitive_load],
        fn value -> is_number(value) and value >= 0.0 and value <= 1.0 end
      )
  end

  defp adjustment(:verified), do: :maintain_current_strategy
  defp adjustment(:failed), do: :diagnose_action_failure
  defp adjustment(_), do: :seek_independent_evidence

  defp map_get(%_struct{} = struct, key, default),
    do: struct |> Map.from_struct() |> map_get(key, default)

  defp map_get(%{} = map, key, default) when is_atom(key),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key), default))

  defp map_get(_map, _key, default), do: default
  defp map_get(map, key), do: map_get(map, key, nil)
end
