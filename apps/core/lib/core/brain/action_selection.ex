defmodule Core.Brain.ActionSelection do
  @moduledoc """
  Core-side adapter for Brain.ActionSelector.

  Brain owns the pure action-selection policy. Core owns pipeline orchestration
  and attaches the result to SemanticInput.
  """

  alias Core.Pipeline.Trace
  alias Core.Agency.Command
  alias Core.Agency.Decision
  alias Core.Agency.Executor

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    if Code.ensure_loaded?(Brain.ActionSelector) and
         function_exported?(Brain.ActionSelector, :select, 2) do
      ctx = context_from_si(si)

      result =
        ctx
        |> Brain.ActionSelector.select(opts)
        |> normalize_result()

      selected = Map.get(result, :selected)
      candidates = Map.get(result, :candidates, [])
      confidence = Map.get(result, :confidence, 0.0)
      decision = Decision.from_action_result(result, ctx)
      commands = Command.from_decision(decision)
      command_results = maybe_execute_commands(commands, opts)

      si
      |> Map.put(:selected_action, selected)
      |> Map.put(:action_candidates, candidates)
      |> Map.put(:action_meta, result)
      |> Map.put(:agency_decision, decision)
      |> Map.put(:agency_commands, commands)
      |> Map.put(:agency_command_results, command_results)
      |> Trace.append(:action_selection,
        decision: selected || :none,
        reason: selected_reason(result),
        scores: %{confidence: confidence},
        meta: %{
          trace_id: decision.trace_id,
          selected: selected,
          candidates: summarize_candidates(candidates),
          safety_gate: Map.get(result, :safety_gate),
          version: Map.get(result, :version),
          risk: decision.risk,
          permission: decision.permission,
          commands: summarize_commands(commands),
          command_results: summarize_command_results(command_results)
        }
      )
    else
      si
    end
  end

  def attach(si, _opts), do: si

  defp context_from_si(%{} = si) do
    %{
      intent: si_get(si, :intent),
      confidence: si_get(si, :confidence),
      keyword: si_get(si, :keyword),
      text: si_get(si, :sentence) || si_get(si, :text) || si_get(si, :keyword),
      symbolic_frame: si_get(si, :symbolic_frame),
      mood: si_get(si, :mood),
      self_model: si_get(si, :self_model),
      prefrontal: si_get(si, :prefrontal),
      control_signals: si_get(si, :control_signals),
      evidence: si_get(si, :evidence),
      comprehension: si_get(si, :comprehension),
      lifg_choices: si_get(si, :lifg_choices),
      acc_conflict: si_get(si, :acc_conflict),
      response_meta: si_get(si, :response_meta),
      self_monitor: si_get(si, :self_monitor),
      session_id: si_get(si, :session_id)
    }
  end

  defp normalize_result(%{} = result) do
    %{
      version: Map.get(result, :version, "action_selector.v1"),
      selected: Map.get(result, :selected),
      selected_candidate: Map.get(result, :selected_candidate),
      candidates: result |> Map.get(:candidates, []) |> List.wrap(),
      confidence: result |> Map.get(:confidence, 0.0) |> number() |> clamp01(),
      safety_gate: Map.get(result, :safety_gate),
      safety: Map.get(result, :safety, %{})
    }
  end

  defp normalize_result(_), do: %{selected: nil, candidates: [], confidence: 0.0}

  defp selected_reason(%{selected_candidate: %{reason: reason}}) when is_atom(reason), do: reason
  defp selected_reason(_), do: :action_selection

  defp summarize_candidates(candidates) when is_list(candidates) do
    Enum.map(candidates, fn
      %{} = candidate ->
        %{
          action: Map.get(candidate, :action),
          score: Map.get(candidate, :score),
          reason: Map.get(candidate, :reason)
        }

      other ->
        %{action: other, score: 0.0, reason: :unknown_candidate}
    end)
  end

  defp summarize_candidates(_), do: []

  defp summarize_commands(commands) when is_list(commands) do
    Enum.map(commands, fn command ->
      command = Command.to_map(command)

      %{
        id: Map.get(command, :id),
        type: Map.get(command, :type),
        risk: Map.get(command, :risk),
        requires_permission?: Map.get(command, :requires_permission?)
      }
    end)
  end

  defp summarize_commands(_), do: []

  defp summarize_command_results(results) when is_list(results) do
    Enum.map(results, fn
      %{} = result ->
        Map.take(result, [:command_id, :type, :status, :reason])

      other ->
        %{status: :unknown, reason: inspect(other)}
    end)
  end

  defp summarize_command_results(_), do: []

  defp maybe_execute_commands(commands, opts) do
    if agency_opt(opts, :execute_agency_commands?, :agency_execute_commands?, false) do
      Executor.execute_all(commands,
        permission?: agency_opt(opts, :agency_permission?, :agency_require_permission?, false),
        record?: agency_opt(opts, :record_agency_commands?, :agency_record_commands?, true)
      )
    else
      []
    end
  end

  defp agency_opt(opts, opt_key, env_key, default) do
    Keyword.get(opts, opt_key, Application.get_env(:core, env_key, default))
  end

  defp si_get(map, key, default \\ nil)

  defp si_get(%{} = map, key, default) when is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp si_get(_map, _key, default), do: default

  defp number(value) when is_integer(value), do: value * 1.0
  defp number(value) when is_float(value), do: value
  defp number(_), do: 0.0

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0
end
