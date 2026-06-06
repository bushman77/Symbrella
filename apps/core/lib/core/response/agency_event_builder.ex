defmodule Core.Response.AgencyEventBuilder do
  @moduledoc """
  Pure builders for durable self-agency ledger event attributes.

  This module shapes response and command data into maps suitable for the
  `Db.AgencyEvents` boundary. It performs no persistence.
  """

  @max_text_chars 1_600
  @max_list_items 20

  @spec response_attrs(String.t(), String.t(), map(), map()) :: map()
  def response_attrs(user_text, assistant_text, features, meta)
      when is_map(features) and is_map(meta) do
    reflection =
      Core.Response.AgencyReflection.from_response(user_text, assistant_text, features, meta)

    %{
      session_id: safe_string(Map.get(meta, :session_id), "global"),
      actor: "symbrella",
      source: "core_response",
      status: "observed",
      action: safe_string(Map.get(meta, :action), "respond"),
      input: %{
        text: clamp_text(user_text),
        intent: safe_json(Map.get(meta, :intent_inferred)),
        confidence: safe_json(Map.get(meta, :confidence)),
        risk_bucket: safe_json(Map.get(meta, :risk_bucket))
      },
      decision: %{
        policy_version: safe_json(Map.get(meta, :policy_version)),
        mode: safe_json(Map.get(meta, :mode)),
        tone: safe_json(Map.get(meta, :tone)),
        action: safe_json(Map.get(meta, :action)),
        agency_decision: safe_json(Map.get(meta, :agency_decision)),
        agency_commands: safe_json(Map.get(meta, :agency_commands, [])),
        agency_command_results: safe_json(Map.get(meta, :agency_command_results, [])),
        response_source: safe_json(Map.get(meta, :response_source)),
        chosen_skill: safe_json(Map.get(meta, :chosen_skill)),
        guardrail?: safe_json(Map.get(meta, :guardrail?))
      },
      reasons: %{
        explanation: safe_json(Map.get(meta, :explanation)),
        skill_reason: safe_json(Map.get(meta, :skill_reason)),
        overrides: safe_json(Map.get(meta, :overrides)),
        scores: safe_json(Map.get(meta, :scores)),
        fallback_reason: safe_json(Map.get(meta, :response_fallback_reason)),
        agency_memory: safe_json(Map.get(features, :agency_memory, %{}))
      },
      self_model: safe_json(Map.get(features, :self_model, %{})),
      self_state: safe_json(Map.get(meta, :self_state, %{})),
      outcome: %{
        assistant_text: clamp_text(assistant_text),
        assistant_chars: String.length(to_string(assistant_text || "")),
        curiosity_probe: safe_json(Map.get(meta, :curiosity_probe)),
        self_state_effects: safe_json(Map.get(meta, :self_state_effects, []))
      },
      reflection: safe_json(reflection)
    }
  end

  @spec command_attrs(map() | struct(), map(), keyword()) :: map()
  def command_attrs(command, result, opts) when is_map(result) and is_list(opts) do
    command = safe_json(command)

    %{
      session_id: safe_string(Keyword.get(opts, :session_id), "global"),
      actor: "symbrella",
      source: "agency_executor",
      status: safe_string(Map.get(result, :status), "observed"),
      action: safe_string(Map.get(command, "type"), "command"),
      input: %{
        command: command
      },
      decision: %{
        command_id: safe_json(Map.get(command, "id")),
        decision_trace_id: safe_json(Map.get(command, "decision_trace_id")),
        risk: safe_json(Map.get(command, "risk")),
        requires_permission?: safe_json(Map.get(command, "requires_permission?"))
      },
      reasons: %{
        command_reason: safe_json(Map.get(command, "reason")),
        result_reason: safe_json(Map.get(result, :reason))
      },
      self_model: %{},
      self_state: %{},
      outcome: safe_json(result),
      reflection: %{
        signals: command_reflection_signals(result),
        next_time_adjustment: command_next_time_adjustment(result)
      }
    }
  end

  defp safe_string(value, default) when is_binary(value) do
    value = String.trim(value)
    if value == "", do: default, else: value
  end

  defp safe_string(value, _default) when is_atom(value), do: Atom.to_string(value)
  defp safe_string(value, _default) when is_integer(value), do: Integer.to_string(value)
  defp safe_string(_value, default), do: default

  defp clamp_text(text) do
    text = text |> to_string() |> String.trim()

    if String.length(text) <= @max_text_chars do
      text
    else
      String.slice(text, 0, @max_text_chars) <> "..."
    end
  end

  defp safe_json(%_struct{} = struct), do: struct |> Map.from_struct() |> safe_json()

  defp safe_json(%{} = map) do
    map
    |> Enum.map(fn {key, value} -> {safe_key(key), safe_json(value)} end)
    |> Map.new()
  end

  defp safe_json(list) when is_list(list) do
    list
    |> Enum.take(@max_list_items)
    |> Enum.map(&safe_json/1)
  end

  defp safe_json(nil), do: nil
  defp safe_json(value) when is_boolean(value), do: value
  defp safe_json(value) when is_binary(value), do: value
  defp safe_json(value) when is_integer(value), do: value
  defp safe_json(value) when is_float(value), do: value
  defp safe_json(value) when is_atom(value), do: Atom.to_string(value)
  defp safe_json(value), do: inspect(value)

  defp safe_key(key) when is_atom(key), do: Atom.to_string(key)
  defp safe_key(key) when is_binary(key), do: key
  defp safe_key(key), do: inspect(key)

  defp command_reflection_signals(%{status: :deferred}), do: [:defer_action]
  defp command_reflection_signals(%{status: :rejected}), do: [:reject_unknown_or_unsafe_action]
  defp command_reflection_signals(%{status: :failed}), do: [:repair]
  defp command_reflection_signals(%{status: :executed}), do: [:command_executed]
  defp command_reflection_signals(_), do: []

  defp command_next_time_adjustment(%{status: :deferred}), do: :request_permission
  defp command_next_time_adjustment(%{status: :rejected}), do: :reduce_scope
  defp command_next_time_adjustment(%{status: :failed}), do: :prefer_repair
  defp command_next_time_adjustment(_), do: :none
end
