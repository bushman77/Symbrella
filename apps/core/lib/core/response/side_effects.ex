defmodule Core.Response.SideEffects do
  @moduledoc """
  Boundary side effects used by `Core.Response`.

  Response planning decides policy. This module performs optional process,
  telemetry, memory, turn-history, and durable-ledger effects behind named APIs.
  """

  alias Core.Response.LlmSynthesis

  @compile {:no_warn_undefined, Brain.Hippocampus}
  @compile {:no_warn_undefined, Brain.MoodCore}

  @spec apply_mood_intent(atom(), number(), boolean()) :: :ok
  def apply_mood_intent(intent, confidence, read_only?)
      when is_atom(intent) and is_number(confidence) do
    if not read_only? and Code.ensure_loaded?(Brain.MoodCore) and
         is_pid(Process.whereis(Brain.MoodCore)) and
         function_exported?(Brain.MoodCore, :apply_intent, 2) do
      _ = Brain.MoodCore.apply_intent(intent, confidence)
    end

    :ok
  end

  def apply_mood_intent(_intent, _confidence, _read_only?), do: :ok

  @spec record_turn(term(), String.t(), String.t()) :: :ok
  def record_turn(session_id, user_text, assistant_text) do
    if function_exported?(LlmSynthesis, :record_turn, 3) do
      _ = LlmSynthesis.record_turn(session_id, user_text, assistant_text)
    end

    :ok
  end

  @spec emit_plan(map()) :: :ok
  def emit_plan(meta) when is_map(meta) do
    :telemetry.execute([:core, :response, :plan], %{}, meta)
    :ok
  end

  def emit_plan(_), do: :ok

  @spec emit_mode_selected(map()) :: :ok
  def emit_mode_selected(meta) when is_map(meta) do
    self_state = Map.get(meta, :self_state, %{})

    :telemetry.execute(
      [:brain, :response, :mode_selected],
      %{count: 1},
      %{
        v: 1,
        mode: Map.get(meta, :mode),
        tone: Map.get(meta, :tone),
        action: Map.get(meta, :action),
        confidence: Map.get(meta, :confidence),
        self_model_v: map_get(self_state, :v),
        focus: map_get(self_state, :focus),
        effects: Map.get(meta, :self_state_effects, [])
      }
    )

    :ok
  end

  def emit_mode_selected(_), do: :ok

  @spec record_agency_response(String.t(), String.t(), map(), map()) :: :ok
  def record_agency_response(user_text, assistant_text, features, meta)
      when is_map(features) and is_map(meta) do
    _ = Core.Response.AgencyLedger.record_response(user_text, assistant_text, features, meta)
    :ok
  end

  def record_agency_response(_user_text, _assistant_text, _features, _meta), do: :ok

  @spec persist_user_name(String.t(), String.t()) :: :ok
  def persist_user_name(name, raw_text) when is_binary(name) do
    if Code.ensure_loaded?(Brain.Hippocampus) and
         function_exported?(Brain.Hippocampus, :encode, 2) do
      slate = %{
        sentence: raw_text,
        winners: [%{lemma: name}],
        tokens: [name],
        tags: ["fact", "user_name"]
      }

      meta = %{
        tags: ["fact", "user_name"],
        scope: :chat,
        kind: :fact,
        key: :user_name,
        value: name
      }

      _ = Brain.Hippocampus.encode(slate, meta)
    end

    :ok
  end

  def persist_user_name(_name, _raw_text), do: :ok

  @spec persist_user_fact(String.t(), String.t(), String.t(), String.t()) :: :ok
  def persist_user_fact(key, label, value, raw_text)
      when is_binary(key) and is_binary(label) and is_binary(value) do
    if Code.ensure_loaded?(Brain.Hippocampus) and
         function_exported?(Brain.Hippocampus, :encode, 2) do
      tokens = key |> String.split("_", trim: true) |> Enum.uniq()

      slate = %{
        sentence: raw_text,
        winners: [%{id: "#{key}|fact|0", lemma: key, norm: key}],
        tokens: Enum.uniq([key, value | tokens]),
        tags: ["fact", "user_fact", key]
      }

      meta = %{
        tags: ["fact", "user_fact", key],
        scope: :chat,
        subject: :user,
        kind: :fact,
        key: key,
        label: label,
        value: value
      }

      _ = Brain.Hippocampus.encode(slate, meta)
    end

    :ok
  end

  def persist_user_fact(_key, _label, _value, _raw_text), do: :ok

  @spec recalled_user_name(String.t() | nil) :: String.t() | nil
  def recalled_user_name(extracted_name) do
    cond do
      is_binary(extracted_name) and extracted_name != "" ->
        extracted_name

      hippocampus_fact_available?() ->
        case Brain.Hippocampus.fact(:user_name) do
          value when is_binary(value) and value != "" -> value
          _ -> nil
        end

      true ->
        nil
    end
  end

  @spec recalled_fact(String.t()) :: String.t() | nil
  def recalled_fact(key) when is_binary(key) do
    if hippocampus_fact_available?() do
      case Brain.Hippocampus.fact(key) do
        value when is_binary(value) and value != "" -> value
        _ -> nil
      end
    end
  end

  def recalled_fact(_), do: nil

  defp hippocampus_fact_available? do
    Code.ensure_loaded?(Brain.Hippocampus) and function_exported?(Brain.Hippocampus, :fact, 1)
  end

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
