defmodule Brain.SelfContinuity do
  @moduledoc """
  Warm-start and persistence-safe restoration for Brain.SelfModel.

  This module is deliberately conservative:

  - missing snapshots degrade explicitly
  - unsupported versions degrade explicitly
  - stale snapshots degrade explicitly
  - invalid snapshots degrade explicitly
  - restored numeric fields are clamped to bounded runtime ranges
  - restore telemetry always includes `count` and `v`

  Durable storage belongs in the `db` app. This module only defines the Brain-owned
  restoration policy boundary.
  """

  alias Brain.SelfModel

  @event [:brain, :self_model, :continuity_restored]
  @snapshot_v 1
  @default_max_age_ms 86_400_000

  @bounded_fields [
    :confidence,
    :uncertainty,
    :stability,
    :vigilance,
    :plasticity,
    :inhibition,
    :cognitive_load
  ]

  @type restore_reason ::
          :restored
          | :missing_snapshot
          | :invalid_snapshot
          | :missing_snapshot_version
          | :unsupported_snapshot_version
          | :stale_snapshot

  @type restore_result ::
          {:ok, SelfModel.t()}
          | {:degraded, SelfModel.t(), restore_reason()}

  def persist(%SelfModel{} = model, opts \\ []) do
    snapshot = to_snapshot(model)
    Db.SelfSnapshots.create_snapshot(snapshot, opts)
  end

  def restore_latest(opts \\ []) do
    case Db.SelfSnapshots.latest_snapshot(opts) do
      {:ok, row} -> restore(row.snapshot, opts)
      {:error, :not_found} -> restore(nil, opts)
    end
  end

  @doc """
  Exports a serializable self-model snapshot.

  This is safe to persist as JSON after normal map/string-key conversion by the
  storage layer.
  """
  @spec to_snapshot(SelfModel.t()) :: map()
  def to_snapshot(%SelfModel{} = model) do
    %{
      v: @snapshot_v,
      confidence: clamp01(model.confidence),
      uncertainty: clamp01(model.uncertainty),
      stability: clamp01(model.stability),
      vigilance: clamp01(model.vigilance),
      plasticity: clamp01(model.plasticity),
      inhibition: clamp01(model.inhibition),
      cognitive_load: clamp01(model.cognitive_load),
      mood: model.mood |> map_or_empty() |> json_safe(),
      active_goals: model.active_goals |> List.wrap() |> json_safe(),
      recent_errors: model.recent_errors |> List.wrap() |> json_safe(),
      recent_actions: model.recent_actions |> List.wrap() |> json_safe(),
      last_appraisal: json_safe(model.last_appraisal),
      last_lifg: json_safe(model.last_lifg),
      self_other_attribution: model.self_other_attribution |> map_or_empty() |> json_safe(),
      continuity: model.continuity |> map_or_empty() |> json_safe(),
      updated_at_ms: model.updated_at_ms,
      snapshot_at_ms: now_ms()
    }
  end

  @doc """
  Restores the latest snapshot into a bounded self-model.

  Returns `{:ok, model}` only for supported, fresh, valid snapshots.
  Returns `{:degraded, model, reason}` for every unsafe restore case.
  """
  @spec restore(map() | SelfModel.t() | nil, keyword()) :: restore_result()
  def restore(snapshot, opts \\ [])

  def restore(nil, opts) do
    degraded(:missing_snapshot, opts)
  end

  def restore(%SelfModel{} = model, opts) do
    model
    |> to_snapshot()
    |> restore(opts)
  end

  def restore(%{} = snapshot, opts) do
    with :ok <- validate_version(snapshot),
         :ok <- validate_freshness(snapshot, opts),
         {:ok, model} <- model_from_snapshot(snapshot) do
      restored =
        model
        |> put_restored_continuity(:restored, snapshot)
        |> Map.put(:updated_at_ms, now_ms())

      emit(:restored, :ok, restored, snapshot)
      {:ok, restored}
    else
      {:error, reason} ->
        degraded(reason, opts, snapshot)
    end
  end

  def restore(_snapshot, opts) do
    degraded(:invalid_snapshot, opts)
  end

  @doc """
  Imports a snapshot without freshness checks.

  Use `restore/2` for runtime warm-start behavior. This function exists so tests
  and storage code can validate snapshot shape independently.
  """
  @spec from_snapshot(map()) :: {:ok, SelfModel.t()} | {:error, restore_reason()}
  def from_snapshot(%{} = snapshot) do
    with :ok <- validate_version(snapshot),
         {:ok, model} <- model_from_snapshot(snapshot) do
      {:ok, model}
    end
  end

  def from_snapshot(_), do: {:error, :invalid_snapshot}

  defp validate_version(snapshot) do
    case get(snapshot, :v) do
      @snapshot_v -> :ok
      nil -> {:error, :missing_snapshot_version}
      _ -> {:error, :unsupported_snapshot_version}
    end
  end

  defp validate_freshness(snapshot, opts) do
    max_age_ms = Keyword.get(opts, :max_age_ms, @default_max_age_ms)

    case get(snapshot, :snapshot_at_ms) || get(snapshot, :updated_at_ms) do
      ts when is_integer(ts) ->
        if now_ms() - ts <= max_age_ms do
          :ok
        else
          {:error, :stale_snapshot}
        end

      _ ->
        {:error, :stale_snapshot}
    end
  end

  defp model_from_snapshot(snapshot) do
    if valid_snapshot_map?(snapshot) do
      {:ok,
       %SelfModel{
         confidence: bounded(snapshot, :confidence, 0.5),
         uncertainty: bounded(snapshot, :uncertainty, 0.5),
         stability: bounded(snapshot, :stability, 0.5),
         vigilance: bounded(snapshot, :vigilance, 0.5),
         plasticity: bounded(snapshot, :plasticity, 0.5),
         inhibition: bounded(snapshot, :inhibition, 0.5),
         cognitive_load: bounded(snapshot, :cognitive_load, 0.0),
         mood: map_field(snapshot, :mood),
         active_goals: list_field(snapshot, :active_goals),
         recent_errors: list_field(snapshot, :recent_errors),
         recent_actions: list_field(snapshot, :recent_actions),
         last_appraisal: get(snapshot, :last_appraisal),
         last_lifg: get(snapshot, :last_lifg),
         self_other_attribution: map_field(snapshot, :self_other_attribution),
         continuity: map_field(snapshot, :continuity),
         updated_at_ms: int_field(snapshot, :updated_at_ms),
         v: @snapshot_v
       }}
    else
      {:error, :invalid_snapshot}
    end
  end

  defp valid_snapshot_map?(snapshot) do
    Enum.all?(@bounded_fields, fn field ->
      value = get(snapshot, field)
      is_nil(value) or is_number(value)
    end)
  end

  defp degraded(reason, _opts, snapshot \\ nil) do
    model =
      %SelfModel{}
      |> put_restored_continuity(reason, snapshot)
      |> Map.put(:updated_at_ms, now_ms())

    emit(reason, :degraded, model, snapshot || %{})
    {:degraded, model, reason}
  end

  defp put_restored_continuity(%SelfModel{} = model, reason, snapshot) do
    continuity =
      model.continuity
      |> map_or_empty()
      |> Map.merge(%{
        reboot_restored?: reason == :restored,
        degraded?: reason != :restored,
        restore_reason: reason,
        restored_at_ms: now_ms(),
        source_snapshot_v: get(snapshot || %{}, :v)
      })

    %{model | continuity: continuity}
  end

  defp emit(reason, status, %SelfModel{} = model, snapshot) do
    :telemetry.execute(
      @event,
      %{
        count: 1,
        restored: if(status == :ok, do: 1, else: 0),
        degraded: if(status == :degraded, do: 1, else: 0)
      },
      %{
        v: @snapshot_v,
        status: status,
        reason: reason,
        self_model_v: model.v,
        snapshot_v: get(snapshot || %{}, :v),
        reboot_restored?: get_in(model.continuity, [:reboot_restored?]) == true
      }
    )
  end

  defp bounded(snapshot, field, default) do
    snapshot
    |> get(field, default)
    |> number(default)
    |> clamp01()
  end

  defp list_field(snapshot, field) do
    case get(snapshot, field, []) do
      value when is_list(value) -> value
      _ -> []
    end
  end

  defp map_field(snapshot, field) do
    case get(snapshot, field, %{}) do
      value when is_map(value) -> value
      _ -> %{}
    end
  end

  defp int_field(snapshot, field) do
    case get(snapshot, field) do
      value when is_integer(value) -> value
      _ -> nil
    end
  end

  defp get(nil, _field), do: nil
  defp get(map, field), do: get(map, field, nil)

  defp get(%{} = map, field, default) when is_atom(field) do
    Map.get(map, field, Map.get(map, Atom.to_string(field), default))
  end

  defp number(value, _default) when is_integer(value), do: value * 1.0
  defp number(value, _default) when is_float(value), do: value
  defp number(_value, default), do: default

  defp clamp01(value) when is_number(value), do: value |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp map_or_empty(value) when is_map(value), do: value
  defp map_or_empty(_), do: %{}

  defp json_safe(%DateTime{} = value), do: DateTime.to_iso8601(value)
  defp json_safe(%NaiveDateTime{} = value), do: NaiveDateTime.to_iso8601(value)
  defp json_safe(%Date{} = value), do: Date.to_iso8601(value)
  defp json_safe(%Time{} = value), do: Time.to_iso8601(value)

  defp json_safe(%_{} = struct) do
    struct
    |> Map.from_struct()
    |> Map.drop([:__meta__])
    |> json_safe()
  end

  defp json_safe(%{} = map) do
    Map.new(map, fn {key, value} -> {json_key(key), json_safe(value)} end)
  end

  defp json_safe(list) when is_list(list), do: Enum.map(list, &json_safe/1)

  defp json_safe(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> json_safe()
  end

  defp json_safe(value)
       when is_binary(value) or is_number(value) or is_boolean(value) or is_nil(value),
       do: value

  defp json_safe(value) when is_atom(value), do: value
  defp json_safe(value), do: inspect(value)

  defp json_key(key) when is_atom(key), do: Atom.to_string(key)
  defp json_key(key) when is_binary(key), do: key
  defp json_key(key) when is_integer(key), do: Integer.to_string(key)
  defp json_key(key), do: inspect(key)

  defp now_ms, do: System.system_time(:millisecond)
end
