defmodule Brain.Telemetry do
  @moduledoc """
  Attach lightweight telemetry handlers for Brain events.

  Logs high-signal cognition pipeline events when enabled:

      SYMBRELLA_TRACE=1 mix phx.server

  `LOG_COGNITION_PIPELINE=1` is kept as a compatible alias.

      [:brain, :pipeline, :lifg_stage1, :stop]
      [:brain, :thalamus, :curiosity, :decision]
      [:brain, :dlpfc, :heard]
      [:brain, :basal_ganglia, :gate]
      [:brain, :drive_loop, :idle_status]
      [:brain, :drive_loop, :impulse]
      [:core, :agency, :autonomy, :transition]

  Use `attach!/0` once at boot. Safe to call multiple times.
  """

  require Logger

  @lifg_stop_event [:brain, :pipeline, :lifg_stage1, :stop]
  @legacy_handler_id "brain-lifg-stage1-logger"
  @handler_id "brain-cognition-console-logger"

  @events [
    [:core, :intent, :selected],
    [:brain, :intent, :selected],
    @lifg_stop_event,
    [:brain, :pmtg, :consult],
    [:brain, :thalamus, :curiosity, :decision],
    [:brain, :dlpfc, :heard],
    [:brain, :basal_ganglia, :gate],
    [:brain, :wm, :update],
    [:core, :response, :prompt],
    [:core, :response, :complete],
    [:brain, :drive_loop, :idle_status],
    [:brain, :drive_loop, :impulse],
    [:core, :agency, :autonomy, :transition]
  ]

  @doc """
  Attach a logger handler for the LIFG Stage-1 stop event.
  Safe to call multiple times (subsequent calls are ignored).
  """
  def attach!() do
    _ = :telemetry.detach(@legacy_handler_id)

    case :telemetry.attach_many(@handler_id, @events, &__MODULE__.handle/4, nil) do
      :ok -> :ok
      {:error, :already_exists} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Detach the logger handler. No-op if not attached.
  """
  def detach!() do
    case :telemetry.detach(@handler_id) do
      :ok -> :ok
      {:error, :not_found} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  def handle(event, measurements, metadata, _config) do
    cond do
      event == @lifg_stop_event and lifg_stage1_logs?() ->
        log_lifg_stop(measurements, metadata)

      cognition_pipeline_logs?() ->
        log_cognition_event(event, measurements, metadata)

      true ->
        :ok
    end
  end

  @doc false
  def handle_lifg_stop(event, measurements, metadata, config) do
    handle(event, measurements, metadata, config)
  end

  defp log_cognition_event([:core, :intent, :selected], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] intent selected source=core label=#{inspect(map_get(metadata, :label))} " <>
        "confidence=#{fmt_number(map_get(measurements, :confidence))} " <>
        "text=#{preview(map_get(metadata, :text))}" <>
        raw_text_suffix(metadata) <>
        fuzzy_suffix(metadata)
    end)
  end

  defp log_cognition_event([:brain, :intent, :selected], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] intent mirrored source=brain label=#{inspect(map_get(metadata, :label))} " <>
        "confidence=#{fmt_number(map_get(measurements, :confidence))}" <>
        raw_text_suffix(metadata) <>
        fuzzy_suffix(metadata)
    end)
  end

  defp log_cognition_event([:brain, :pipeline, :lifg_stage1, :stop], measurements, metadata) do
    log_lifg_stop(measurements, metadata)
  end

  defp log_cognition_event([:brain, :pmtg, :consult], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] pMTG consult mode=#{inspect(map_get(metadata, :mode))} " <>
        "needy=#{inspect(map_get(measurements, :needy))} " <>
        "threshold=#{inspect(map_get(metadata, :thr))}"
    end)
  end

  defp log_cognition_event([:brain, :thalamus, :curiosity, :decision], measurements, metadata) do
    probe = map_get(metadata, :probe, %{})

    Logger.info(fn ->
      "[Cog] thalamus decision=#{inspect(map_get(metadata, :decision))} " <>
        "score=#{fmt_number(map_get(measurements, :score))} " <>
        "probe_score=#{fmt_number(map_get(probe, :score))} " <>
        "source=#{inspect(map_get(metadata, :source))} " <>
        "probe=#{probe_preview(probe)} " <>
        "ofc=#{applied_number(metadata, :ofc_blended?, :ofc_value)} " <>
        "acc=#{applied_number(metadata, :acc_applied?, :acc_conflict)} " <>
        "mood=#{applied_number(metadata, :mood_applied?, :mood_factor)}"
    end)
  end

  defp log_cognition_event([:brain, :dlpfc, :heard], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] dlpfc heard decision=#{inspect(map_get(metadata, :decision))} " <>
        "score=#{fmt_number(map_get(measurements, :score))} " <>
        "will_act=#{inspect(map_get(metadata, :will_act))} " <>
        "has_probe=#{inspect(map_get(metadata, :has_probe))}"
    end)
  end

  defp log_cognition_event([:brain, :basal_ganglia, :gate], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] basal_ganglia decision=#{inspect(map_get(metadata, :decision))} " <>
        "score=#{fmt_number(map_get(measurements, :score))} " <>
        "bias=#{fmt_number(map_get(measurements, :self_state_bias))} " <>
        "source=#{inspect(map_get(metadata, :source))} " <>
        "id=#{preview(map_get(metadata, :id))} " <>
        "token=#{inspect(map_get(metadata, :token_index))} " <>
        "self_state=#{inspect(map_get(metadata, :self_state_applied?))}"
    end)
  end

  defp log_cognition_event([:brain, :wm, :update], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] WM update size=#{inspect(map_get(measurements, :size))} " <>
        "added=#{inspect(map_get(measurements, :added))} " <>
        "removed=#{inspect(map_get(measurements, :removed))} " <>
        "capacity=#{inspect(map_get(measurements, :capacity))} " <>
        "reason=#{inspect(map_get(metadata, :reason))}"
    end)
  end

  defp log_cognition_event([:core, :response, :prompt], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] response prompt intent=#{inspect(map_get(metadata, :intent))} " <>
        "mode=#{inspect(map_get(metadata, :mode))} " <>
        "user_chars=#{inspect(map_get(measurements, :user_chars))} " <>
        "system_chars=#{inspect(map_get(measurements, :system_chars))}"
    end)
  end

  defp log_cognition_event([:core, :response, :complete], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] response complete intent=#{inspect(map_get(metadata, :intent))} " <>
        "assistant_chars=#{inspect(map_get(measurements, :assistant_chars))} " <>
        "repair=#{inspect(map_get(map_get(metadata, :reflection, %{}), :status))}"
    end)
  end

  defp log_cognition_event([:brain, :drive_loop, :idle_status], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] idle current=#{inspect(map_get(metadata, :current, :idle))} " <>
        "idle_ms=#{inspect(map_get(measurements, :idle_ms))} " <>
        "threshold_ms=#{inspect(map_get(metadata, :threshold_ms))}"
    end)
  end

  defp log_cognition_event([:brain, :drive_loop, :impulse], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] drive impulse reason=#{inspect(map_get(metadata, :reason))} " <>
        "idle_ms=#{inspect(map_get(measurements, :idle_ms))} " <>
        "salience=#{fmt_number(map_get(measurements, :salience_score))} " <>
        "trace=#{inspect(map_get(metadata, :trace_id))}"
    end)
  end

  defp log_cognition_event([:core, :agency, :autonomy, :transition], measurements, metadata) do
    Logger.info(fn ->
      "[Cog] autonomy step=#{inspect(map_get(metadata, :step))} " <>
        "count=#{inspect(map_get(measurements, :count))} " <>
        "trace=#{inspect(map_get(metadata, :trace_id))}"
    end)
  end

  defp log_cognition_event(event, measurements, metadata) do
    Logger.info(fn ->
      "[Cog] event=#{inspect(event)} meas=#{inspect(measurements)} meta=#{inspect(metadata)}"
    end)
  end

  defp log_lifg_stop(measurements, metadata) do
    duration_ms = map_get(measurements, :duration_ms, 0)

    winners = map_get(metadata, :winners)
    boosts = map_get(metadata, :boosts)
    inhibitions = map_get(metadata, :inhibitions)
    weak = map_get(measurements, :weak)
    missing = map_get(measurements, :missing)
    low_confidence = map_get(measurements, :low_confidence)
    intent = map_get(metadata, :intent)
    confidence = map_get(metadata, :confidence)

    # Optional/legacy fields (log if present)
    groups = map_get(metadata, :groups)
    ctx_dim = map_get(metadata, :ctx_dim)
    norm = map_get(metadata, :normalize)
    scores = map_get(metadata, :scores_mode) || map_get(metadata, :scores)
    parallel = map_get(metadata, :parallel)

    Logger.info(fn ->
      base =
        "[LIFG] #{duration_ms}ms" <>
          maybe_int(" winners", winners) <>
          maybe_int(" boosts", boosts) <>
          maybe_int(" inhibitions", inhibitions) <>
          maybe_int(" weak", weak) <>
          maybe_int(" missing", missing) <>
          maybe_int(" low_conf", low_confidence) <>
          if(is_nil(intent), do: "", else: " intent=#{inspect(intent)}") <>
          if(is_nil(confidence), do: "", else: " confidence=#{fmt_number(confidence)}")

      extras =
        " groups=#{inspect(groups)} ctx_dim=#{inspect(ctx_dim)} " <>
          "norm=#{inspect(norm)} scores=#{inspect(scores)} parallel=#{inspect(parallel)}"

      base <> extras
    end)
  end

  defp lifg_stage1_logs? do
    enabled?(Application.get_env(:brain, :log_lifg_stage1?, false)) or cognition_pipeline_logs?()
  end

  defp cognition_pipeline_logs? do
    enabled?(System.get_env("SYMBRELLA_TRACE")) or
      enabled?(Application.get_env(:brain, :log_cognition_pipeline?, false))
  end

  defp enabled?(value) when is_binary(value) do
    String.downcase(value) in ["true", "1", "yes", "on"]
  end

  defp enabled?(value), do: value in [true, 1, :on]

  defp maybe_int(label, value) when is_integer(value), do: "#{label}=#{value}"
  defp maybe_int(_label, _value), do: ""

  defp fmt_number(value) when is_float(value), do: :erlang.float_to_binary(value, decimals: 3)
  defp fmt_number(value) when is_integer(value), do: Integer.to_string(value)
  defp fmt_number(value), do: inspect(value)

  defp applied_number(metadata, flag_key, value_key) do
    if map_get(metadata, flag_key, false) do
      fmt_number(map_get(metadata, value_key))
    else
      "skip"
    end
  end

  defp probe_preview(%{} = probe) do
    label =
      map_get(probe, :text) ||
        map_get(probe, :sentence) ||
        map_get(probe, :lemma) ||
        map_get(probe, :id) ||
        probe

    preview(label)
  end

  defp probe_preview(probe), do: preview(probe)

  defp raw_text_suffix(metadata) do
    raw = map_get(metadata, :sentence)
    text = map_get(metadata, :text)

    if is_binary(raw) and raw != "" and raw != text do
      " raw=#{preview(raw)}"
    else
      ""
    end
  end

  defp fuzzy_suffix(metadata) do
    corrections = map_get(metadata, :fuzzy_corrections, [])
    aliases = map_get(metadata, :fuzzy_aliases, [])

    cond do
      is_list(corrections) and corrections != [] ->
        " fuzzy=#{preview_fuzzy_corrections(corrections)}"

      is_list(aliases) and aliases != [] ->
        " aliases=#{preview(aliases)}"

      true ->
        ""
    end
  end

  defp preview_fuzzy_corrections(corrections) do
    corrections
    |> Enum.take(3)
    |> Enum.map(fn
      %{original: original, replacement: replacement, reason: reason} ->
        "#{original}->#{replacement}(#{reason})"

      %{"original" => original, "replacement" => replacement, "reason" => reason} ->
        "#{original}->#{replacement}(#{reason})"

      other ->
        inspect(other)
    end)
    |> Enum.join(",")
    |> preview()
  end

  defp preview(value) when is_binary(value) do
    compact = value |> String.replace(~r/\s+/, " ") |> String.trim()

    if String.length(compact) > 90 do
      inspect(String.slice(compact, 0, 90) <> "...")
    else
      inspect(compact)
    end
  end

  defp preview(value), do: inspect(value)

  defp map_get(map, key, default \\ nil)

  defp map_get(%{} = map, key, default) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_map, _key, default), do: default

  # helper for other telemetry sites (kept for compatibility)
  def meta(extra \\ %{}) when is_map(extra) do
    Map.merge(%{v: 3}, extra)
  end
end
