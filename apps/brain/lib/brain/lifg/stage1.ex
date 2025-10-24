defmodule Brain.LIFG.Stage1 do
  @moduledoc """
  LIFG.Stage1 — first-pass disambiguation scoring.

  Now includes:
    • mood nudging
    • semantic context enrichment via `Brain.LIFG.SemanticsAdapter`

  Listens:
    • [:brain, :mood, :update] -> updates mood weights

  Emits:
    • [:brain, :lifg, :stage1, :score]
      measurements: %{score: float()}
      metadata: %{
        token: term(),
        sense_id: term(),
        base_score: float(),
        mood_bias: float(),
        mood_snapshot: map() | nil,
        mood_weights: map(),
        mood_cap: float(),
        v: 2
      }
  """

  use Brain, region: :lifg_stage1
  require Logger
  alias Brain.LIFG.SemanticsAdapter

  @default_mw %{expl: 0.02, inhib: -0.03, vigil: 0.02, plast: 0.00}
  @default_cap 0.05
  @mood_handler_prefix "brain-lifg-stage1-mood-"

  # ─── Public API (compat stubs remain) ───────────────────────────────────────
  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  def run(si, opts) when is_list(opts), do: {:ok, si}
  def run(si, _legacy_arg, _opts),      do: {:ok, si, %{stage1: :noop, passthrough: true}}

  # ─── GenServer Lifecycle ────────────────────────────────────────────────────
  @impl true
  def init(opts) do
    state = %{
      region: :lifg_stage1,
      opts: normalize_opts(opts),
      mood: nil,
      mood_last_ms: nil
    }

    mood_id = unique(@mood_handler_prefix)
    :ok = :telemetry.attach(mood_id, [:brain, :mood, :update], &__MODULE__.on_mood_update/4, %{pid: self()})
    {:ok, Map.put(state, :mood_handler, mood_id)}
  end

  @impl true
  def terminate(_reason, %{mood_handler: mood_id}) when is_binary(mood_id) do
    :telemetry.detach(mood_id)
    :ok
  end
  def terminate(_reason, _state), do: :ok

  # ─── Telemetry handler (this was missing; caused :undef) ────────────────────
  # Forward mood updates into the server mailbox; never crash.
  @doc false
  def on_mood_update(_event, measurements, _meta, %{pid: pid}) when is_pid(pid) do
    send(pid, {:mood_update, measurements})
    :ok
  catch
    _, _ -> :ok
  end
  def on_mood_update(_e, _m, _meta, _config), do: :ok

  @impl true
  def handle_info({:mood_update, meas}, state) do
    mood = %{
      exploration: get_num(meas, :exploration, 0.5) |> clamp01(),
      inhibition:  get_num(meas, :inhibition,  0.5) |> clamp01(),
      vigilance:   get_num(meas, :vigilance,   0.5) |> clamp01(),
      plasticity:  get_num(meas, :plasticity,  0.5) |> clamp01()
    }

    {:noreply, %{state | mood: mood, mood_last_ms: System.system_time(:millisecond)}}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  # ─── Main Scoring ───────────────────────────────────────────────────────────
  def score(server \\ __MODULE__, ctx),
    do: GenServer.call(server, {:score, ctx})

  @impl true
  def handle_call({:score, ctx}, _from, state) do
    enriched_ctx = SemanticsAdapter.adjust_ctx(ctx)

    base = get_num(enriched_ctx, :base_score, 0.0) |> clamp01()
    {factor, bias, mood_snapshot, mw, cap} = mood_factor(state.mood, state.opts)
    final = clamp01(base * factor)

    :telemetry.execute(
      [:brain, :lifg, :stage1, :score],
      %{score: final},
      %{
        token: Map.get(enriched_ctx, :token) || Map.get(enriched_ctx, "token"),
        sense_id: Map.get(enriched_ctx, :sense_id) || Map.get(enriched_ctx, "sense_id"),
        base_score: base,
        mood_bias: bias,
        mood_snapshot: mood_snapshot,
        mood_weights: mw,
        mood_cap: cap,
        v: 2
      }
    )

    {:reply, final, state}
  end

  # ─── Mood Bias Calculation ──────────────────────────────────────────────────
  defp mood_factor(nil, opts),
    do: {1.0, 0.0, nil, cfg_mw(opts), cfg_cap(opts)}

  defp mood_factor(mood, opts) do
    w   = cfg_mw(opts)
    cap = cfg_cap(opts)

    dx = %{
      expl:  (mood.exploration - 0.5),
      inhib: (mood.inhibition  - 0.5),
      vigil: (mood.vigilance   - 0.5),
      plast: (mood.plasticity  - 0.5)
    }

    raw =
      dx.expl  * (w.expl  || 0.0) +
      dx.inhib * (w.inhib || 0.0) +
      dx.vigil * (w.vigil || 0.0) +
      dx.plast * (w.plast || 0.0)

    bias = max(-cap, min(cap, raw))
    factor = 1.0 + bias
    {factor, bias, mood, w, cap}
  end

  # ─── Helpers ────────────────────────────────────────────────────────────────
  defp cfg_mw(opts) do
    val = get_opt(opts, :mood_weights, Application.get_env(:brain, :lifg_mood_weights, @default_mw))
    %{
      expl:  to_small(val[:expl]  || val["expl"]  || @default_mw.expl),
      inhib: to_small(val[:inhib] || val["inhib"] || @default_mw.inhib),
      vigil: to_small(val[:vigil] || val["vigil"] || @default_mw.vigil),
      plast: to_small(val[:plast] || val["plast"] || @default_mw.plast)
    }
  end

  defp cfg_cap(opts),
    do: to_cap(get_opt(opts, :mood_cap, Application.get_env(:brain, :lifg_mood_cap, @default_cap)))

  defp get_opt(opts, key, default) when is_list(opts), do: Keyword.get(opts, key, default)
  defp get_opt(%{} = opts, key, default),           do: Map.get(opts, key, default)
  defp get_opt(_, _, default),                      do: default

  defp to_small(x) when is_number(x), do: x * 1.0
  defp to_small(_), do: 0.0

  defp to_cap(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp to_cap(_), do: @default_cap

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x * 1.0))
  defp clamp01(_), do: 0.0

  defp get_num(map, key, default) do
    case {Map.get(map, key), Map.get(map, to_string(key))} do
      {v, _} when is_integer(v) -> v * 1.0
      {v, _} when is_float(v)   -> v
      {_, v} when is_integer(v) -> v * 1.0
      {_, v} when is_float(v)   -> v
      _                         -> default * 1.0
    end
  end

  defp unique(prefix),
    do:
      prefix <>
        Integer.to_string(:erlang.unique_integer([:positive])) <>
        "-" <> Integer.to_string(System.system_time(:microsecond))

  defp normalize_opts(opts) when is_list(opts), do: if(Keyword.keyword?(opts), do: opts, else: [])
  defp normalize_opts(%{} = opts), do: Map.to_list(opts)
  defp normalize_opts(_), do: []
end

