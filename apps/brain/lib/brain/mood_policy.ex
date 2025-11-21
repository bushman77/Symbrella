defmodule Brain.MoodPolicy do
  @moduledoc """
  Listens to intent telemetry and nudges MoodCore accordingly.
  Started under Symbrella.Application (only), per your rule.

  Emits optional debug telemetry:
    [:brain, :mood, :policy, :bump]  measurements: %{da:, "5ht":, ne:, glu:}
                                     metadata:     %{intent:, confidence:, source:}
  """
  use GenServer

  # Public API ---------------------------------------------------------

  def start_link(opts), do: GenServer.start_link(__MODULE__, opts, name: __MODULE__)

  # GenServer ----------------------------------------------------------

  @impl true
  def init(_opts) do
    # attach once; handler sends this server a message
    id = "mood-policy-#{System.unique_integer([:positive])}"

    :ok =
      :telemetry.attach(id, [:core, :intent, :selected], &__MODULE__.on_intent/4, %{pid: self()})

    state = %{
      telemetry_id: id,
      last_bump_ms: 0,
      min_interval_ms: Application.get_env(:brain, __MODULE__, [])[:min_interval_ms] || 150
    }

    {:ok, state}
  end

  @impl true
  def terminate(_reason, %{telemetry_id: id}), do: :telemetry.detach(id)

  # Telemetry hook (no state here) -> forward to server
  def on_intent(_event, meas, meta, %{pid: pid}) when is_pid(pid),
    do: send(pid, {:intent, meas, meta})

  def on_intent(_, _, _, _), do: :ok

  @impl true
  def handle_info({:intent, meas, meta}, %{last_bump_ms: last, min_interval_ms: min} = st) do
    now = System.monotonic_time(:millisecond)
    if now - last < min, do: {:noreply, st}, else: do_bump(meas, meta, %{st | last_bump_ms: now})
  end

  # Internal -----------------------------------------------------------

  defp do_bump(meas, meta, st) do
    intent = meta[:intent] || meta["intent"] || :unknown
    conf = meas[:confidence] || meas["confidence"] || 0.0
    source = meta[:source] || meta["source"] || :core

    # Map intent → neurotransmitter deltas
    deltas = bump_for(intent, conf)

    # Apply
    _ = Brain.MoodCore.bump(deltas)

    # Optional debug telemetry for the HUD or logs
    :telemetry.execute(
      [:brain, :mood, :policy, :bump],
      deltas,
      %{intent: intent, confidence: conf, source: source}
    )

    {:noreply, st}
  end

  # Tunable mapping. Keep tiny and reversible; MoodCore’s half-life does the rest.
  # Positive raises level; negative lowers. All clamped 0..1 in MoodCore.

  # Hostile / stressful: ↑NE (vigilance), ↓5HT (inhibition), ↓DA/GLU a bit
  defp bump_for(:abuse, conf),
    do: scale(%{"5ht": -0.12, ne: +0.18, da: -0.06, glu: -0.03}, conf)

  defp bump_for(:insult, conf),
    do: scale(%{"5ht": -0.08, ne: +0.12, da: -0.04, glu: -0.02}, conf)

  # Friendly greeting: ↑5HT (calm/connection), slight ↑DA/GLU, ↓NE so we don't look panicky
  defp bump_for(:greet, conf),
    do: scale(%{"5ht": +0.10, ne: -0.06, da: +0.03, glu: +0.02}, conf)

  # Task / curiosity: modest exploration bump
  defp bump_for(:translate, conf),
    do: scale(%{da: +0.02, ne: +0.01}, conf)

  defp bump_for(:ask, conf),
    do: scale(%{da: +0.03, ne: +0.02}, conf)

  # Default: tiny calming / plasticity nudge
  defp bump_for(_other, conf),
    do: scale(%{"5ht": +0.01, glu: +0.01}, conf)

  # Confidence-scaled deltas with global gain from config
  defp scale(map, conf) do
    gain = Application.get_env(:brain, __MODULE__, [])[:gain] || 1.0
    c = normalize_conf(conf)
    for {k, v} <- map, into: %{}, do: {k, v * c * gain}
  end

  defp normalize_conf(nil), do: 0.5
  defp normalize_conf(c) when is_integer(c), do: normalize_conf(c * 1.0)
  defp normalize_conf(c) when is_float(c), do: if(c <= 1.0, do: c, else: c / 100.0)

  defp normalize_conf(c) when is_binary(c) do
    case Float.parse(c) do
      {f, _} -> normalize_conf(f)
      _ -> 0.5
    end
  end

  defp normalize_conf(_), do: 0.5
end
