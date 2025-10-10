defmodule Brain.ACC do
  @moduledoc """
  **Anterior Cingulate Cortex (ACC) — conflict monitoring.**

  Computes a scalar **conflict** score from LIFG choices and flags **needy**
  items (low margin / low p_top1 / has alternatives). Emits telemetry and keeps
  a small rolling window for status.

  ### Telemetry
  - `[:brain, :acc, :conflict]`
    - **measurements:** `%{conflict, needy, n}`
    - **metadata:** `%{tau_m, p_min, weights}`

  ### Config (in `:brain`)
  - `:acc_tau_margin` (default `0.20`) — margin threshold for “confident”
  - `:acc_p_min` (default `0.65`) — minimum p(top1) to avoid “needy”
  - `:acc_weights` (default `%{margin: 0.40, p_top1: 0.30, entropy: 0.20, alts: 0.10}`)
  - `:acc_window_keep` (default `50`) — rolling window length
  """

  use Brain, region: :acc
  require Logger
  alias Brain.Utils.Safe

  @type choice :: map()
  @type si :: map()

  # ---- Public API -----------------------------------------------------

  @doc """
  Assess conflict for current choices.

  Returns:
    `{:ok, %{si, conflict: float, needy: [choice()], audit: map()}}`
  """
  @spec assess(si(), [choice()], keyword()) ::
          {:ok, %{si: si(), conflict: float(), needy: [choice()], audit: map()}}
  def assess(si, choices, opts \\ [])
      when is_map(si) and is_list(choices) and is_list(opts) do
    t0 = System.monotonic_time()

    tau_m =
      Keyword.get(opts, :tau_confident, Application.get_env(:brain, :acc_tau_margin, 0.20))

    p_min =
      Keyword.get(opts, :p_min, Application.get_env(:brain, :acc_p_min, 0.65))

    wts_env =
      Application.get_env(:brain, :acc_weights, %{
        margin: 0.40,
        p_top1: 0.30,
        entropy: 0.20,
        alts: 0.10
      })

    wts_opt = Map.new(Keyword.get(opts, :weights, []))
    w = Map.merge(wts_env, wts_opt)

    # Per-choice features
    feats =
      Enum.map(choices, fn ch ->
        ch = Safe.to_plain(ch)
        scores = Safe.get(ch, :scores, %{}) || %{}
        p1 = scores |> Map.values() |> Enum.max(fn -> 0.0 end)
        m = (Safe.get(ch, :margin, 0.0) || 0.0) * 1.0
        k = max(map_size(scores), 1)

        # entropy normalized to 0..1 when k > 1
        h_raw = entropy(scores)
        h_den = :math.log(max(k, 2))
        h = if h_den > 0.0, do: h_raw / h_den, else: 0.0

        altc = Safe.get(ch, :alt_ids, []) |> length()

        %{
          p1: p1,
          margin: m,
          entropy: if(k > 1, do: h, else: 0.0),
          alts: if(altc > 0, do: 1.0, else: 0.0),
          token_index: Safe.get(ch, :token_index, 0)
        }
      end)

    # Conflict per item (higher = worse)
    per_item =
      Enum.map(feats, fn f ->
        m_norm = 1.0 - min(f.margin / max(tau_m, 1.0e-9), 1.0)
        p_norm = 1.0 - f.p1
        e_norm = clamp01(f.entropy)
        a_norm = clamp01(f.alts)

        w.margin * m_norm +
          w.p_top1 * p_norm +
          w.entropy * e_norm +
          w.alts * a_norm
      end)

    # Aggregate conflict
    conflict =
      case per_item do
        [] -> 0.0
        xs -> Enum.sum(xs) / length(xs)
      end

    # Needy = items that would trigger pMTG gate
    needy =
      Enum.zip(choices, feats)
      |> Enum.filter(fn {ch, f} ->
        m = Safe.get(ch, :margin, 0.0) || 0.0
        alts? = (Safe.get(ch, :alt_ids, []) || []) != []
        m < tau_m or f.p1 < p_min or alts?
      end)
      |> Enum.map(fn {ch, _} -> ch end)

    # Telemetry + state
    :telemetry.execute(
      [:brain, :acc, :conflict],
      %{conflict: conflict, needy: length(needy), n: length(choices)},
      %{tau_m: tau_m, p_min: p_min, weights: w}
    )

    timing_ms =
      System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

    audit = %{
      stage: :acc,
      n: length(choices),
      needy: length(needy),
      conflict: conflict,
      timing_ms: timing_ms,
      tau_m: tau_m,
      p_min: p_min,
      weights: w
    }

    {:ok,
     %{
       si:
         push_trace(si, %{
           stage: :acc,
           conflict: conflict,
           needy: length(needy)
         }),
       conflict: conflict,
       needy: needy,
       audit: audit
     }}
  end

  @doc "Server status (rolling window + last assessment)."
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  # ---- GenServer ------------------------------------------------------

  @impl true
  def init(opts) do
    keep =
      Keyword.get(opts, :window_keep, Application.get_env(:brain, :acc_window_keep, 50))

    {:ok,
     %{
       region: :acc,
       window_keep: keep,
       window: [],
       last: nil,
       opts: Map.new(opts)
     }}
  end

  @impl true
  def handle_call(:status, _from, state), do: {:reply, state, state}

  @impl true
  def handle_cast({:record, payload}, state) do
    ts = System.system_time(:millisecond)
    win = [{ts, payload} | state.window] |> Enum.take(state.window_keep)
    {:noreply, %{state | window: win, last: payload}}
  end

  # ---- Internals ------------------------------------------------------

  defp push_trace(si, ev) do
    si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
    GenServer.cast(__MODULE__, {:record, %{ev: ev, tokens: Map.get(si2, :tokens)}})
    si2
  end

  defp entropy(%{} = scores) do
    ps = scores |> Map.values() |> Enum.filter(&(&1 > 0.0))
    Enum.reduce(ps, 0.0, fn p, acc -> acc - p * :math.log(p) end)
  end

  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x))
end
