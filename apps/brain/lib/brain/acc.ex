# apps/brain/lib/brain/acc.ex
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

  # ───────────────────────────── Public API ─────────────────────────────

  @doc """
  Assess conflict for a set of LIFG `choices`.

  Returns:
    `{:ok, %{si, conflict: float, needy: [choice()], audit: map()}}`

  Notes:
  * `p_top1` is derived from, in order: `ch.probs[chosen_id]`, `ch.p_top1`, then
    `max(ch.scores)` as a last resort.
  * Entropy is computed over available **probabilities** when present; falls back
    to normalized entropy over scores.
  """
  @spec assess(si(), [choice()], keyword()) ::
          {:ok, %{si: si(), conflict: float(), needy: [choice()], audit: map()}}
  def assess(si, choices, opts \\ [])
      when is_map(si) and is_list(choices) and is_list(opts) do
    t0 = System.monotonic_time()

    cfg = load_cfg(opts)

    feats = Enum.map(choices, &features_for_choice/1)

    conflict =
      feats
      |> Enum.map(&conflict_component(&1, cfg))
      |> average()
      |> clamp01()

    needy =
      Enum.zip(choices, feats)
      |> Enum.filter(fn {ch, f} -> needy?(ch, f, cfg) end)
      |> Enum.map(fn {ch, _} -> ch end)

    :telemetry.execute(
      [:brain, :acc, :conflict],
      %{conflict: conflict, needy: length(needy), n: length(choices)},
      %{tau_m: cfg.tau_m, p_min: cfg.p_min, weights: cfg.weights}
    )

    audit = %{
      stage: :acc,
      n: length(choices),
      needy: length(needy),
      conflict: conflict,
      timing_ms: elapsed_ms_since(t0),
      tau_m: cfg.tau_m,
      p_min: cfg.p_min,
      weights: cfg.weights
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

  @doc "Server status (rolling window + last assessment payload)."
  @spec status(pid() | module()) :: map()
  def status(server \\ __MODULE__), do: GenServer.call(server, :status)

  # ───────────────────────────── GenServer ─────────────────────────────

  @impl true
  def init(opts) do
    keep = Keyword.get(opts, :window_keep, Application.get_env(:brain, :acc_window_keep, 50))

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

  # ───────────────────────────── Internals ─────────────────────────────

  # ---- Config ----

  @doc false
  defp load_cfg(opts) do
    tau_m =
      Keyword.get(opts, :tau_confident, Application.get_env(:brain, :acc_tau_margin, 0.20)) * 1.0

    p_min =
      Keyword.get(opts, :p_min, Application.get_env(:brain, :acc_p_min, 0.65)) * 1.0

    weights_env =
      Application.get_env(:brain, :acc_weights, %{
        margin: 0.40,
        p_top1: 0.30,
        entropy: 0.20,
        alts: 0.10
      })

    weights = Map.merge(weights_env, Map.new(Keyword.get(opts, :weights, [])))

    %{tau_m: tau_m, p_min: p_min, weights: weights}
  end

  # ---- Trace & window ----

  @doc false
  defp push_trace(si, ev) do
    si2 = Map.update(si, :trace, [ev], fn tr -> [ev | tr] end)
    GenServer.cast(__MODULE__, {:record, %{ev: ev, tokens: Map.get(si2, :tokens)}})
    si2
  end

  # ---- Conflict math ----

  @doc false
  defp conflict_component(%{margin: m, p1: p1, entropy: h, alts: a}, %{tau_m: tau, weights: w}) do
    m_norm = 1.0 - min(safe_div(m, max(tau, 1.0e-9)), 1.0)
    p_norm = 1.0 - p1
    e_norm = clamp01(h)
    a_norm = clamp01(a)

    (Map.get(w, :margin, 0.0) * m_norm) +
      (Map.get(w, :p_top1, 0.0) * p_norm) +
      (Map.get(w, :entropy, 0.0) * e_norm) +
      (Map.get(w, :alts, 0.0) * a_norm)
  end

  @doc false
  defp needy?(ch, f, %{tau_m: tau_m, p_min: p_min}) do
    m = Safe.get(ch, :margin, 0.0) || 0.0
    alts? = (Safe.get(ch, :alt_ids, []) || []) != []

    (is_number(m) and m < tau_m) or (is_number(f.p1) and f.p1 < p_min) or alts?
  end

  # ---- Feature extraction ----

  @doc false
  @spec features_for_choice(choice()) :: %{
          p1: float(),
          margin: float(),
          entropy: float(),
          alts: float(),
          token_index: non_neg_integer()
        }
  defp features_for_choice(ch0) do
    ch = Safe.to_plain(ch0)

    probs = as_map(Safe.get(ch, :probs, %{}))
    scores = as_map(Safe.get(ch, :scores, %{}))
    chosen_id = Safe.get(ch, :chosen_id, nil)

    p1 =
      cond do
        is_binary(chosen_id) and is_map(probs) and is_number(Map.get(probs, chosen_id)) ->
          Map.get(probs, chosen_id) * 1.0

        is_number(Safe.get(ch, :p_top1, nil)) ->
          Safe.get(ch, :p_top1, 0.0) * 1.0

        true ->
          score_max =
            scores
            |> Map.values()
            |> Enum.max(fn -> 0.0 end)

          score_max * 1.0
      end
      |> clamp01()

    # Prefer entropy of probs (if present), else of normalized scores
    {dist, k} =
      if map_size(probs) > 0 do
        {probs, map_size(probs)}
      else
        nd = normalize_dist(scores)
        {nd, map_size(nd)}
      end

    h_raw = entropy(dist)
    h_den = :math.log(max(k, 2))
    entropy01 = if h_den > 0.0, do: h_raw / h_den, else: 0.0

    altc = Safe.get(ch, :alt_ids, []) |> List.wrap() |> length()
    margin = (Safe.get(ch, :margin, 0.0) || 0.0) * 1.0

    %{
      p1: clamp01(p1),
      margin: max(0.0, margin),
      entropy: clamp01(entropy01),
      alts: if(altc > 0, do: 1.0, else: 0.0),
      token_index:
        case Safe.get(ch, :token_index, 0) do
          i when is_integer(i) and i >= 0 -> i
          _ -> 0
        end
    }
  end

  # ---- Math helpers ----

  @doc false
  @spec entropy(%{optional(any()) => number()}) :: float()
  defp entropy(%{} = dist) do
    dist
    |> Map.values()
    |> Enum.filter(&is_number/1)
    |> Enum.filter(&(&1 > 0.0))
    |> Enum.reduce(0.0, fn p, acc -> acc - p * :math.log(p) end)
  end

  @doc false
  defp average([]), do: 0.0
  defp average(xs) when is_list(xs), do: Enum.sum(xs) / length(xs)

  @doc false
  # OTP 27+: handle both +0.0 and -0.0 without literal pattern-matching.
  defp safe_div(_a, b) when is_number(b) and (b == 0.0 or b == -0.0), do: 0.0
  defp safe_div(a, b) when is_number(a) and is_number(b), do: a / b

  @doc false
  defp clamp01(x) when is_number(x), do: max(0.0, min(1.0, x) * 1.0)
  defp clamp01(_), do: 0.0

  @doc false
  defp elapsed_ms_since(t0),
    do: System.convert_time_unit(System.monotonic_time() - t0, :native, :millisecond)

  # ---- Normalization helpers ----

  @doc false
  defp as_map(%{} = m), do: m
  defp as_map(_), do: %{}

  @doc false
  defp normalize_dist(%{} = m) do
    vals =
      m
      |> Map.values()
      |> Enum.filter(&is_number/1)
      |> Enum.map(&max(0.0, &1))

    s = Enum.sum(vals)

    if s <= 0.0 do
      %{}
    else
      Enum.reduce(m, %{}, fn {k, v}, acc ->
        if is_number(v) and v > 0.0, do: Map.put(acc, k, v / s * 1.0), else: acc
      end)
    end
  end
end

