defmodule Brain.PFC do
  @moduledoc """
  Prefrontal Cortex (controller) — policy for semantic selection.

  Role:
    • Read current intent + WM load + Meta signals and emit POLICY for LIFG/PMTG:
      - weights override (gentle)
      - margin_threshold (tighter when WM is saturated; looser when low confidence)
      - when/how to consult PMTG (retrieval boost on needy tokens / low confidence / surprise)
      - ACC conflict tau (when to escalate)
      - delta model tuning (boost/inhib scaling from margins)
      - optional gate_into_wm policy
      - lifg_min_score (raise bar when conflict high)

  Usage:
    Brain.PFC.set_goal(%{intent: :ask, keyword: "weather", confidence: 0.72})
    policy = Brain.PFC.policy(si)   # returns keyword list to merge into LIFG opts

  Notes:
    • No DB deps; lives entirely in :brain app.
    • Macro discipline: `use Brain, region: :pfc`.
  """

  use Brain, region: :pfc
  @behaviour Brain.Region

  require Logger

  # ── Public API ──────────────────────────────────────────────────────────────

  @type goal :: %{
          optional(:intent) => atom() | String.t(),
          optional(:keyword) => String.t(),
          optional(:confidence) => number(),
          optional(:at_ms) => integer()
        }

  @doc "Set/override the current goal (usually from Core.IntentResolver)."
  @spec set_goal(goal()) :: :ok
  def set_goal(g) when is_map(g), do: GenServer.cast(__MODULE__, {:set_goal, g})

  @doc "Adjust controller mode: :conservative | :balanced | :adventurous."
  @spec set_mode(atom()) :: :ok
  def set_mode(mode), do: GenServer.cast(__MODULE__, {:set_mode, mode})

  @doc "Optionally override specific knobs at runtime (weights, thresholds, etc)."
  @spec set_overrides(map()) :: :ok
  def set_overrides(m) when is_map(m), do: GenServer.cast(__MODULE__, {:set_overrides, m})

  @doc """
  Produce a policy for the current SI and context.

  Returns a keyword list designed to be merged into LIFG options:
    [
      weights: %{lex_fit: 0.40, rel_prior: 0.35, activation: 0.15, intent_bias: 0.10},
      margin_threshold: 0.12,
      min_margin: 0.05,
      pmtg_mode: :boost,
      acc_conflict_tau: 0.50,
      delta_model: :margin_scaled,
      base_boost: 0.20,
      base_inhib: 0.10,
      gate_into_wm: true,
      lifg_min_score: 0.60,
      intent_bias_map: %{token_index => bias}
    ]
  """
  @spec policy(map()) :: keyword()
  def policy(si \\ %{}) when is_map(si), do: GenServer.call(__MODULE__, {:policy, si}, 250)

  @doc "Region atom key."
  @impl Brain.Region
  def region, do: :pfc

  @doc """
  Web/UI adapter: summarize policy for display badges.
  Accepts `%{snapshot: map}` or plain snapshot (from :status).
  """
  @impl Brain.Region
  def handle(region_state) do
    snap =
      cond do
        is_map(region_state) and is_map(region_state[:snapshot]) -> region_state.snapshot
        is_map(region_state) -> region_state
        true -> %{}
      end

    mode   = snap[:mode] || :balanced
    mt     = get_in(snap, [:policy, :margin_threshold]) || 0.12
    pmtg   = get_in(snap, [:policy, :pmtg_mode]) || :boost
    items  = (get_in(snap, [:wm, :size]) || 0) * 1
    cap    = (get_in(snap, [:wm, :capacity]) || 0) * 1
    meta   = Map.get(snap, :meta, %{})
    conf   = meta[:conf] || 1.0
    confl  = meta[:conflict] || 0.0
    surpr  = meta[:surprise] || 0.0

    %{
      title: "PFC",
      summary:
        "mode=#{mode} · mt=#{Float.round(mt, 2)} · pMTG=#{pmtg} · WM #{items}/#{cap} · conf=#{Float.round(conf,2)} · conflict=#{Float.round(confl,2)} · surprise=#{Float.round(surpr,2)}",
      badges: [
        %{k: :mode, v: mode},
        %{k: :mt, v: mt},
        %{k: :pmtg, v: pmtg},
        %{k: :wm, v: "#{items}/#{cap}"},
        %{k: :conf, v: conf},
        %{k: :conflict, v: confl},
        %{k: :surprise, v: surpr}
      ]
    }
  end

  # ── State ───────────────────────────────────────────────────────────────────

  defmodule State do
    @moduledoc false
    defstruct mode: :balanced,
              last_goal: nil,
              overrides: %{},          # {:weights, :margin_threshold, :pmtg_mode, :acc_conflict_tau, ...}
              defaults: %{
                weights: %{lex_fit: 0.40, rel_prior: 0.35, activation: 0.15, intent_bias: 0.10},
                margin_threshold: 0.12,
                min_margin: 0.05,
                pmtg_mode: :boost,
                acc_conflict_tau: 0.50,
                delta_model: :margin_scaled,
                base_boost: 0.20,
                base_inhib: 0.10,
                gate_into_wm: true
              }
  end

  @impl true
  def init(_opts) do
    {:ok, %State{}}
  end

  # ── Casts ───────────────────────────────────────────────────────────────────

  @impl true
  def handle_cast({:set_goal, g}, %State{} = st) do
    g2 = normalize_goal(g)
    :telemetry.execute([:brain, :pfc, :set_goal], %{confidence: g2.confidence}, g2)
    {:noreply, %State{st | last_goal: g2}}
  end

  @impl true
  def handle_cast({:set_mode, mode}, %State{} = st) do
    mode2 = if mode in [:conservative, :balanced, :adventurous], do: mode, else: :balanced
    {:noreply, %State{st | mode: mode2}}
  end

  @impl true
  def handle_cast({:set_overrides, m}, %State{} = st) do
    {:noreply, %State{st | overrides: Map.merge(st.overrides, m)}}
  end

  # ── Calls ───────────────────────────────────────────────────────────────────

  @impl true
  def handle_call({:policy, si}, _from, %State{} = st) do
    goal = st.last_goal || safe_latest_intent()
    wm   = safe_wm()
    pol  = compute_policy(si, goal, wm, st)

    snap = %{
      mode: st.mode,
      goal: goal,
      wm: %{size: wm_size(wm), capacity: wm_cap(wm)},
      policy: pol |> Enum.into(%{}),
      meta: safe_meta()
    }

    :telemetry.execute([:brain, :pfc, :policy], %{wm_size: wm_size(wm)}, snap)
    {:reply, pol, st}
  end

  @impl true
  def handle_call(:status, _from, %State{} = st) do
    goal = st.last_goal || safe_latest_intent()
    wm   = safe_wm()
    pol  = compute_policy(%{}, goal, wm, st) |> Enum.into(%{})

    snapshot = %{
      mode: st.mode,
      goal: goal,
      overrides: st.overrides,
      wm: %{size: wm_size(wm), capacity: wm_cap(wm)},
      policy: pol,
      meta: safe_meta()
    }

    :telemetry.execute([:brain, :pfc, :status], %{wm_size: wm_size(wm)}, snapshot)
    {:reply, %{snapshot: snapshot, region: :pfc}, st}
  end

  # ── Policy engine (meta-aware) ──────────────────────────────────────────────

  defp compute_policy(si, goal, wm, %State{} = st) do
    defaults = st.defaults
    ov = st.overrides

    # Start with defaults and optional overrides
    weights0 = Map.merge(defaults.weights, Map.get(ov, :weights, %{}))

    # Goal- and meta-derived signals
    conf_goal = clamp01(goal[:confidence] || goal["confidence"] || 0.0)
    wm_fill   = wm_fill_ratio(wm)

    meta        = safe_meta()
    m_conf      = clamp01((meta[:conf] || 1.0) * 1.0)
    m_conflict  = clamp01((meta[:conflict] || 0.0) * 1.0)
    m_surprise  = clamp01((meta[:surprise] || 0.0) * 1.0)

    # Be conservative: effective confidence = min(goal_conf, meta_conf)
    conf_eff = min(conf_goal, m_conf)

    # Dynamic thresholds (gentle, monotonic):
    # - Loosen a touch when low confidence
    # - Tighten with WM load and ACC conflict
    mt_base =
      cond do
        conf_eff < 0.30 -> 0.09
        conf_eff < 0.55 -> 0.11
        true -> defaults.margin_threshold
      end

    mt_conflict = mt_base + 0.04 * m_conflict

    mt_mode =
      case st.mode do
        :conservative -> mt_conflict + 0.03
        :balanced     -> mt_conflict
        :adventurous  -> max(mt_conflict - 0.02, 0.05)
      end

    mt_load =
      cond do
        wm_fill >= 0.90 -> mt_mode + 0.05
        wm_fill >= 0.75 -> mt_mode + 0.02
        true            -> mt_mode
      end

    # When to consult pMTG
    pmtg_mode =
      cond do
        Map.has_key?(ov, :pmtg_mode) -> Map.get(ov, :pmtg_mode)
        conf_eff < 0.55              -> :boost
        m_surprise >= 0.30           -> :boost
        m_conflict >= 0.50           -> :boost
        true                         -> defaults.pmtg_mode
      end

    # Conflict raises the admission bar a bit
    lifg_min_score = clamp01(0.60 + 0.10 * m_conflict)

    acc_tau =
      Map.get(ov, :acc_conflict_tau) ||
        case st.mode do
          :conservative -> 0.45
          :balanced     -> defaults.acc_conflict_tau + (wm_fill >= 0.90 && 0.05 || 0.0)
          :adventurous  -> 0.60
        end

    bias_weight = weights0.intent_bias || defaults.weights.intent_bias
    intent_bias =
      intent_bias_map_from_si(
        si,
        goal[:keyword] || goal["keyword"],
        bias_weight * bias_scale_for(conf_eff, st.mode)
      )

    base_boost =
      Map.get(ov, :base_boost) ||
        case st.mode do
          :conservative -> defaults.base_boost * 0.9
          :balanced     -> defaults.base_boost
          :adventurous  -> defaults.base_boost * 1.1
        end

    base_inhib =
      Map.get(ov, :base_inhib) ||
        case st.mode do
          :conservative -> defaults.base_inhib * 1.1
          :balanced     -> defaults.base_inhib
          :adventurous  -> defaults.base_inhib * 0.9
        end

    [
      weights: Map.put(weights0, :intent_bias, weights0.intent_bias || defaults.weights.intent_bias),
      margin_threshold: Map.get(ov, :margin_threshold, mt_load),
      min_margin: Map.get(ov, :min_margin, defaults.min_margin),
      pmtg_mode: pmtg_mode,
      acc_conflict_tau: acc_tau,
      delta_model: Map.get(ov, :delta_model, defaults.delta_model),
      base_boost: base_boost,
      base_inhib: base_inhib,
      gate_into_wm: Map.get(ov, :gate_into_wm, defaults.gate_into_wm),
      lifg_min_score: lifg_min_score,
      intent_bias_map: intent_bias
    ]
  end

  # ── Helpers ─────────────────────────────────────────────────────────────────

  defp safe_latest_intent() do
    try do
      Brain.latest_intent() || %{}
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  defp safe_meta() do
    try do
      Brain.Meta.status()
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  defp safe_wm() do
    try do
      Brain.snapshot_wm()
    rescue
      _ -> %{wm: [], cfg: %{capacity: 7}, attention: %{}}
    catch
      _, _ -> %{wm: [], cfg: %{capacity: 7}, attention: %{}}
    end
  end

  defp wm_size(%{wm: l}) when is_list(l), do: length(l)
  defp wm_size(_), do: 0

  defp wm_cap(%{cfg: %{capacity: c}}) when is_integer(c), do: c
  defp wm_cap(_), do: 7

  defp wm_fill_ratio(wm) do
    cap = max(wm_cap(wm), 1)
    min(wm_size(wm) / cap, 1.0)
  end

  defp clamp01(x) when is_number(x), do: x |> max(0.0) |> min(1.0)
  defp clamp01(_), do: 0.0

  defp bias_scale_for(conf, mode) do
    base =
      cond do
        conf < 0.30 -> 1.0
        conf < 0.55 -> 0.7
        true -> 0.5
      end

    case mode do
      :adventurous  -> base * 0.9
      :conservative -> base * 1.1
      _             -> base
    end
  end

  defp intent_bias_map_from_si(%{tokens: toks}, kw, scale) when is_list(toks) and is_binary(kw) do
    down = String.downcase(kw)
    toks
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {t, i}, acc ->
      phrase = (t[:phrase] || t["phrase"] || "") |> to_string() |> String.downcase()
      if down != "" and phrase == down, do: Map.put(acc, i, scale), else: acc
    end)
  end
  defp intent_bias_map_from_si(_, _, _), do: %{}

  defp normalize_goal(m) do
    intent0 = m[:intent] || m["intent"]
    intent =
      cond do
        is_atom(intent0) -> intent0
        is_binary(intent0) ->
          try do
            String.to_existing_atom(intent0)
          rescue
            _ -> :unknown
          end
        true -> :unknown
      end

    kw0 = m[:keyword] || m["keyword"] || ""
    conf0 = m[:confidence] || m["confidence"] || 0.0
    conf =
      cond do
        is_number(conf0) -> conf0 * 1.0
        is_binary(conf0) ->
          case Float.parse(conf0) do
            {f, _} -> f
            _ -> 0.0
          end
        true -> 0.0
      end

    at_ms = m[:at_ms] || m["at_ms"] || System.system_time(:millisecond)
    %{intent: intent, keyword: to_string(kw0), confidence: conf, at_ms: at_ms}
  end
end

