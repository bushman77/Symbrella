defmodule Brain.Introspect do
  @moduledoc """
  Best-effort snapshot of a brain *region* at runtime, plus a turn-level explainer.

  Region snapshot:
    Returns a map with:
      :region, :running?, :pid, :module, :info (process info), :state (from :sys.get_state/1 if possible)

    It tries multiple strategies to find the region PID:
      1) If the region has a primary module (e.g., Brain.LIFG), check Process.whereis(module)
      2) If a Registry named Brain.Registry exists, try Registry.lookup/2 with the region key
      3) Try common global names like :"brain-<region>"

    All calls are wrapped in try/rescue so the UI never crashes.

    Convenience:
      • snapshot/0 returns a map of all known regions → %{lifg: ..., pmtg: ..., ...}

  Turn explainer:
    explain_turn/1 inspects the *blackboard* (Brain.snapshot/0) and returns a structured,
    UI-friendly map showing:
      - top_cells (sorted)
      - gating decisions (eligible? and reason)
      - wm_before (from snapshot)
      - wm_selected (simulated promotions given current cfg)
      - intent (from last_intent)
      - notes (brief summary)
  """

  @type region :: atom()

  # ===== Region snapshots =====================================================

  @doc """
  Take a best-effort snapshot of all known regions and return a map keyed by region.
  """
  @spec snapshot() :: map()
  def snapshot() do
    Enum.reduce(known_regions(), %{}, fn r, acc ->
      Map.put(acc, r, snapshot(r))
    end)
  end

  @spec snapshot(region) :: map()
  def snapshot(region) when is_atom(region) do
    mod = module_for(region)
    pid = resolve_pid(region, mod)

    info =
      if is_pid(pid) and Process.alive?(pid) do
        safe_process_info(pid)
      else
        %{}
      end

    state =
      if is_pid(pid) and Process.alive?(pid) do
        safe_get_state(pid)
      else
        :down
      end

    %{
      region: region,
      running?: is_pid(pid) and Process.alive?(pid),
      pid: pid,
      module: mod,
      info: info,
      state: state
    }
  end

  @spec known_regions() :: [region]
  def known_regions(),
    do: [
      :lifg,
      :pmtg,
      :hippocampus,
      :thalamus,
      :ofc,
      :cerebellum,
      :occipital,
      :parietal,
      :temporal,
      :frontal
    ]

  # ---------- resolution ----------

  defp module_for(:lifg), do: Brain.LIFG
  defp module_for(:pmtg), do: Brain.PMTG
  defp module_for(:hippocampus), do: Brain.Hippocampus
  defp module_for(:thalamus), do: Brain.Thalamus
  defp module_for(:ofc), do: Brain.OFC
  defp module_for(:cerebellum), do: Brain.Cerebellum
  defp module_for(:occipital), do: Brain.Occipital
  defp module_for(:parietal), do: Brain.Parietal
  # If you later add Brain.Temporal, swap this to that module.
  defp module_for(:temporal), do: Brain.PMTG
  defp module_for(:frontal), do: nil
  defp module_for(_), do: nil

  defp resolve_pid(region, mod) do
    cond do
      is_atom(mod) and is_pid(Process.whereis(mod)) ->
        Process.whereis(mod)

      Code.ensure_loaded?(Registry) and Code.ensure_loaded?(Brain.Registry) ->
        lookup_registry(region)

      is_pid(Process.whereis(:"brain-#{region}")) ->
        Process.whereis(:"brain-#{region}")

      true ->
        nil
    end
  end

  defp lookup_registry(region) do
    try do
      case Registry.lookup(Brain.Registry, region) do
        [{pid, _meta} | _] -> pid
        _ -> nil
      end
    rescue
      _ -> nil
    end
  end

  # ---------- safe readers ----------

  defp safe_process_info(pid) when is_pid(pid) do
    try do
      keys = [:registered_name, :message_queue_len, :current_function, :reductions, :memory]
      info = Process.info(pid, keys) || []
      Map.new(info)
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  defp safe_get_state(pid) do
    try do
      # :sys.get_state/1 can raise if the process isn't a sys-aware server; sandbox it
      task = Task.async(fn -> :sys.get_state(pid) end)
      Task.await(task, 75)
    rescue
      _ -> :unavailable
    catch
      _, _ -> :unavailable
    end
  end

  @doc """
  UI adapter: return %{ok?: true, status: snapshot} when region is running,
  or %{error: reason, status: snapshot} otherwise.
  """
  @spec region_state(region) :: map()
  def region_state(region) when is_atom(region) do
    snap = snapshot(region)

    if snap.running? do
      %{ok?: true, status: snap}
    else
      reason =
        cond do
          snap.pid == nil -> :noproc
          snap.state == :down -> :not_running
          true -> :unavailable
        end

      %{error: reason, status: snap}
    end
  end

  # ===== Turn-level explainer =================================================

  @type explain_opts :: %{
          optional(:limit) => pos_integer(),
          optional(:min_score) => number(),
          optional(:dedup_by_norm?) => boolean()
        }

  @doc """
  Explain the *current* turn using the Brain blackboard.

  Returns a map:
    %{
      intent: %{intent: atom, confidence: number} | nil,
      top_cells: [%{id, norm, pos, score}],
      gating: [
        %{
          id, norm, pos, score,
          eligible?: boolean,
          reason: :below_threshold | :duplicate | :budget_exhausted | :excluded_source | nil
        }
      ],
      wm_before: list(),
      wm_selected: [id],   # simulated promotions given cfg/budget
      cfg: %{gate_threshold, lemma_budget, diversity_lambda, ...},
      notes: [String.t()],
      simulated?: true
    }
  """
  @spec explain_turn(keyword | explain_opts) :: map()
  def explain_turn(opts \\ []) do
    # we only need the blackboard, not regions
    snap = safe_brain_snapshot()

    active = Map.get(snap, :active_cells, %{}) || %{}
    wm_before = Map.get(snap, :wm, []) || []
    cfg = Map.get(snap, :wm_cfg, %{}) || %{}
    last_intent = Map.get(snap, :last_intent)

    limit = get_in_opts(opts, :limit, 10)
    min_score = get_in_opts(opts, :min_score, 0.0)
    dedup? = get_in_opts(opts, :dedup_by_norm?, true)

    gate_threshold = Map.get(cfg, :gate_threshold, 0.4)
    lemma_budget = Map.get(cfg, :lemma_budget, 2)

    # Normalize active cells → list of %{id, norm, pos, score}
    cells =
      active
      |> Enum.map(fn {id, score} -> parse_cell(id, score) end)
      # drop malformed
      |> Enum.filter(& &1)
      |> Enum.sort_by(& &1.score, :desc)

    # Top N preview for the UI
    top_cells =
      cells
      |> Enum.filter(&(&1.score >= min_score))
      |> Enum.take(limit)

    # Simulated gating:
    # 1) eligible by score
    eligible = Enum.filter(cells, &(&1.score >= gate_threshold))

    # 2) dedup by norm (keep best per norm) if requested
    {unique, duplicates} =
      if dedup? do
        dedup_by_norm(eligible)
      else
        {eligible, []}
      end

    # 3) sort & budget
    unique_sorted = Enum.sort_by(unique, & &1.score, :desc)
    {selected, overflow} = Enum.split(unique_sorted, lemma_budget)

    # Mark reasons for everything
    below_threshold =
      Enum.filter(cells, &(&1.score < gate_threshold))
      |> Enum.map(&Map.merge(&1, %{eligible?: false, reason: :below_threshold}))

    dup_marked =
      Enum.map(duplicates, &Map.merge(&1, %{eligible?: false, reason: :duplicate}))

    overflow_marked =
      Enum.map(overflow, &Map.merge(&1, %{eligible?: false, reason: :budget_exhausted}))

    selected_marked =
      Enum.map(selected, &Map.merge(&1, %{eligible?: true, reason: nil}))

    gating =
      (selected_marked ++ overflow_marked ++ dup_marked ++ below_threshold)
      |> Enum.sort_by(fn m ->
        # order for readability: selected first, then overflow, dupes, below threshold (all by score desc)
        rank =
          case m[:reason] do
            nil -> 0
            :budget_exhausted -> 1
            :duplicate -> 2
            :below_threshold -> 3
            _ -> 4
          end

        {rank, -m.score}
      end)

    notes =
      [
        "cells=#{length(cells)}",
        "eligible=#{length(eligible)} (≥ #{gate_threshold})",
        if(dedup?, do: "dedup_by_norm=on dupes=#{length(duplicates)}", else: "dedup_by_norm=off"),
        "budget=#{lemma_budget} selected=#{length(selected)}"
      ]
      |> Enum.reject(&is_nil/1)

    %{
      intent:
        if is_map(last_intent) do
          %{
            intent: last_intent[:intent] || last_intent["intent"],
            confidence: last_intent[:confidence] || last_intent["confidence"]
          }
        else
          nil
        end,
      top_cells: top_cells,
      gating: gating,
      wm_before: wm_before,
      wm_selected: Enum.map(selected, & &1.id),
      cfg: cfg,
      notes: notes,
      simulated?: true
    }
  end

  # ===== helpers for explainer ===============================================

  defp safe_brain_snapshot() do
    try do
      Brain.snapshot()
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  defp parse_cell(id, score) when is_binary(id) and is_number(score) do
    {norm, pos, _idx} = split_cell_id(id)

    %{
      id: id,
      norm: norm,
      pos: pos,
      score: score
    }
  end

  defp parse_cell(_id, _score), do: nil

  # "hello|noun|0" → {"hello","noun","0"}
  defp split_cell_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [a, b, c] -> {String.downcase(a), b, c}
      [a, b] -> {String.downcase(a), b, "0"}
      [a] -> {String.downcase(a), "?", "0"}
      _ -> {"?", "?", "0"}
    end
  end

  # return {unique, duplicates}
  defp dedup_by_norm(list) do
    Enum.reduce(list, {%{}, []}, fn m, {uniq_map, dups} ->
      case Map.fetch(uniq_map, m.norm) do
        :error ->
          {Map.put(uniq_map, m.norm, m), dups}

        {:ok, existing} ->
          # keep higher score
          if m.score > existing.score do
            {Map.put(uniq_map, m.norm, m), [existing | dups]}
          else
            {uniq_map, [m | dups]}
          end
      end
    end)
    |> then(fn {uniq_map, dups} -> {Map.values(uniq_map), dups} end)
  end

  defp get_in_opts(opts, key, default) when is_list(opts) do
    Keyword.get(opts, key, default)
  end

  defp get_in_opts(%{} = opts, key, default) do
    Map.get(opts, key, default)
  end

  # ===== end =================================================================
end
