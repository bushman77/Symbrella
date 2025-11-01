defmodule Brain.Introspect do
  @moduledoc """
  Best-effort snapshot of a brain *region* at runtime.

  Returns a map with:
    :region, :running?, :pid, :module, :info (process info), :state (from :sys.get_state/1 if possible)

  It tries multiple strategies to find the region PID:
    1) If the region has a primary module (e.g., Brain.LIFG), check Process.whereis(module)
    2) If a Registry named Brain.Registry exists, try Registry.lookup/2 with the region key
    3) Try common global names like :"brain-<region>"

  All calls are wrapped in try/rescue so the UI never crashes.

  Convenience:
    • snapshot/0 returns a map of all known regions → %{lifg: ..., pmtg: ..., ...}
  """

  @type region :: atom()

  @doc """
  Convenience: take a best-effort snapshot of all known regions and return a map keyed by region.
  Keeps UI calls that use `snapshot/0` working without knowing a specific region.
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
    do: [:lifg, :pmtg, :hippocampus, :thalamus, :ofc, :cerebellum, :occipital, :parietal, :temporal, :frontal]

  # ---------- resolution ----------

  defp module_for(:lifg), do: Brain.LIFG
  defp module_for(:pmtg), do: Brain.PMTG
  defp module_for(:hippocampus), do: Brain.Hippocampus
  defp module_for(:thalamus), do: Brain.Thalamus
  defp module_for(:ofc), do: Brain.OFC
  defp module_for(:cerebellum), do: Brain.Cerebellum
  defp module_for(:occipital), do: Brain.Occipital
  defp module_for(:parietal), do: Brain.Parietal
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
      # :sys.get_state/1 can raise if the process isn't a sys-aware server; we sandbox it
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


end

