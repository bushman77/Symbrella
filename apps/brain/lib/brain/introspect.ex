defmodule Brain.Introspect do
  @moduledoc """
  Best-effort snapshot of a brain *region* at runtime.

  Returns a map with:
    :region, :running?, :pid, :module, :info (process info), :state (from :sys.get_state/1 if possible)

  It tries multiple strategies to find the region PID:
    1) If the region has a primary module (e.g., Brain.DLPFC), check Process.whereis(module)
    2) If a Registry named Brain.Registry exists, try Registry.lookup/2 with the region key
    3) Try common global names like :"brain-<region>"

  All calls are wrapped in try/rescue so the UI never crashes.
  """

  @type region :: atom()

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

  # ---------- resolution ----------

  defp module_for(:lifg), do: Brain.DLPFC
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
      # 1) Module name registration (common for GenServers)
      is_atom(mod) and Process.whereis(mod) |> is_pid() ->
        Process.whereis(mod)

      # 2) Registry registration (if you have one)
      Code.ensure_loaded?(Registry) and Code.ensure_loaded?(Brain.Registry) ->
        lookup_registry(region)

      # 3) Common global/local atoms
      Process.whereis(:"brain-#{region}") |> is_pid() ->
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
end

