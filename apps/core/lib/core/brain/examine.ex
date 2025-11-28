defmodule Core.Brain.Examine do
  @moduledoc """
  Introspection helpers for inspecting brain-region state from Core.

  This is intentionally "debug/tooling" code, extracted out of Core.ex to keep
  the main pipeline slim.

  Notes:
  • Safe in the absence of running processes.
  • Avoids hard-coding any Symbrella/Supervisor module to prevent cross-app cycles.
    If you want supervisor discovery, pass `supervisor: YourSupervisor` explicitly.
  """

  @ui_regions [
    {:brain, Brain, :snapshot, []},
    {:lifg, Brain.LIFG, :status, []},
    {:pmtg, Brain.PMTG, :status, []},
    {:atl, Brain.ATL, :status, []},
    {:hippocampus, Brain.Hippocampus, :snapshot, []}
  ]

  @extra_regions [
    {:acc, Brain.ACC, :auto, []},
    {:thalamus, Brain.Thalamus, :auto, []},
    {:ofc, Brain.OFC, :auto, []},
    {:dlpfc, Brain.DLPFC, :auto, []},
    {:negcache, Core.NegCache, :auto, []},
    {:curiosity, Curiosity, :auto, []},
    {:core_curiosity, Core.Curiosity, :auto, []}
  ]

  @default_regions @ui_regions

  @doc """
  Inspect the state of brain regions.

  Options:
    • compact?: boolean (default false)
    • ui_only?: boolean (default true)
    • all?: boolean (default false)
    • regions: custom list of region specs
    • only: labels/modules to include
    • exclude: labels/modules to exclude
    • supervisor: pid or module name of supervisor to discover children from (optional)
    • discover_from_sup?: boolean (default false unless all?: true)
  """
  def examine(opts \\ []) do
    compact? = Keyword.get(opts, :compact?, false)
    ui_only? = Keyword.get(opts, :ui_only?, true)
    all? = Keyword.get(opts, :all?, false)

    base_regions =
      cond do
        is_list(Keyword.get(opts, :regions)) ->
          Keyword.fetch!(opts, :regions)

        all? ->
          @ui_regions ++ @extra_regions

        ui_only? ->
          @ui_regions

        true ->
          @default_regions
      end

    sup = Keyword.get(opts, :supervisor, nil)
    discover? = Keyword.get(opts, :discover_from_sup?, all?)
    discovered = if discover? and not is_nil(sup), do: discover_regions_from_sup(sup), else: []

    regions =
      (base_regions ++ discovered)
      |> uniq_regions()
      |> apply_only_filter(Keyword.get(opts, :only))
      |> apply_exclude_filter(Keyword.get(opts, :exclude))

    regions
    |> Enum.map(&safe_invoke/1)
    |> Enum.each(fn
      {:ok, label, state} ->
        if compact? do
          IO.inspect(compact_state(state), label: label)
        else
          IO.inspect(state, label: label)
        end

      {:missing, label} ->
        IO.puts("#{label}: (not running / no module)")

      {:error, label, reason} ->
        IO.puts("#{label}: ERROR #{inspect(reason)}")
    end)

    :ok
  end

  # ───────────────────────── helpers ─────────────────────────

  defp safe_invoke({label, mod, fun, args}) when is_atom(label) and is_atom(mod) do
    cond do
      Code.ensure_loaded?(mod) and fun == :auto ->
        auto_snapshot(label, mod)

      Code.ensure_loaded?(mod) and function_exported?(mod, fun, length(args)) ->
        try do
          {:ok, label, apply(mod, fun, args)}
        rescue
          e -> {:error, label, e}
        catch
          kind, term -> {:error, label, {kind, term}}
        end

      true ->
        case Process.whereis(mod) do
          pid when is_pid(pid) ->
            try do
              {:ok, label, :sys.get_state(pid)}
            rescue
              e -> {:error, label, e}
            catch
              kind, term -> {:error, label, {kind, term}}
            end

          _ ->
            {:missing, label}
        end
    end
  end

  defp safe_invoke(mod) when is_atom(mod),
    do: safe_invoke({module_label(mod), mod, :auto, []})

  defp auto_snapshot(label, mod) do
    cond do
      function_exported?(mod, :status, 0) ->
        {:ok, label, apply(mod, :status, [])}

      function_exported?(mod, :snapshot, 0) ->
        {:ok, label, apply(mod, :snapshot, [])}

      function_exported?(mod, :state, 0) ->
        {:ok, label, apply(mod, :state, [])}

      true ->
        case Process.whereis(mod) do
          pid when is_pid(pid) ->
            try do
              {:ok, label, :sys.get_state(pid)}
            rescue
              e -> {:error, label, e}
            catch
              kind, term -> {:error, label, {kind, term}}
            end

          _ ->
            {:missing, label}
        end
    end
  end

  defp discover_regions_from_sup(sup) do
    pid =
      case sup do
        m when is_atom(m) -> Process.whereis(m)
        p when is_pid(p) -> p
        _ -> nil
      end

    cond do
      is_pid(pid) ->
        Supervisor.which_children(pid)
        |> Enum.flat_map(fn
          {_id, _pid, _type, [mod]} when is_atom(mod) -> [{module_label(mod), mod, :auto, []}]
          {_id, _pid, _type, mod} when is_atom(mod) -> [{module_label(mod), mod, :auto, []}]
          _ -> []
        end)

      true ->
        []
    end
  end

  defp module_label(mod) when is_atom(mod) do
    mod
    |> Atom.to_string()
    |> String.split(".")
    |> List.last()
    |> Macro.underscore()
    |> String.to_atom()
  end

  defp label_from_selector(sel) when is_atom(sel) do
    case Atom.to_string(sel) do
      "Elixir." <> _ -> module_label(sel)
      _ -> sel
    end
  end

  defp uniq_regions(specs) do
    specs
    |> Enum.uniq_by(fn
      {label, _m, _f, _a} -> label
      m when is_atom(m) -> module_label(m)
      other -> other
    end)
  end

  defp apply_only_filter(specs, nil), do: specs

  defp apply_only_filter(specs, only) when is_list(only) do
    wanted = only |> Enum.map(&label_from_selector/1) |> MapSet.new()

    Enum.filter(specs, fn
      {label, _m, _f, _a} -> MapSet.member?(wanted, label)
      m when is_atom(m) -> MapSet.member?(wanted, module_label(m))
      _ -> false
    end)
  end

  defp apply_exclude_filter(specs, nil), do: specs

  defp apply_exclude_filter(specs, exclude) when is_list(exclude) do
    blocked = exclude |> Enum.map(&label_from_selector/1) |> MapSet.new()

    Enum.reject(specs, fn
      {label, _m, _f, _a} -> MapSet.member?(blocked, label)
      m when is_atom(m) -> MapSet.member?(blocked, module_label(m))
      _ -> false
    end)
  end

  defp compact_state(%{} = map) do
    keys = Map.keys(map)

    %{
      __summary__: true,
      size: map_size(map),
      keys: Enum.take(Enum.sort(keys), 10)
    }
  end

  defp compact_state(list) when is_list(list), do: %{__summary__: true, length: length(list)}
  defp compact_state(other), do: other
end
