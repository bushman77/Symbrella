defmodule SymbrellaWeb.Components.Brain.AtlasEvidence do
  @moduledoc """
  Read-only evidence for the Neural Atlas.

  BEAM imports establish static remote-call references, not observed calls.
  Live telemetry captures only registered topics received by the connected
  BrainLive process; no event payloads or user text are retained here.
  """

  @max_events 40

  @doc """
  Finds cross-region static remote-call references among registered modules.

  Dynamic dispatch, messages and calls to unregistered modules are not
  represented. An empty result is not evidence of no dependencies.
  """
  def dependencies(nodes) when is_list(nodes) do
    owners =
      for %{key: key, modules: modules} <- nodes,
          mod <- modules,
          is_atom(mod),
          into: %{},
          do: {mod, key}

    for %{key: from, modules: modules} <- nodes,
        mod <- modules,
        is_atom(mod),
        {target, function, arity} <- imports(mod),
        to = Map.get(owners, target),
        not is_nil(to),
        to != from do
      %{
        from: from,
        to: to,
        source: inspect(mod),
        target: inspect(target),
        call: "#{function}/#{arity}"
      }
    end
    |> Enum.uniq()
    |> Enum.sort_by(&{&1.from, &1.to, &1.source, &1.target, &1.call})
  end

  @doc """
  Retains only registered telemetry topic names and receiving regions,
  bounded to recent events observed while the page is connected.
  """
  def capture(events, env, nodes, at_ms \\ System.system_time(:millisecond))

  def capture(events, env, nodes, at_ms)
      when is_list(events) and is_map(env) and is_list(nodes) and is_integer(at_ms) do
    kind = Map.get(env, :kind, Map.get(env, "kind"))
    topic = Map.get(env, :event, Map.get(env, "event"))

    if kind in [:telemetry, "telemetry"] and is_list(topic) do
      regions =
        for %{key: key, telemetry: registered} <- nodes,
            is_list(registered),
            topic in registered,
            do: key

      case regions do
        [] ->
          events

        _ ->
          observed = %{
            topic: inspect(topic),
            regions: regions,
            at_ms: at_ms,
            seen_at:
              at_ms
              |> DateTime.from_unix!(:millisecond)
              |> Calendar.strftime("%H:%M:%S UTC")
          }

          [observed | events] |> Enum.take(@max_events)
      end
    else
      events
    end
  end

  def capture(events, _env, _nodes, _at_ms), do: events

  defp imports(mod) do
    with true <- Code.ensure_loaded?(mod),
         path when is_list(path) <- :code.which(mod),
         {:ok, {_module, [imports: imports]}} <- :beam_lib.chunks(path, [:imports]) do
      imports
    else
      _ -> []
    end
  end
end
