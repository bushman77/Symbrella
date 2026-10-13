defmodule Core.TrainingArchive.RuntimeObservation do
  @moduledoc """
  Read-only, best-effort MoodCore telemetry captured at completed chat turns.

  It does not synthesize SemanticInput, trigger cognition, advance MoodCore, or
  fabricate a self-model for responses that bypassed the semantic pipeline.
  """

  @source "Brain.MoodCore.snapshot"
  @raw %{"da" => ["da", "dopamine"], "5ht" => ["5ht", "serotonin", "ht"],
         "glu" => ["glu", "glutamate"], "ne" => ["ne", "norepinephrine"]}
  @derived ["exploration", "inhibition", "vigilance", "plasticity"]

  @doc "Capture a single fresh, read-only runtime snapshot if MoodCore is running."
  def capture do
    cond do
      not Code.ensure_loaded?(Brain.MoodCore) ->
        unavailable("module_unavailable")

      not Process.whereis(Brain.MoodCore) ->
        unavailable("process_unavailable")

      not function_exported?(Brain.MoodCore, :snapshot, 0) ->
        unavailable("snapshot_unavailable")

      true ->
        from_snapshot(Brain.MoodCore.snapshot())
    end
  rescue
    _ -> unavailable("snapshot_failed")
  catch
    :exit, _ -> unavailable("snapshot_failed")
  end

  @doc "Project a supplied MoodCore snapshot for deterministic tests."
  def from_snapshot(%{} = snapshot) do
    levels = map_get(snapshot, "levels") |> map()
    mood = map_get(snapshot, "mood") |> map()

    raw = Map.new(@raw, fn {name, keys} ->
      {name, Enum.find_value(keys, &numeric_value(levels, &1))}
    end)

    derived = Map.new(@derived, fn name -> {name, numeric_value(mood, name)} end)

    raw_count = Enum.count(raw, fn {_name, value} -> is_number(value) end)

    %{
      source: @source,
      captured_at: DateTime.utc_now() |> DateTime.to_iso8601(),
      status: if(raw_count == 4, do: "captured", else: "partial"),
      levels: raw,
      mood: derived
    }
  end

  def from_snapshot(_), do: unavailable("invalid_snapshot")

  defp unavailable(reason) do
    %{
      source: @source,
      captured_at: DateTime.utc_now() |> DateTime.to_iso8601(),
      status: "unavailable",
      reason: reason,
      levels: %{},
      mood: %{}
    }
  end

  defp map_get(map, key) do
    Map.get(map, key) ||
      Enum.find_value(map, fn
        {atom, value} when is_atom(atom) -> if Atom.to_string(atom) == key, do: value
        _ -> nil
      end)
  end
  defp map(%{} = value), do: value
  defp map(_), do: %{}

  defp numeric_value(map, key) do
    value =
      Map.get(map, key) ||
        Enum.find_value(map, fn
          {atom, v} when is_atom(atom) -> if Atom.to_string(atom) == key, do: v
          _ -> nil
        end)

    if is_number(value) and value >= 0 and value <= 1, do: value * 1.0
  end
end
