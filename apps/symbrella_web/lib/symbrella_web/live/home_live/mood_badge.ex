defmodule SymbrellaWeb.HomeLive.MoodBadge do
  @moduledoc """
  Builds the compact neuromodulator/mood badge payload for chat messages.
  """

  alias SymbrellaWeb.BrainRuntime

  @spec build(map() | nil) :: map()
  def build(meta) do
    snapshot = BrainRuntime.mood_snapshot()
    mood = snapshot |> map_get(:mood, %{}) |> normalize_mod_map()
    levels = snapshot |> map_get(:levels, %{}) |> normalize_mod_map()
    mood_sample = meta |> ensure_map() |> Map.get(:mood_sample, %{}) |> normalize_mod_map()

    %{
      mood: merge_if_empty(mood, mood_sample),
      levels: levels,
      pressure_label: map_get(snapshot, :pressure_label),
      tone_hint: map_get(snapshot, :tone_hint)
    }
  end

  defp merge_if_empty(map, fallback) when map == %{} and is_map(fallback), do: fallback
  defp merge_if_empty(map, _fallback), do: map

  defp normalize_mod_map(map) when is_map(map) do
    Map.new(map, fn {key, value} -> {normalize_mod_key(key), value} end)
  end

  defp normalize_mod_map(_), do: %{}

  defp normalize_mod_key(key) when is_atom(key), do: key
  defp normalize_mod_key("da"), do: :da
  defp normalize_mod_key("5ht"), do: :"5ht"
  defp normalize_mod_key("glu"), do: :glu
  defp normalize_mod_key("ne"), do: :ne
  defp normalize_mod_key("exploration"), do: :exploration
  defp normalize_mod_key("inhibition"), do: :inhibition
  defp normalize_mod_key("vigilance"), do: :vigilance
  defp normalize_mod_key("plasticity"), do: :plasticity
  defp normalize_mod_key(key) when is_binary(key), do: key
  defp normalize_mod_key(key), do: key

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default

  defp ensure_map(map) when is_map(map), do: map
  defp ensure_map(_), do: %{}
end
