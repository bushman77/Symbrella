defmodule Brain.LIFG.RelationSignals do
  @moduledoc """
  Signals derived from `si.evidence[:relations]` to nudge LIFG Stage1 scoring.

  Exposes:
    • count_overlaps/1 → {syn_hits, ant_hits}
    • homonym_bonus?/2 → true if candidate sense id is in homonym edges
  """

  # Keep runtime support for both atom- and string-keyed maps,
  # but don't reference string-keyed maps in the typespec.
  @type candidate :: %{id: String.t()} | map() | String.t()

  @spec count_overlaps(map()) :: {non_neg_integer(), non_neg_integer()}
  def count_overlaps(si) when is_map(si) do
    edges = get_edges(si)
    window_norms = window_norms(si)

    syn_targets =
      for {:syn, _from, to_norm, _w} <- edges, into: MapSet.new(), do: to_norm

    ant_targets =
      for {:ant, _from, to_norm, _w} <- edges, into: MapSet.new(), do: to_norm

    syn_hits = Enum.count(window_norms, &MapSet.member?(syn_targets, &1))
    ant_hits = Enum.count(window_norms, &MapSet.member?(ant_targets, &1))

    {syn_hits, ant_hits}
  end

  @spec homonym_bonus?(map(), candidate) :: boolean()
  def homonym_bonus?(si, candidate) when is_map(si) do
    id = candidate_id(candidate)

    hom_ids =
      get_edges(si)
      |> Enum.flat_map(fn
        {:hom, _from_norm, sid, _w} -> [sid]
        _ -> []
      end)
      |> MapSet.new()

    is_binary(id) and MapSet.member?(hom_ids, id)
  end

  # ------- internals --------

  defp get_edges(si) when is_map(si) do
    ev = Map.get(si, :evidence) || %{}

    case Map.get(ev, :relations) || Map.get(ev, "relations") do
      xs when is_list(xs) -> xs
      _ -> []
    end
  end

  defp window_norms(si) when is_map(si) do
    (Map.get(si, :tokens, []) || [])
    |> Enum.flat_map(fn t -> [Map.get(t, :phrase), Map.get(t, :word)] end)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp candidate_id(%{id: id}) when is_binary(id), do: id
  defp candidate_id(%{"id" => id}) when is_binary(id), do: id
  defp candidate_id(id) when is_binary(id), do: id
  defp candidate_id(_), do: nil

  defp normalize(s) when is_binary(s) do
    s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")
  end

  defp normalize(_), do: ""
end
