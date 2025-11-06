defmodule Core.Relations do
  @moduledoc """
  Zero-IO relations reader.

  • Reads synonyms / antonyms / homonyms from `Db.BrainCell` for norms present in SI tokens.
  • Attaches edges to `si.evidence[:relations]`:
      {:syn, from_norm, to_norm, 0.6}
      {:ant, from_norm, to_norm, -0.4}
      {:hom, from_norm, sense_id, 0.5}
  • Optionally brings related norms into `si.active_cells` with a *soft* activation.
  """

  alias Db
  import Ecto.Query, only: [from: 2]

  @type edge ::
          {:syn, String.t(), String.t(), float()}
          | {:ant, String.t(), String.t(), float()}
          | {:hom, String.t(), String.t(), float()}

  @spec attach_edges(map(), keyword()) :: map()
  def attach_edges(si, opts \\ []) when is_map(si) do
    attach_related_cells? = Keyword.get(opts, :attach_related_cells?, true)

    norms =
      (Map.get(si, :tokens, []) || [])
      |> Enum.flat_map(&[Map.get(&1, :phrase), Map.get(&1, :word)])
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&normalize/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    rows =
      if norms == [] do
        []
      else
        Db.all(
          from c in Db.BrainCell,
            where: c.norm in ^norms and c.status == "active",
            select: %{
              id: c.id,
              norm: c.norm,
              pos: c.pos,
              synonyms: c.synonyms,
              antonyms: c.antonyms
            }
        )
      end

    hom =
      rows
      |> Enum.group_by(& &1.norm)
      |> Enum.map(fn {norm, rs} -> {norm, Enum.map(rs, & &1.id)} end)
      |> Map.new()

    syn_edges =
      for r <- rows,
          s <- List.wrap(r.synonyms || []),
          s_norm = normalize(s),
          s_norm != "" do
        {:syn, r.norm, s_norm, 0.6}
      end

    ant_edges =
      for r <- rows,
          a <- List.wrap(r.antonyms || []),
          a_norm = normalize(a),
          a_norm != "" do
        {:ant, r.norm, a_norm, -0.4}
      end

    hom_edges =
      for {norm, sense_ids} <- hom, sid <- sense_ids do
        {:hom, norm, sid, 0.5}
      end

    edges = syn_edges ++ ant_edges ++ hom_edges

    # --- SAFE evidence update (works even if :evidence isn't a struct field) ---
    ev0 = Map.get(si, :evidence) || %{}
    evidence = Map.update(ev0, :relations, edges, fn xs -> xs ++ edges end)
    si_with_ev = Map.put(si, :evidence, evidence)

    si_final =
      if attach_related_cells? do
        maybe_attach_related_cells(si_with_ev, edges)
      else
        si_with_ev
      end

    :telemetry.execute([:core, :relations, :edges_attached],
      %{count: length(edges)},
      %{syn: length(syn_edges), ant: length(ant_edges), hom: length(hom_edges)}
    )

    si_final
  rescue
    _ -> si
  end

  # ------- internals -------

  defp maybe_attach_related_cells(si, edges) do
    related_norms =
      edges
      |> Enum.flat_map(fn
        {:syn, _n, syn_norm, _w} -> [syn_norm]
        {:ant, _n, ant_norm, _w} -> [ant_norm]
        {:hom, _n, _sid, _w} -> []
      end)
      |> Enum.uniq()

    if related_norms == [] do
      si
    else
      rows = fetch_cells_by_norms(related_norms)

      merged =
        (Map.get(si, :active_cells, []) || [])
        |> Kernel.++(Enum.map(rows, &soften/1))
        |> Enum.uniq_by(&get_id/1)

      Map.put(si, :active_cells, merged)
    end
  end

  defp fetch_cells_by_norms(ns) do
    Db.all(from c in Db.BrainCell, where: c.norm in ^ns and c.status == "active")
  rescue
    _ -> []
  end

  defp soften(%{} = row) do
    row
    |> Map.put_new(:modulated_activation, 0.5)
    |> Map.put_new(:activation, 0.5)
  end

  defp get_id(%{id: id}), do: id
  defp get_id(%{"id" => id}), do: id

  defp normalize(s) when is_binary(s) do
    s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")
  end

  defp normalize(_), do: ""
end

