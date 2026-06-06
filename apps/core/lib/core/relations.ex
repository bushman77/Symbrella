defmodule Core.Relations do
  @moduledoc """
  Zero-write relations reader and evidence annotator.

  This module enriches a SemanticInput-like map (`si`) by *reading* lexical relations
  (synonyms, antonyms, and homonyms) from `Db.BrainCell` rows that match the norms
  present in `si.tokens`.

  It performs **no database writes**.

  ## What it attaches

  The function appends relation edges into `si.evidence[:relations]` using compact tuples:

    * `{:syn, from_norm, to_norm, 0.6}` — synonym edge
    * `{:ant, from_norm, to_norm, -0.4}` — antonym edge
    * `{:hom, norm, sense_id, 0.5}` — homonym edge from a norm to one candidate sense id

  These edges are evidence for downstream scoring, not absolute truth.

  ## Optional active-cell priming

  When `attach_related_cells?: true` (default), the module also fetches related
  `Db.BrainCell` rows for synonym/antonym targets and merges **plain maps** into
  `si.active_cells` with soft activation values. This avoids leaking Ecto structs into
  the active-cell path.

  ## Telemetry

  Emits:

    * `[:core, :relations, :edges_attached]`

  Measurements:

    * `%{count: total_edges}`

  Metadata:

    * `%{syn: syn_count, ant: ant_count, hom: hom_count}`

  ## Failure semantics

  This function is defensive: if anything raises (bad token shapes, DB unavailable, etc.),
  it returns the original `si` unchanged.
  """

  alias Core.Relations.DbSource

  @syn_weight 0.6
  @ant_weight -0.4
  @hom_weight 0.5

  @typedoc """
  A compact relation edge stored in `si.evidence[:relations]`.
  """
  @type edge ::
          {:syn, String.t(), String.t(), float()}
          | {:ant, String.t(), String.t(), float()}
          | {:hom, String.t(), String.t(), float()}

  @type relation_row :: %{
          required(:id) => String.t(),
          required(:norm) => String.t(),
          optional(:pos) => String.t() | nil,
          optional(:synonyms) => [String.t()],
          optional(:antonyms) => [String.t()]
        }

  @type related_cell :: %{
          required(:id) => String.t(),
          required(:norm) => String.t(),
          optional(:pos) => String.t() | nil,
          optional(:lemma) => String.t(),
          optional(:word) => String.t(),
          optional(:definition) => String.t() | nil,
          optional(:example) => String.t() | nil,
          optional(:synonyms) => [String.t()],
          optional(:antonyms) => [String.t()],
          optional(:source) => atom(),
          optional(:score) => float(),
          optional(:activation) => float(),
          optional(:modulated_activation) => float()
        }

  @doc """
  Attaches synonym/antonym/homonym edges to `si.evidence[:relations]`, optionally
  priming `si.active_cells` with softly-activated related entries.

  The function extracts norms from `si.tokens` by reading `token.phrase`, `token.word`,
  and `token.norm`, normalizing them (lowercase, trim, collapse whitespace), and querying
  `Db.BrainCell` for matching active rows (`status == "active"`).

  Edges are appended (not replaced) if `si.evidence[:relations]` already exists.

  ## Options

    * `:attach_related_cells?` (boolean, default: `true`)
      When true, additionally fetches active `Db.BrainCell` rows for related norms
      discovered via synonym/antonym edges and merges them into `si.active_cells`
      as plain maps with soft activation.
  """
  @spec attach_edges(map(), keyword()) :: map()
  def attach_edges(si, opts \\ []) when is_map(si) do
    attach_related_cells? = Keyword.get(opts, :attach_related_cells?, true)

    norms = extract_norms(si)
    rows = DbSource.load_relation_rows(norms)

    syn_edges = synonym_edges(rows)
    ant_edges = antonym_edges(rows)
    hom_edges = homonym_edges(rows)
    edges = Enum.uniq(syn_edges ++ ant_edges ++ hom_edges)

    si
    |> put_relation_evidence(edges)
    |> maybe_attach_related_cells(edges, attach_related_cells?)
    |> emit_telemetry(length(syn_edges), length(ant_edges), length(hom_edges), length(edges))
  rescue
    _ -> si
  end

  # -- extraction -------------------------------------------------------------

  defp extract_norms(%{} = si) do
    si
    |> Map.get(:tokens, Map.get(si, "tokens", []))
    |> List.wrap()
    |> Enum.flat_map(&token_texts/1)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp token_texts(%{} = token) do
    [
      Map.get(token, :phrase) || Map.get(token, "phrase"),
      Map.get(token, :word) || Map.get(token, "word"),
      Map.get(token, :norm) || Map.get(token, "norm")
    ]
  end

  defp token_texts(_), do: []

  # -- edge building ----------------------------------------------------------

  defp synonym_edges(rows) do
    for row <- rows,
        synonym <- List.wrap(Map.get(row, :synonyms) || []),
        norm = normalize(synonym),
        norm != "" do
      {:syn, row.norm, norm, @syn_weight}
    end
  end

  defp antonym_edges(rows) do
    for row <- rows,
        antonym <- List.wrap(Map.get(row, :antonyms) || []),
        norm = normalize(antonym),
        norm != "" do
      {:ant, row.norm, norm, @ant_weight}
    end
  end

  defp homonym_edges(rows) do
    rows
    |> Enum.group_by(& &1.norm, & &1.id)
    |> Enum.flat_map(fn {norm, ids} ->
      Enum.map(ids, fn id -> {:hom, norm, id, @hom_weight} end)
    end)
  end

  # -- SI updates -------------------------------------------------------------

  defp put_relation_evidence(si, edges) when is_map(si) and is_list(edges) do
    evidence0 = current_evidence(si)
    relations0 = current_relations(evidence0)
    evidence1 = Map.put(evidence0, :relations, relations0 ++ edges)
    Map.put(si, :evidence, evidence1)
  end

  defp maybe_attach_related_cells(si, _edges, false), do: si

  defp maybe_attach_related_cells(si, edges, true) when is_map(si) and is_list(edges) do
    related_norms = related_norms(edges)

    if related_norms == [] do
      si
    else
      related_cells = DbSource.fetch_related_cells(related_norms)
      active_cells = current_active_cells(si)

      merged =
        (active_cells ++ related_cells)
        |> Enum.uniq_by(&cell_identity/1)

      Map.put(si, :active_cells, merged)
    end
  end

  defp maybe_attach_related_cells(si, _edges, _flag), do: si

  defp related_norms(edges) do
    edges
    |> Enum.flat_map(fn
      {:syn, _from_norm, to_norm, _weight} -> [to_norm]
      {:ant, _from_norm, to_norm, _weight} -> [to_norm]
      {:hom, _norm, _sense_id, _weight} -> []
    end)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  # -- telemetry --------------------------------------------------------------

  defp emit_telemetry(si, syn_count, ant_count, hom_count, total_edges) do
    :telemetry.execute(
      [:core, :relations, :edges_attached],
      %{count: total_edges},
      %{syn: syn_count, ant: ant_count, hom: hom_count}
    )

    si
  end

  # -- normalization helpers --------------------------------------------------

  defp current_evidence(si) when is_map(si) do
    case Map.get(si, :evidence) || Map.get(si, "evidence") do
      %{} = evidence -> evidence
      _ -> %{}
    end
  end

  defp current_relations(evidence) when is_map(evidence) do
    case Map.get(evidence, :relations) || Map.get(evidence, "relations") do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp current_active_cells(si) when is_map(si) do
    case Map.get(si, :active_cells) || Map.get(si, "active_cells") do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp cell_identity(%{id: id}) when is_binary(id), do: id
  defp cell_identity(%{"id" => id}) when is_binary(id), do: id
  defp cell_identity(other), do: other

  defp normalize(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize(_), do: ""
end
