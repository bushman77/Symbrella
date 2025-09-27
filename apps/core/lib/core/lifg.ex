defmodule Core.LIFG do
  @moduledoc """
  Lightweight Input Feature Graph (LIFG).

  This module is intentionally **drop-in** and **side-effect free**.
  """                                                         
  @typedoc "Node id string"
  @type id :: String.t()

  @typedoc "LIFG node (token or cell)"                          @type node_t :: %{                                                    required(:id) => id(),
          required(:kind) => :token | :cell,
          optional(:phrase) => String.t(),                              optional(:norm) => String.t(),
          optional(:span) => {non_neg_integer(), non_neg_integer()} | nil,                                                            optional(:mw) => boolean(),
          optional(:pos) => String.t() | nil,
          optional(:status) => any(),
          optional(:activation) => number(),
          optional(:meta) => map()
        }

  @typedoc "Directed edge (from, to, type, weight)"
  @type edge_t :: {id(), id(), atom(), number()}

  @typedoc "Main graph container"
  @type lifg_t :: %{
          nodes: %{id() => node_t()},
          edges: [edge_t()],
          index: %{
            token_idx: %{non_neg_integer() => id()},
            norm: %{String.t() => [id()]}
          },
          stats: map()
        }

  @doc """
  Attach a LIFG to `si` (map or struct).

  ## Options
    * `:lowercase_norm?` (default true) – normalize token phrases to lowercase for matching
    * `:adjacency_edges?` (default true) – add token adjacency edges
    * `:max_token_edges` (default 4) – cap edges per token when linking to cells by norm
  """
  @spec attach(map(), Keyword.t()) :: map()
  def attach(si, opts \\ []) when is_map(si) do
    opts = Keyword.merge([lowercase_norm?: true, adjacency_edges?: true, max_token_edges: 4], opts)

    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :cells, Map.get(si, :db_cells, []))
    act_summary = Map.get(si, :activation_summary, %{})

    lifg =
      %{
        nodes: %{},
        edges: [],
        index: %{token_idx: %{}, norm: %{}},
        stats: %{token_count: length(tokens), cell_count: length(cells)}
      }
      |> add_token_nodes(tokens, opts)
      |> add_cell_nodes(cells)
      |> add_token_cell_edges(opts)
      |> add_token_adjacency_edges(opts)
      |> recompute_stats(act_summary)

    si
    |> put_default(:intent_candidates, [])
    |> put_default(:phrase_matches, [])
    |> Map.put(:lifg, lifg)
  end

  # ─────────────────── Node builders ───────────────────

  defp add_token_nodes(lifg, tokens, opts) do
    Enum.with_index(tokens)
    |> Enum.reduce(lifg, fn {tok, idx}, acc ->
      norm =
        case {opts[:lowercase_norm?], Map.get(tok, :phrase)} do
          {true, phrase} when is_binary(phrase) -> String.downcase(phrase)
          _ -> Map.get(tok, :phrase)
        end

      id = "tok:" <> Integer.to_string(idx)

      node = %{
        id: id,
        kind: :token,
        phrase: Map.get(tok, :phrase),
        norm: norm,
        span: Map.get(tok, :span),
        mw: Map.get(tok, :mw),
        pos: Map.get(tok, :pos)
      }

      acc
      |> put_node(node)
      |> put_index_norm(norm, id)
      |> put_in([:index, :token_idx, idx], id)
    end)
  end

  defp add_cell_nodes(lifg, cells) do
    Enum.reduce(cells, lifg, fn cell, acc ->
      cid = "cell:" <> (cell_id(cell) || make_ref_id())
      node = %{
        id: cid,
        kind: :cell,
        norm: Map.get(cell, :norm) || safe_down(Map.get(cell, :word)),
        pos: Map.get(cell, :pos),
        status: Map.get(cell, :status),
        activation: Map.get(cell, :activation),
        meta: %{
          word: Map.get(cell, :word),
          type: Map.get(cell, :type),
          token_id: Map.get(cell, :token_id),
          connections: Map.get(cell, :connections, [])
        }
      }

      acc
      |> put_node(node)
      |> put_index_norm(node[:norm], cid)
    end)
  end

  # ─────────────────── Edge builders ───────────────────

  # Link tokens to cells by matching norm (or cell.word)
  defp add_token_cell_edges(lifg, opts) do
    max_edges = opts[:max_token_edges] || 4

    lifg.index.token_idx
    |> Enum.reduce(lifg, fn {_idx, tok_id}, acc ->
      tok_norm = get_in(acc, [:nodes, tok_id, :norm])
      candidates = Map.get(acc.index.norm, tok_norm, []) |> Enum.filter(&String.starts_with?(&1, "cell:"))
      picked = Enum.take(candidates, max_edges)
      edges = Enum.map(picked, fn cid -> {tok_id, cid, :evidence, 1.0} end)
      %{acc | edges: acc.edges ++ edges}
    end)
  end

  # Add token adjacency edges (tok:i -> tok:i+1)
  defp add_token_adjacency_edges(lifg, opts) do
    if opts[:adjacency_edges?] do
      ids =
        lifg.index.token_idx
        |> Enum.sort_by(fn {i, _} -> i end, :asc)
        |> Enum.map(fn {_i, id} -> id end)

      edges =
        ids
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [a, b] -> {a, b, :adjacent, 0.5} end)

      %{lifg | edges: lifg.edges ++ edges}
    else
      lifg
    end
  end

  # ─────────────────── Stats ───────────────────

  defp recompute_stats(lifg, act_summary) do
    token_nodes =
      lifg.nodes
      |> Map.values()
      |> Enum.filter(&(&1.kind == :token))

    covered_tokens =
      token_nodes
      |> Enum.count(fn t ->
        has_edge?(lifg, t.id, :evidence)
      end)

    stats =
      lifg.stats
      |> Map.put(:token_covered, covered_tokens)
      |> Map.put(:edges, length(lifg.edges))
      |> Map.put(:activated_cells, Map.keys(act_summary) |> length())

    %{lifg | stats: stats}
  end

  defp has_edge?(lifg, from_id, type) do
    Enum.any?(lifg.edges, fn {f, _t, ty, _w} -> f == from_id and ty == type end)
  end

  # ─────────────────── Utilities ───────────────────

  defp put_default(si, key, value) do
    case Map.fetch(si, key) do
      {:ok, _} -> si
      :error -> Map.put(si, key, value)
    end
  end

  defp put_node(lifg, %{id: id} = node) do
    put_in(lifg, [:nodes, id], node)
  end

  defp put_index_norm(lifg, nil, _id), do: lifg
  defp put_index_norm(lifg, norm, id) when is_binary(norm) do
    update_in(lifg, [:index, :norm, norm], fn
      nil -> [id]
      ids -> [id | ids]
    end)
  end

  defp safe_down(nil), do: nil
  defp safe_down(s) when is_binary(s), do: String.downcase(s)

  defp cell_id(cell) do
    cond do
      v = Map.get(cell, :id) -> to_string(v)
      v = Map.get(cell, "id") -> to_string(v)
      true -> nil
    end
  end

  defp make_ref_id do
    "anon-" <> (System.unique_integer([:positive]) |> Integer.to_string())
  end
end

