defmodule Core.Pipeline.LTM do
  @moduledoc """
  Long-term memory enrichment stage for `Core.SemanticInput`.

  Responsibilities:
  - read LTM matches through `Core.Pipeline.LTM.Source`
  - merge normalized active cells into the SI
  - accumulate DB hit metadata into `:activation_summary`
  """

  alias Core.Pipeline.LTM.Source

  @spec run(map(), keyword()) :: map()
  def run(%{} = si, opts) when is_list(opts) do
    case Source.ltm(si, opts) do
      {:ok, %{rows: rows, db_hits: %MapSet{} = db_hits}} when is_list(rows) ->
        audit = retrieval_audit(rows, opts)

        :telemetry.execute(
          [:core, :ltm, :retrieval],
          %{
            candidate_rows: audit.candidate_rows,
            token_positions: audit.token_positions,
            max_senses_per_token: audit.max_senses_per_token,
            multi_pos_positions: audit.multi_pos_positions
          },
          %{budget: audit.budget, pos_diverse?: audit.pos_diverse?}
        )

        si
        |> enrich(rows, db_hits)
        |> Map.update(:activation_summary, %{ltm_retrieval: audit}, fn summary ->
          Map.put(summary, :ltm_retrieval, audit)
        end)

      _ ->
        si
    end
  end

  def run(si, _opts), do: si

  @doc """
  Summarize lexical retrieval without asserting that a bounded query preserved
  the correct sense. All terms and sense IDs are omitted from this audit.

  `limit_per_norm: :all` remains the default; finite limits are explicit and
  potentially lossy. `pos_diverse?: true` requests POS-balanced rows from Db.
  """
  @spec retrieval_audit([map()], keyword()) :: map()
  def retrieval_audit(rows, opts \\ []) when is_list(rows) and is_list(opts) do
    groups = Enum.group_by(rows, &Map.get(&1, :token_index))
    per_token = Map.values(groups)

    limit = Keyword.get(opts, :limit_per_norm, :all)

    budget =
      case limit do
        n when is_integer(n) and n > 0 -> {:limited, n}
        _ -> :all
      end

    %{
      budget: budget,
      pos_diverse_requested?: Keyword.get(opts, :pos_diverse?, false) == true,
      pos_diverse?:
        budget != :all and Keyword.get(opts, :pos_diverse?, false) == true,
      candidate_rows: length(rows),
      token_positions: map_size(groups),
      max_senses_per_token:
        per_token |> Enum.map(&length/1) |> Enum.max(fn -> 0 end),
      ambiguous_positions: Enum.count(per_token, &(length(&1) > 1)),
      multi_pos_positions:
        Enum.count(per_token, fn entries ->
          entries |> Enum.map(&Map.get(&1, :pos)) |> Enum.uniq() |> length() > 1
        end),
      # Without a full-slate comparison, a limited query cannot claim to have
      # retained the correct sense or every applicable part of speech.
      sense_recall: :not_measured
    }
  end

  @doc """
  Merge already retrieved, bounded read-only LTM results into a SemanticInput.
  Both production run/2 and the checked Colony path use this same mapping.
  """
  @spec enrich(map(), [map()], MapSet.t()) :: map()
  def enrich(%{} = si, rows, %MapSet{} = db_hits) when is_list(rows) do
    existing =
      case Map.get(si, :active_cells, []) do
        cells when is_list(cells) -> cells
        _ -> []
      end

    active_cells =
      (existing ++ rows)
      |> Enum.flat_map(&sanitize_cell/1)
      |> Enum.reject(&(cell_id(&1) == nil))
      # A dictionary sense can occur at several token positions in one
      # sentence. Deduplicate within one position, not across occurrences:
      # "the ... the" needs distinct POS evidence for both indices.
      |> Enum.uniq_by(&candidate_position_key/1)

    db_hits_ms = db_hits

    activation_summary0 =
      case Map.get(si, :activation_summary) do
        %{} = summary -> summary
        _ -> %{}
      end

    activation_summary =
      Map.update(activation_summary0, :db_hits, db_hits_ms, fn acc ->
        acc_ms =
          cond do
            match?(%MapSet{}, acc) -> acc
            is_list(acc) -> MapSet.new(acc)
            is_nil(acc) -> MapSet.new()
            true -> MapSet.new(List.wrap(acc))
          end

        MapSet.union(acc_ms, db_hits_ms)
      end)

    si
    |> Map.put(:active_cells, active_cells)
    |> Map.put(:activation_summary, activation_summary)
  end

  defp sanitize_cell(%{id: _} = cell), do: [cell]
  defp sanitize_cell(%{"id" => _} = cell), do: [cell]
  defp sanitize_cell(id) when is_binary(id), do: [%{id: id}]
  defp sanitize_cell(_), do: []

  defp candidate_position_key(cell) when is_map(cell) do
    index = Map.get(cell, :token_index, Map.get(cell, "token_index"))
    {cell_id(cell), index}
  end

  defp candidate_position_key(cell), do: {cell_id(cell), nil}

  defp cell_id(%{id: id}), do: id
  defp cell_id(%{"id" => id}), do: id
  defp cell_id(id) when is_binary(id), do: id
  defp cell_id(_), do: nil
end
