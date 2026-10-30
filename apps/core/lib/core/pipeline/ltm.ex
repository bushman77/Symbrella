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
  Opt-in, read-only comparison of full and bounded dictionary candidate sets.

  This never changes the live candidate slate or invokes LIFG. The result
  measures candidate ID and POS coverage, **not** semantic correctness.
  Query errors remain explicit rather than masquerading as zero candidates.

  Options:
    * :limit_per_norm - bounded candidate count per normalized term (1..32)
    * :pos_diverse? - request POS-balanced bounded lookup (default true)
    * :max_tokens - cap requested lexical tokens (default 8, maximum 16)
    * :lookup - optional two-argument injected checked lookup for tests

  For a real database comparison, pass the canonical Phase 1 SI and ensure Db
  is running. The default production path remains unlimited and unchanged.
  """
  @spec compare_retrieval(map(), keyword()) :: {:ok, map()} | {:error, atom()}
  def compare_retrieval(si, opts \\ [])

  def compare_retrieval(%{tokens: tokens} = si, opts)
      when is_list(tokens) and is_list(opts) do
    budget = Keyword.get(opts, :limit_per_norm, 8)
    cap = Keyword.get(opts, :max_tokens, 8)
    pos_diverse? = Keyword.get(opts, :pos_diverse?, true) == true

    cond do
      not is_integer(budget) or budget < 1 or budget > 32 ->
        {:error, :invalid_budget}

      not is_integer(cap) or cap < 1 or cap > 16 ->
        {:error, :invalid_max_tokens}

      tokens == [] ->
        {:error, :no_tokens}

      length(tokens) > cap ->
        {:error, :too_many_tokens}

      true ->
        lookup = Keyword.get(opts, :lookup, &Source.checked_ltm/2)
        db_opts = Keyword.drop(opts, [:lookup, :max_tokens])
        full_opts = db_opts |> Keyword.put(:limit_per_norm, :all) |> Keyword.put(:pos_diverse?, false)
        bounded_opts =
          db_opts
          |> Keyword.put(:limit_per_norm, budget)
          |> Keyword.put(:pos_diverse?, pos_diverse?)

        with {:ok, full, full_us} <- comparison_lookup(lookup, si, full_opts),
             {:ok, bounded, bounded_us} <- comparison_lookup(lookup, si, bounded_opts) do
          {:ok,
           compare_slates(full.rows, bounded.rows)
           |> Map.merge(%{
             limit_per_norm: budget,
             pos_diverse?: pos_diverse?,
             full_lookup_us: full_us,
             bounded_lookup_us: bounded_us,
             interpretation: :lexical_candidate_overlap_only,
             semantic_accuracy: :not_measured
           })}
        else
          {:error, reason} -> {:error, reason}
        end
    end
  end

  def compare_retrieval(_, _), do: {:error, :invalid_input}

  @doc """
  Compare two independently retrieved candidate lists by token index and
  exact sense ID, while separately recording POS coverage. No SQL is required.

  The full set is the baseline. Any extra IDs in the bounded set are reported,
  not silently counted as retained.
  """
  @spec compare_slates([map()], [map()]) :: map()
  def compare_slates(full, bounded) when is_list(full) and is_list(bounded) do
    full_by_index = slate_by_token(full)
    bounded_by_index = slate_by_token(bounded)

    positions =
      (Map.keys(full_by_index) ++ Map.keys(bounded_by_index))
      |> Enum.uniq()
      |> Enum.sort()
      |> Enum.map(fn index ->
        all = Map.get(full_by_index, index, %{})
        selected = Map.get(bounded_by_index, index, %{})
        full_ids = Map.keys(all) |> MapSet.new()
        bounded_ids = Map.keys(selected) |> MapSet.new()
        retained = MapSet.intersection(full_ids, bounded_ids)
        lost = MapSet.difference(full_ids, bounded_ids)
        unexpected = MapSet.difference(bounded_ids, full_ids)
        full_pos = all |> Map.values() |> MapSet.new()
        bounded_pos = selected |> Map.values() |> MapSet.new()

        %{
          token_index: index,
          full_senses: MapSet.size(full_ids),
          bounded_senses: MapSet.size(bounded_ids),
          retained_senses: MapSet.size(retained),
          lost_senses: MapSet.size(lost),
          unexpected_senses: MapSet.size(unexpected),
          full_pos_count: MapSet.size(full_pos),
          bounded_pos_count: MapSet.size(bounded_pos),
          lost_pos: MapSet.difference(full_pos, bounded_pos) |> MapSet.to_list() |> Enum.sort()
        }
      end)

    full_count = Enum.sum(Enum.map(positions, & &1.full_senses))
    retained_count = Enum.sum(Enum.map(positions, & &1.retained_senses))

    %{
      full_senses: full_count,
      bounded_senses: Enum.sum(Enum.map(positions, & &1.bounded_senses)),
      retained_senses: retained_count,
      lost_senses: Enum.sum(Enum.map(positions, & &1.lost_senses)),
      unexpected_senses: Enum.sum(Enum.map(positions, & &1.unexpected_senses)),
      lost_pos_positions: Enum.count(positions, &(&1.lost_pos != [])),
      sense_id_coverage:
        if(full_count == 0, do: nil, else: retained_count / full_count),
      positions: positions
    }
  end

  defp slate_by_token(rows) do
    rows
    |> Enum.filter(fn row ->
      is_map(row) and is_integer(Map.get(row, :token_index)) and
        is_binary(Map.get(row, :id)) and Map.get(row, :id) != ""
    end)
    |> Enum.group_by(&Map.fetch!(&1, :token_index))
    |> Map.new(fn {index, candidates} ->
      {index, Map.new(candidates, fn row -> {row.id, Map.get(row, :pos)} end)}
    end)
  end

  defp comparison_lookup(lookup, si, opts) when is_function(lookup, 2) do
    start = System.monotonic_time()

    result =
      try do
        lookup.(si, opts)
      rescue
        _ -> {:error, :comparison_lookup_failed}
      catch
        _, _ -> {:error, :comparison_lookup_failed}
      end

    duration = System.convert_time_unit(System.monotonic_time() - start, :native, :microsecond)

    case result do
      {:ok, %{rows: rows}} when is_list(rows) ->
        {:ok, %{rows: rows}, duration}

      {:error, reason} when is_atom(reason) ->
        {:error, reason}

      _ ->
        {:error, :invalid_ltm_result}
    end
  end

  defp comparison_lookup(_, _, _), do: {:error, :invalid_lookup}

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
