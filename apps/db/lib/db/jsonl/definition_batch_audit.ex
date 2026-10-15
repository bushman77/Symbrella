defmodule Db.JSONL.DefinitionBatchAudit do
  @moduledoc """
  Pure read-only batching support for source-to-database definition recovery.

  A source group is ALL contiguous entries for one normalized word, in source
  order. Conversion mirrors Db.JSONL.Import's positional sense numbering.
  The resulting report contains possible review candidates, not permission
  for a bulk update. Positional IDs cannot prove immutable sense identity.
  """

  alias Db.JSONL.{DefinitionOnlyBackfill, Populate, Util}

  @spec source_cells([map()], map()) :: {list(), map()}
  def source_cells(entries, counters) when is_list(entries) and is_map(counters) do
    senses =
      Enum.flat_map(entries, fn entry ->
        meta =
          %{
            "word" => Map.get(entry, "word"),
            "pos" => Map.get(entry, "pos"),
            "etymology_number" => Map.get(entry, "etymology_number"),
            "entry_synonyms" => Util.flatten_term_list(Map.get(entry, "synonyms")),
            "entry_antonyms" => Util.flatten_term_list(Map.get(entry, "antonyms"))
          }
          |> Enum.reject(fn {_key, value} -> is_nil(value) end)
          |> Map.new()

        entry
        |> Map.get("senses", [])
        |> List.wrap()
        |> Enum.map(fn sense ->
          sense |> Util.ensure_map() |> Map.put("_meta", meta)
        end)
      end)

    Populate.populate_structs_from_senses_with_counters(senses, counters)
  end

  @doc "Classifies a complete word/POS source and stored set; no database calls."
  @spec inspect_group(map(), String.t(), String.t(), list(), list()) :: map()
  def inspect_group(state, word, pos, source, stored) do
    # Only evaluate a POS when a reconstructed sense contains an extended
    # gloss. The full set is still sent to the strict DefinitionOnlyBackfill
    # planner so missing IDs and all sense metadata are checked.
    candidate? =
      Enum.any?(source, fn cell ->
        is_binary(cell.definition) and String.contains?(cell.definition, " — ")
      end)

    if not candidate? do
      %{state | no_extended_gloss: state.no_extended_gloss + 1}
    else
      case DefinitionOnlyBackfill.plan(word, pos, source, stored) do
        {:ok, %{patch_count: 0}} ->
          %{state |
            reviewed_groups: state.reviewed_groups + 1,
            already_current: state.already_current + 1}

        {:ok, plan} ->
          example = %{
            word: word, pos: pos,
            total_senses: plan.row_count, definitions_to_extend: plan.patch_count,
            preview: plan.patches
              |> Enum.take(2)
              |> Enum.map(fn p ->
                %{id: p.id, old: p.old_definition, proposed: p.new_definition}
              end)
          }

          samples =
            [example | state.candidate_examples]
            |> Enum.sort_by(fn e -> {-e.definitions_to_extend, e.word, e.pos} end)
            |> Enum.take(state.sample_limit)

          %{state |
            reviewed_groups: state.reviewed_groups + 1,
            candidate_groups: state.candidate_groups + 1,
            definitions_to_extend: state.definitions_to_extend + plan.patch_count,
            candidate_examples: samples}

        {:error, error} ->
          reason =
            case error do
              {:id_census_mismatch, _, _} -> :id_census_mismatch
              {_id, code} when is_atom(code) -> code
              code when is_atom(code) -> code
              _ -> :other_error
            end

          sample = %{word: word, pos: pos, reason: reason}
          examples =
            if length(state.conflict_examples) < state.sample_limit,
              do: [sample | state.conflict_examples],
              else: state.conflict_examples

          %{state |
            reviewed_groups: state.reviewed_groups + 1,
            blocked_groups: state.blocked_groups + 1,
            blocked_by_reason: Map.update(state.blocked_by_reason, reason, 1, &(&1 + 1)),
            conflict_examples: examples}
      end
    end
  end

  @spec new(non_neg_integer()) :: map()
  def new(sample_limit \\ 8) do
    %{
      sample_limit: sample_limit,
      reviewed_groups: 0,
      no_extended_gloss: 0,
      already_current: 0,
      candidate_groups: 0,
      definitions_to_extend: 0,
      blocked_groups: 0,
      blocked_by_reason: %{},
      candidate_examples: [],
      conflict_examples: []
    }
  end
end
