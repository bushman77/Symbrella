defmodule Db.JSONL.FullFidelityAudit do
  @moduledoc """
  Read-only source-to-BrainCell census and field-level reconciliation.

  Streams the same English JSONL that the importer used and regenerates every
  positional word|POS|index identifier with Db.JSONL.Populate. Fetches stored
  rows in bounded batches. Never writes to PostgreSQL.

  This proves fidelity to the CURRENT importer projection, not lossless storage
  of the complete Wiktextract source. Unprojected fields are counted separately.
  IDs are positional and cannot establish identity across source revisions.
  """

  alias Db.JSONL.{CategoryEvidencePolicy, Populate, Util}

  @fields [
    :word, :norm, :pos, :type, :definition, :example,
    :synonyms, :antonyms, :gram_function, :semantic_atoms
  ]
  @array_fields [:synonyms, :antonyms, :gram_function, :semantic_atoms]
  @projected_sense_keys ~w(
    _meta word pos glosses raw_glosses examples synonyms antonyms
    tags raw_tags categories qualifier
  )
  @projected_entry_keys ~w(
    word pos senses etymology_number synonyms antonyms lang lang_code
  )
  @typed_relationship_keys ~w(
    hypernyms hyponyms holonyms meronyms coordinate_terms related
    derived form_of alt_of troponyms
  )

  @doc """
  Audit one source JSONL against a DB lookup (default: the Db Ecto repository).
  Options: :lookup (fn ids -> %{id => stored_row} end), :db_count, :batch,
  :limit_lines (0 = all), :sample_limit, :progress.
  """
  def audit(path, opts \\ []) when is_binary(path) do
    batch = Keyword.get(opts, :batch, 1_000)
    limit = Keyword.get(opts, :limit_lines, 0)
    sample_limit = Keyword.get(opts, :sample_limit, 5)
    progress = Keyword.get(opts, :progress, 50_000)
    lookup = Keyword.get(opts, :lookup, &lookup_db/1)

    if batch < 1 or batch > 5_000 or limit < 0 or
         sample_limit < 0 or progress < 0 do
      raise ArgumentError, "Invalid batch, limit_lines, sample_limit, or progress"
    end

    state = new(sample_limit)
    lines = File.stream!(path, [], :line)
    lines = if limit == 0, do: lines, else: Stream.take(lines, limit)

    state =
      lines
      |> Stream.with_index(1)
      |> Enum.reduce(state, fn {line, number}, acc ->
        if progress > 0 and rem(number, progress) == 0 do
          IO.puts(:stderr,
            "Audit #{number} lines | #{acc.source_senses} senses | " <>
              "#{acc.compared} checked | #{acc.missing_ids} absent"
          )
        end

        acc = %{acc | lines: number, sha256_ctx: :crypto.hash_update(acc.sha256_ctx, line)}

        case Jason.decode(line) do
          {:ok, %{} = entry} -> consume_entry(acc, entry, lookup, batch)
          _ -> %{acc | bad_json: acc.bad_json + 1}
        end
      end)
      |> flush(lookup)

    # Supplying :db_count avoids any SQL call for unit tests.
    db_count =
      cond do
        Keyword.has_key?(opts, :db_count) -> Keyword.fetch!(opts, :db_count)
        limit > 0 -> nil
        true -> count_db()
      end

    mismatch_total = state.mismatches |> Map.values() |> Enum.sum()

    %{
      source_path: Path.expand(path),
      source_sha256: Base.encode16(:crypto.hash_final(state.sha256_ctx), case: :lower),
      sha256_covers_complete_file: limit == 0,
      complete_source_scan: limit == 0,
      lines: state.lines,
      bad_json: state.bad_json,
      source_entries: state.entries,
      source_senses: state.source_senses,
      source_empty_word_entries: state.empty_word_entries,
      suspicious_non_english_entries: state.non_english_entries,
      compared: state.compared,
      matching_rows: state.matching_rows,
      absent_ids: state.missing_ids,
      mismatch_fields: state.mismatches,
      total_field_mismatches: mismatch_total,
      db_total_rows: db_count,
      row_count_delta: if(is_integer(db_count), do: db_count - state.source_senses, else: nil),
      source_evidence: state.evidence,
      withheld_category_reasons: state.withheld_reasons,
      unprojected_sense_keys: state.unprojected_sense_keys,
      unprojected_entry_keys: state.unprojected_entry_keys,
      samples: state.samples,
      fidelity_pass:
        limit == 0 and state.bad_json == 0 and state.missing_ids == 0 and
          mismatch_total == 0 and db_count == state.source_senses
    }
  end

  defp new(sample_limit) do
    %{
      counters: %{},
      sha256_ctx: :crypto.hash_init(:sha256),
      pending: [],
      pending_count: 0,
      lines: 0,
      bad_json: 0,
      entries: 0,
      empty_word_entries: 0,
      non_english_entries: 0,
      source_senses: 0,
      compared: 0,
      matching_rows: 0,
      missing_ids: 0,
      mismatches: Map.new(@fields, &{&1, 0}),
      samples: %{},
      sample_limit: sample_limit,
      evidence: %{
        no_gloss_senses: 0,
        senses_without_definition: 0,
        hierarchical_gloss_senses: 0,
        senses_with_topics: 0,
        topic_values: 0,
        senses_with_multiple_examples: 0,
        additional_examples: 0,
        senses_with_relationship_qualifiers: 0,
        senses_with_other_typed_relationships: 0,
        entry_relationship_entries: 0,
        category_values: 0,
        promoted_categories: 0,
        withheld_categories: 0
      },
      withheld_reasons: %{},
      unprojected_sense_keys: %{},
      unprojected_entry_keys: %{}
    }
  end

  defp consume_entry(st, entry, lookup, batch) do
    word = Util.normalize_word(Map.get(entry, "word") || "")
    st = %{st | entries: st.entries + 1}

    st =
      if entry["lang_code"] == "en" or entry["lang"] == "English" do
        st
      else
        %{st | non_english_entries: st.non_english_entries + 1}
      end

    st = count_unprojected(st, entry, :unprojected_entry_keys, @projected_entry_keys)

    if word == "" do
      %{st | empty_word_entries: st.empty_word_entries + 1}
    else
      meta =
        %{
          "word" => entry["word"],
          "pos" => entry["pos"],
          "etymology_number" => entry["etymology_number"],
          "entry_synonyms" => Util.flatten_term_list(entry["synonyms"]),
          "entry_antonyms" => Util.flatten_term_list(entry["antonyms"])
        }
        |> Enum.reject(fn {_key, value} -> is_nil(value) end)
        |> Map.new()

      senses =
        entry
        |> Map.get("senses", [])
        |> List.wrap()
        |> Enum.map(fn sense -> sense |> Util.ensure_map() |> Map.put("_meta", meta) end)

      {cells, counters} =
        Populate.populate_structs_from_senses_with_counters(senses, st.counters)

      st = %{st | counters: counters, source_senses: st.source_senses + length(cells)}

      st =
        if present?(entry["synonyms"]) or present?(entry["antonyms"]) do
          increment_evidence(st, :entry_relationship_entries)
        else
          st
        end

      st =
        Enum.zip(cells, senses)
        |> Enum.reduce(st, fn {cell, sense}, acc ->
          acc
          |> count_evidence(sense, cell)
          |> count_unprojected(sense, :unprojected_sense_keys, @projected_sense_keys)
          |> then(fn state ->
            %{state | pending: [cell | state.pending],
                      pending_count: state.pending_count + 1}
          end)
        end)

      if st.pending_count >= batch, do: flush(st, lookup), else: st
    end
  end

  defp count_evidence(st, sense, cell) do
    st =
      if is_nil(cell.definition),
        do: increment_evidence(st, :senses_without_definition),
        else: st

    st =
      if "tag:no-gloss" in cell.semantic_atoms,
        do: increment_evidence(st, :no_gloss_senses),
        else: st

    glosses = if present?(sense["glosses"]), do: sense["glosses"], else: sense["raw_glosses"]
    st =
      if is_list(glosses) and length(glosses) > 1,
        do: increment_evidence(st, :hierarchical_gloss_senses),
        else: st

    topics = List.wrap(sense["topics"])
    st =
      if topics != [] do
        st |> increment_evidence(:senses_with_topics)
           |> increment_evidence(:topic_values, length(topics))
      else
        st
      end

    example_count = length(List.wrap(sense["examples"]))
    st =
      if example_count > 1 do
        st |> increment_evidence(:senses_with_multiple_examples)
           |> increment_evidence(:additional_examples, example_count - 1)
      else
        st
      end

    relationships = List.wrap(sense["synonyms"]) ++ List.wrap(sense["antonyms"])
    st =
      if Enum.any?(relationships, fn
           %{} = rel -> Enum.any?(Map.keys(rel), &(&1 != "word"))
           _ -> false
         end),
        do: increment_evidence(st, :senses_with_relationship_qualifiers),
        else: st

    st =
      if Enum.any?(@typed_relationship_keys, &present?(Map.get(sense, &1))),
        do: increment_evidence(st, :senses_with_other_typed_relationships),
        else: st

    Enum.reduce(List.wrap(sense["categories"]), st, fn category, acc ->
      acc = increment_evidence(acc, :category_values)

      case CategoryEvidencePolicy.decide(category) do
        {:emit, _class, _atom} ->
          increment_evidence(acc, :promoted_categories)

        {:skip, reason} ->
          acc
          |> increment_evidence(:withheld_categories)
          |> Map.update!(:withheld_reasons, fn reasons ->
            Map.update(reasons, reason, 1, &(&1 + 1))
          end)
      end
    end)
  end

  defp present?(value) when value in [nil, [], ""], do: false
  defp present?(_value), do: true

  defp increment_evidence(st, field, by \\ 1) do
    %{st | evidence: Map.update!(st.evidence, field, &(&1 + by))}
  end

  defp count_unprojected(st, entry, key, ignored) do
    frequencies =
      entry
      |> Enum.reduce(Map.fetch!(st, key), fn {field, value}, acc ->
        if is_binary(field) and field not in ignored and present?(value) do
          Map.update(acc, field, 1, &(&1 + 1))
        else
          acc
        end
      end)

    Map.put(st, key, frequencies)
  end

  defp flush(%{pending_count: 0} = st, _lookup), do: st

  defp flush(st, lookup) do
    cells = Enum.reverse(st.pending)
    ids = Enum.map(cells, & &1.id)
    stored = lookup.(ids)
    st = %{st | pending: [], pending_count: 0}

    Enum.reduce(cells, st, fn expected, acc ->
      case Map.get(stored, expected.id) do
        nil ->
          acc
          |> Map.update!(:missing_ids, &(&1 + 1))
          |> sample(:absent_id, %{id: expected.id, expected_definition: excerpt(expected.definition)})

        actual ->
          acc = %{acc | compared: acc.compared + 1}

          differences =
            Enum.filter(@fields, fn field ->
              not equivalent?(field, Map.get(expected, field), Map.get(actual, field))
            end)

          if differences == [] do
            %{acc | matching_rows: acc.matching_rows + 1}
          else
            Enum.reduce(differences, acc, fn field, state ->
              state
              |> Map.update!(:mismatches, fn counts ->
                Map.update!(counts, field, &(&1 + 1))
              end)
              |> sample(field, %{
                id: expected.id,
                expected: excerpt(Map.get(expected, field)),
                actual: excerpt(Map.get(actual, field))
              })
            end)
          end
      end
    end)
  end

  @doc false
  def equivalent?(field, expected, actual) when field in @array_fields do
    MapSet.new(List.wrap(expected)) == MapSet.new(List.wrap(actual))
  end

  def equivalent?(_field, expected, actual), do: expected == actual

  defp sample(st, field, row) do
    samples = Map.get(st.samples, field, [])

    if length(samples) < st.sample_limit do
      %{st | samples: Map.put(st.samples, field, samples ++ [row])}
    else
      st
    end
  end

  defp excerpt(value) when is_binary(value), do: String.slice(value, 0, 250)
  defp excerpt(value) when is_list(value), do: Enum.take(value, 12)
  defp excerpt(value), do: value

  @doc false
  def lookup_db(ids) do
    sql = """
    SELECT id, word::text, norm::text, pos, type, definition, example,
           synonyms, antonyms, gram_function, semantic_atoms
      FROM brain_cells
     WHERE id = ANY($1::text[])
    """

    result = Ecto.Adapters.SQL.query!(Db, sql, [ids], timeout: 120_000)

    Map.new(result.rows, fn
      [id, word, norm, pos, type, definition, example,
       synonyms, antonyms, gram_function, semantic_atoms] ->
        {id, %{
          word: word, norm: norm, pos: pos, type: type,
          definition: definition, example: example,
          synonyms: synonyms, antonyms: antonyms,
          gram_function: gram_function, semantic_atoms: semantic_atoms
        }}
    end)
  end

  defp count_db do
    result =
      Ecto.Adapters.SQL.query!(Db,
        "SELECT COUNT(*)::bigint FROM brain_cells", [], timeout: 120_000)

    [[count]] = result.rows
    count
  end
end
