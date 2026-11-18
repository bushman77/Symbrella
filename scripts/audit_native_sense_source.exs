# Symbrella / development — READ-ONLY source evidence reconstruction pilot.
# Install to existing ~/Symbrella/scripts/audit_native_sense_source.exs.
# Run from umbrella root:
#   mix run --no-start scripts/audit_native_sense_source.exs
#   mix run --no-start scripts/audit_native_sense_source.exs --limit-lines 100000 --max-preview 200
#
# No database writes, no vocabulary learning, no model training, no migrations.
# Output is confined to two named files under EXISTING umbrella-root priv/.
# This is a REVIEW PREVIEW, not approved training labels.
# Positional source IDs may be different if Wiktextract source versions changed.

{opts, rest, invalid} =
  OptionParser.parse(System.argv(),
    strict: [source: :string, limit_lines: :integer, batch: :integer,
             max_preview: :integer, max_examples: :integer, progress: :integer]
  )

if rest != [] or invalid != [] do
  raise ArgumentError,
        "Usage: mix run --no-start scripts/audit_native_sense_source.exs " <>
          "[--source priv/english.jsonl] [--limit-lines N] [--batch N] " <>
          "[--max-preview N] [--max-examples N] [--progress N]"
end

source = Path.expand(Keyword.get(opts, :source, "priv/english.jsonl"))
limit = Keyword.get(opts, :limit_lines, 20_000)
batch = Keyword.get(opts, :batch, 300)
max_preview = Keyword.get(opts, :max_preview, 100)
max_examples = Keyword.get(opts, :max_examples, 12)
progress = Keyword.get(opts, :progress, 10_000)

unless limit >= 0 and batch in 1..2_000 and max_preview in 0..5_000 and
         max_examples in 1..100 and progress >= 0 do
  raise ArgumentError, "Invalid limits (batch 1..2000, max_preview 0..5000, max_examples 1..100)"
end

unless File.regular?(source) do
  raise "Missing #{source}. This audit requires the existing English JSONL. " <>
        "Do not run the DB importer to create it; extract English only after " <>
        "checking priv/raw-wiktextract-data.jsonl.gz."
end

{:ok, _} = Application.ensure_all_started(:db)
case Db.start_link() do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
  other -> raise "Cannot start Db: #{inspect(other)}"
end

db_name =
  case Ecto.Adapters.SQL.query!(Db, "SELECT current_database()", []).rows do
    [[name]] -> name
  end

# Keep preview files under EXISTING root priv. No mkdir and no writes elsewhere.
priv = Path.expand("priv")
unless File.dir?(priv), do: raise("Expected umbrella-root priv directory: #{priv}")
preview_path = Path.join(priv, "native_sense_evidence_preview.jsonl")
report_path = Path.join(priv, "native_sense_evidence_audit.json")

defmodule Symbrella.NativeSenseSourceAudit do
  @moduledoc false
  import Ecto.Query
  alias Db.JSONL.{Populate, SenseEvidenceReconciliation, Util}
  alias Db.NativeVocab

  def initial(max_preview, max_examples) do
    %{
      counters: %{}, pending: [], pending_count: 0,
      scanned_sha256_ctx: :crypto.hash_init(:sha256),
      scanned_lines: 0, english_entries: 0, skipped_non_english: 0,
      invalid_json: 0, source_senses: 0, checked: 0,
      verdicts: %{}, source_multi_gloss: 0, source_multi_example: 0,
      total_source_examples: 0, additional_examples: 0,
      preview_count: 0, review_candidates: 0, usable_single_target_examples: 0,
      unmatched_target_examples: 0, multiple_target_examples: 0,
      previews_by_verdict: %{},
      max_preview: max_preview, max_examples: max_examples
    }
  end

  def consume(st, %{} = entry, line_no, line, batch, device) do
    if entry["lang_code"] == "en" or entry["lang"] == "English" do
      word = Util.normalize_word(entry["word"] || "")

      if word == "" do
        %{st | english_entries: st.english_entries + 1}
      else
        meta =
          %{
            "word" => entry["word"], "pos" => entry["pos"],
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
          |> Enum.map(fn s -> Map.put(Util.ensure_map(s), "_meta", meta) end)

        {cells, counters} =
          Populate.populate_structs_from_senses_with_counters(senses, st.counters)

        new_pending =
          Enum.zip(cells, senses)
          |> Enum.map(fn {cell, sense} ->
            {cell, sense, line_no, Base.encode16(:crypto.hash(:sha256, line), case: :lower)}
          end)

        more_gloss = Enum.count(senses, &(length(glosses(&1)) > 1))
        multi_examples = Enum.count(senses, &(length(raw_examples(&1)) > 1))
        example_total = Enum.sum(Enum.map(senses, &(length(raw_examples(&1)))))

        st = %{
          st | counters: counters,
          english_entries: st.english_entries + 1,
          source_senses: st.source_senses + length(cells),
          source_multi_gloss: st.source_multi_gloss + more_gloss,
          source_multi_example: st.source_multi_example + multi_examples,
          total_source_examples: st.total_source_examples + example_total,
          additional_examples: st.additional_examples +
            Enum.sum(Enum.map(senses, &(max(length(raw_examples(&1)) - 1, 0)))),
          pending: Enum.reverse(new_pending, st.pending),
          pending_count: st.pending_count + length(new_pending)
        }

        if st.pending_count >= batch, do: flush(st, device), else: st
      end
    else
      %{st | skipped_non_english: st.skipped_non_english + 1}
    end
  end

  def flush(%{pending: []} = st, _device), do: st

  def flush(st, device) do
    records = Enum.reverse(st.pending)
    ids = Enum.map(records, fn {cell, _, _, _} -> cell.id end)
    # Maps by ID, with a bounded number of DB rows per query.
    stored =
      from(b in Db.BrainCell,
        where: b.id in ^ids,
        select: %{
          id: b.id, numeric_id: b.numeric_id, norm: b.norm, pos: b.pos,
          definition: b.definition, example: b.example,
          gram_function: b.gram_function, semantic_atoms: b.semantic_atoms
        }
      )
      |> Db.all()
      |> Map.new(&{&1.id, &1})

    Enum.reduce(records, %{st | pending: [], pending_count: 0}, fn
      {cell, sense, line_no, line_sha}, acc ->
        existing = Map.get(stored, cell.id)
        {verdict, _detail} = SenseEvidenceReconciliation.classify(cell, existing)
        acc = %{
          acc | checked: acc.checked + 1,
          verdicts: Map.update(acc.verdicts, verdict, 1, &(&1 + 1))
        }

        # Only aligned records enter the preview. Even 'matched_current' is
        # WEAK LABEL evidence until checked against source version and reviewed.
        aligned? = verdict in [:matched_current, :missing_source_evidence]
        # Prefer samples which actually expose evidence missing from a
        # single definition/example column. Still audit ALL source senses.
        richer_evidence? = length(glosses(sense)) > 1 or
          length(raw_examples(sense)) > 1
        if aligned? and richer_evidence? and acc.preview_count < acc.max_preview do
          examples =
            sense
            |> raw_examples()
            |> Enum.take(acc.max_examples)
            |> Enum.map(&prepare_example(&1, cell.norm))

          singles = Enum.count(examples, &(&1.span_status == "single_target"))
          absent = Enum.count(examples, &(&1.span_status == "target_absent"))
          ambiguous = Enum.count(examples, &(&1.span_status == "multiple_targets"))

          row = %{
            source_line: line_no,
            source_line_sha256: line_sha,
            brain_cell_id: cell.id,
            numeric_id: if(existing, do: existing.numeric_id, else: nil),
            norm: cell.norm, pos: cell.pos,
            reconciliation: Atom.to_string(verdict),
            evidence_status: "source_aligned_provisional_not_human_reviewed",
            definitions: %{full_projection: cell.definition,
                           gloss_path: glosses(sense),
                           raw_glosses: List.wrap(sense["raw_glosses"])},
            examples: examples,
            original_example_count: length(raw_examples(sense)),
            example_preview_truncated?: length(raw_examples(sense)) > acc.max_examples,
            tags: List.wrap(sense["tags"]), raw_tags: List.wrap(sense["raw_tags"]),
            topics: List.wrap(sense["topics"]),
            categories: List.wrap(sense["categories"]),
            semantic_atoms: cell.semantic_atoms,
            related_source_keys: Map.take(sense, ~w(hypernyms hyponyms meronyms
              holonyms coordinate_terms derived related form_of alt_of troponyms))
          }

          IO.binwrite(device, Jason.encode!(row) <> "\n")

          %{
            acc | preview_count: acc.preview_count + 1,
            review_candidates: acc.review_candidates + 1,
            usable_single_target_examples: acc.usable_single_target_examples + singles,
            unmatched_target_examples: acc.unmatched_target_examples + absent,
            multiple_target_examples: acc.multiple_target_examples + ambiguous,
            previews_by_verdict: Map.update(acc.previews_by_verdict, verdict, 1, &(&1 + 1))
          }
        else
          acc
        end
    end)
  end

  defp glosses(sense) do
    chosen =
      case sense["glosses"] do
        g when is_list(g) and g != [] -> g
        _ -> List.wrap(sense["raw_glosses"])
      end

    chosen
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp raw_examples(sense), do: List.wrap(sense["examples"])

  defp prepare_example(raw, word) do
    text =
      case raw do
        %{"text" => s} when is_binary(s) -> s
        %{"example" => s} when is_binary(s) -> s
        s when is_binary(s) -> s
        _ -> ""
      end

    tokens = NativeVocab.tokenize(text)
    target = NativeVocab.tokenize(word)
    width = length(target)

    spans =
      if width == 0 or length(tokens) < width do
        []
      else
        tokens
        |> Enum.chunk_every(width, 1, :discard)
        |> Enum.with_index()
        |> Enum.filter(fn {window, _i} -> window == target end)
        |> Enum.map(fn {_window, i} -> [i, i + width] end)
      end

    %{
      text: text,
      tokens: tokens,
      target_spans_half_open: spans,
      span_status: case length(spans) do
        0 -> "target_absent"
        1 -> "single_target"
        _ -> "multiple_targets"
      end,
      provenance: if(is_map(raw), do: Map.drop(raw, ["text", "example"]), else: %{})
    }
  end
end

IO.puts("Source evidence audit — READ ONLY database access")
IO.puts("DB: #{db_name}")
IO.puts("Source: #{source}")
IO.puts("Scope: #{if(limit == 0, do: "complete source", else: "first #{limit} lines")}")
IO.puts("Outputs: #{preview_path} and #{report_path}")
IO.puts("The preview is NOT approved training data.")

# Write temporary files in existing priv; atomically publish at successful end.
tmp_preview = preview_path <> ".tmp"
tmp_report = report_path <> ".tmp"
File.rm(tmp_preview)
File.rm(tmp_report)
{:ok, device} = File.open(tmp_preview, [:write, :binary])

state =
  try do
    stream = File.stream!(source, [:read], :line)
    stream = if limit == 0, do: stream, else: Stream.take(stream, limit)

    stream
    |> Stream.with_index(1)
    |> Enum.reduce(Symbrella.NativeSenseSourceAudit.initial(max_preview, max_examples),
      fn {line, i}, st ->
        st = %{st | scanned_lines: i,
          scanned_sha256_ctx: :crypto.hash_update(st.scanned_sha256_ctx, line)}
        st =
          case Jason.decode(line) do
            {:ok, %{} = entry} ->
              Symbrella.NativeSenseSourceAudit.consume(st, entry, i, line, batch, device)
            _ ->
              %{st | invalid_json: st.invalid_json + 1}
          end

        if progress > 0 and rem(i, progress) == 0 do
          IO.puts("Scanned #{i} lines, checked #{st.checked} senses, " <>
                  "previewed #{st.preview_count} records")
        end
        st
      end)
    |> Symbrella.NativeSenseSourceAudit.flush(device)
  after
    File.close(device)
  end

report = %{
  mode: "read_only_source_reconstruction_preview",
  db: db_name,
  source: source,
  source_file_bytes: File.stat!(source).size,
  scanned_prefix_sha256: Base.encode16(:crypto.hash_final(state.scanned_sha256_ctx), case: :lower),
  scanned_prefix_is_complete_source?: limit == 0,
  partial_scan: limit != 0,
  scanned_lines: state.scanned_lines,
  english_entries: state.english_entries,
  skipped_non_english: state.skipped_non_english,
  invalid_json: state.invalid_json,
  source_senses: state.source_senses,
  checked: state.checked,
  reconciliation_verdicts: Map.new(state.verdicts, fn {k, v} -> {Atom.to_string(k), v} end),
  source_multiple_glosses: state.source_multi_gloss,
  source_multiple_examples: state.source_multi_example,
  source_example_count: state.total_source_examples,
  additional_examples_current_braincell_does_not_store: state.additional_examples,
  preview_count: state.preview_count,
  preview_selection: "aligned sources with multiple glosses OR multiple examples",
  preview_reconciliation_verdicts: Map.new(state.previews_by_verdict,
    fn {k, v} -> {Atom.to_string(k), v} end),
  preview_single_target_examples: state.usable_single_target_examples,
  preview_no_target_examples: state.unmatched_target_examples,
  preview_multiple_target_examples: state.multiple_target_examples,
  preview_path: preview_path,
  warnings: [
    "Source positional IDs can drift between Wiktextract versions.",
    "A matched projection is not proof of identical original source sense.",
    "Preview data is weak supervision and must not be treated as gold labels.",
    "No training splits have been assigned; this is not a training dataset.",
    "The source example may contain a matched surface form but have ambiguous target occurrence."
  ],
  db_writes: false
}

File.write!(tmp_report, Jason.encode!(report, pretty: true) <> "\n")
File.rename!(tmp_preview, preview_path)
File.rename!(tmp_report, report_path)

IO.puts("\n=== Reconstruction evidence audit ===")
IO.inspect(Map.drop(report, [:warnings, :preview_path, :source]), pretty: true,
  limit: :infinity, width: 120)
IO.puts("\nReview sample: #{preview_path}")
IO.puts("Audit report: #{report_path}")
IO.puts("No DB rows, vocabulary IDs, embeddings or synapses were modified.")
