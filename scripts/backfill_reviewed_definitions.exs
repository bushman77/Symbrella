# Explicitly approved, SINGLE-word/POS, definition-only recovery.
#
# READ ONLY by default. ALWAYS scans the FULL source for normalized variants:
#   mix run scripts/backfill_reviewed_definitions.exs --word point --pos noun
#   mix run scripts/backfill_reviewed_definitions.exs --word point --pos noun --show-all
#
# ONLY after separately inspecting ALL senses, tests and database backup:
#   mix run scripts/backfill_reviewed_definitions.exs --word point --pos noun \
#     --apply --fingerprint REVIEWED_SHA256 --expect-senses 59 --expect-patches 56
#
# This does NOT perform any bulk update. A single invocation is one word/POS,
# requiring the exact full-source sense count, patch count and plan fingerprint.
# DefinitionOnlyBackfill.apply/2 rechecks the fingerprint and guards current DB
# values in one transaction. Only definition and updated_at can change.

defmodule Symbrella.ReviewedDefinitionBackfillScript do
  @moduledoc false

  alias Db.JSONL.{
    DefinitionBatchAudit,
    DefinitionOnlyBackfill,
    DefinitionReviewGate,
    DefinitionWordReview,
    Util
  }

  def run(args) do
    args = case args do
      ["--" | rest] -> rest
      other -> other
    end

    {opts, rest, invalid} =
      OptionParser.parse(args,
        strict: [
          word: :string,
          pos: :string,
          apply: :boolean,
          fingerprint: :string,
          expect_senses: :integer,
          expect_patches: :integer,
          show_all: :boolean,
          preview: :integer
        ]
      )

    if rest != [] or invalid != [] do
      raise ArgumentError, usage()
    end

    raw_word = Keyword.get(opts, :word)
    raw_pos = Keyword.get(opts, :pos)
    apply? = Keyword.get(opts, :apply, false)
    preview = Keyword.get(opts, :preview, 8)

    unless is_binary(raw_word) and String.trim(raw_word) != "" and
             is_binary(raw_pos) and String.trim(raw_pos) != "" and
             is_integer(preview) and preview >= 0 and preview <= 100 do
      raise ArgumentError, usage()
    end

    if not apply? and
         Enum.any?([:fingerprint, :expect_senses, :expect_patches],
           &Keyword.has_key?(opts, &1)) do
      raise ArgumentError, "Approval arguments only make sense with --apply"
    end

    word = Util.normalize_word(raw_word)
    pos = raw_pos |> String.trim() |> String.downcase()
    path = Db.JSONL.IO.default_path()

    unless File.regular?(path), do: raise("Missing source JSONL: #{path}")

    IO.puts("\n=== Reviewed definition-only recovery: #{word}|#{pos} ===")
    IO.puts("Mode: #{if(apply?, do: "EXPLICIT APPROVAL CHECK", else: "READ ONLY")}")
    IO.puts("Source: #{path} (entire JSONL, all case variants)")
    IO.puts("Only definition + updated_at are permitted to change.")

    case DefinitionWordReview.collect(File.stream!(path, [], :line), word) do
      {:error, reason} ->
        abort("Source scan BLOCKED", reason)

      {:ok, entries, stats} ->
        IO.inspect(stats, label: "Full-source word census", pretty: true)

        {source, _counters} = DefinitionBatchAudit.source_cells(entries, %{})
        stored = Db.brain_cells_for_norm(word)

        case DefinitionOnlyBackfill.plan(word, pos, source, stored) do
          {:error, reason} ->
            abort("Sense/metadata conflict: no writes", reason)

          {:ok, plan} ->
            IO.inspect(%{
              word: plan.word,
              pos: plan.pos,
              total_senses: plan.row_count,
              definitions_to_extend: plan.patch_count,
              unchanged_senses: plan.row_count - plan.patch_count,
              fingerprint: plan.fingerprint
            }, label: "Current plan", pretty: true, width: 120)

            show_all? = Keyword.get(opts, :show_all, false) or apply?
            rows =
              cond do
                show_all? -> plan.rows
                true -> Enum.take(plan.patches, preview)
              end

            Enum.each(rows, fn row ->
              IO.inspect(%{
                id: row.id,
                changed?: row.changed?,
                old_definition: row.old_definition,
                proposed_definition: row.new_definition,
                example: row.example
              }, label: "Sense", pretty: true, limit: :infinity, width: 120)
            end)

            if not show_all? and length(rows) < plan.patch_count do
              IO.puts("Preview: #{length(rows)} of #{plan.patch_count} changes.")
              IO.puts("Inspect every sense with --show-all before approval.")
            end

            if apply? do
              # Explicit review authorization must match ALL reported
              # dimensions from a prior read-only full-source plan.
              case DefinitionReviewGate.validate(
                plan, word, pos,
                Keyword.get(opts, :expect_senses),
                Keyword.get(opts, :expect_patches),
                Keyword.get(opts, :fingerprint)
              ) do
                :ok ->
                  case DefinitionOnlyBackfill.apply(plan, Keyword.fetch!(opts, :fingerprint)) do
                    {:ok, count} ->
                      IO.puts("\nSUCCESS: updated #{count} definitions for #{word}|#{pos}.")
                      IO.puts("No categories, grammar or cognitive state was updated.")

                    {:error, reason} ->
                      abort("Transaction rolled back / update not applied", reason)
                  end

                {:error, reason} ->
                  abort("Approval BLOCKED: reviewed source or DB state changed", reason)
              end
            else
              IO.puts("\nDRY RUN: no database records changed.")
              IO.puts("Fingerprint is an audit identifier, not authorization.")
              IO.puts("Back up PostgreSQL and pass tests before any explicit approval.")
            end
        end
    end
  end

  defp abort(message, reason) do
    IO.inspect(reason, label: message, pretty: true, width: 120)
    IO.puts("No database changes committed by this invocation.")
    System.halt(2)
  end

  defp usage do
    "Usage: mix run scripts/backfill_reviewed_definitions.exs " <>
      "--word WORD --pos CANONICAL_POS " <>
      "[--show-all] [--preview N] " <>
      "[--apply --fingerprint SHA256 --expect-senses N --expect-patches N]"
  end
end

Symbrella.ReviewedDefinitionBackfillScript.run(System.argv())
