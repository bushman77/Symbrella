defmodule Db.JSONL.Import do
  @moduledoc false

  alias Db.JSONL.{Populate, Upsert, Util}

  @spec import_all(keyword()) :: {:ok, map()} | {:error, any()}
  def import_all(opts \\ []) do
    path = Db.JSONL.IO.resolve_path(opts)

    unless File.exists?(path) do
      {:error, {:no_such_file, path}}
    else
      if not Db.JSONL.IO.jason_available?() do
        {:error, :jason_not_available}
      else
        repo = Keyword.get(opts, :repo, Db)
        every = Keyword.get(opts, :every, 50_000)
        batch_size = Keyword.get(opts, :batch_size, 500)
        assume_sorted? = Keyword.get(opts, :assume_sorted?, true)
        limit_words = Keyword.get(opts, :limit_words, :infinity)
        on_err = Keyword.get(opts, :on_decode_error, :skip)

        start_ms = System.monotonic_time(:millisecond)

        st0 = %{
          repo: repo,
          path: path,
          every: every,
          batch_size: batch_size,
          assume_sorted?: assume_sorted?,
          on_err: on_err,
          limit_words: limit_words,
          start_ms: start_ms,
          total_lines: 0,
          bad_json: 0,
          words_done: 0,
          current_word: nil,
          group_entries: [],
          pending_rows: [],
          pending_count: 0,
          upserted: 0,
          db_errors: 0,
          invalid_skipped: 0,
          batches: 0,
          counters: %{}
        }

        st1 =
          path
          |> Db.JSONL.IO.stream_lines()
          |> Stream.with_index(1)
          |> Enum.reduce_while(st0, fn {line, idx}, st ->
            st = maybe_progress(st, idx)

            case Jason.decode(line) do
              {:ok, %{} = entry} ->
                st = %{st | total_lines: st.total_lines + 1}
                st = add_entry_to_group(st, entry, opts)

                if st.limit_words != :infinity and st.words_done >= st.limit_words do
                  {:halt, st}
                else
                  {:cont, st}
                end

              {:error, _} ->
                st = %{st | total_lines: st.total_lines + 1, bad_json: st.bad_json + 1}

                case st.on_err do
                  :raise ->
                    raise "JSON decode failed for line beginning: #{String.slice(line, 0, 200)}"

                  :skip ->
                    {:cont, st}

                  :count ->
                    {:cont, st}
                end

              _ ->
                {:cont, %{st | total_lines: st.total_lines + 1}}
            end
          end)

        st2 =
          st1
          |> flush_group(opts)
          |> flush_pending(opts)

        took_ms = System.monotonic_time(:millisecond) - start_ms

        {:ok,
         %{
           path: path,
           total_lines: st2.total_lines,
           bad_json: st2.bad_json,
           words_done: st2.words_done,
           upserted: st2.upserted,
           batches: st2.batches,
           db_errors: st2.db_errors,
           invalid_skipped: st2.invalid_skipped,
           took_ms: took_ms
         }}
      end
    end
  end

  # ───────────────────────── internals: import grouping / db batching ─────────────────────────

  defp maybe_progress(st, idx) do
    if st.every > 0 and rem(idx, st.every) == 0 do
      now = System.monotonic_time(:millisecond)
      ms = max(now - st.start_ms, 1)
      rate = st.total_lines * 1000 / ms

      IO.puts(
        :stderr,
        "… #{idx} lines | words #{st.words_done} | upserted #{st.upserted} | bad #{st.bad_json} | #{Float.round(rate, 1)} lines/s"
      )
    end

    st
  end

  defp add_entry_to_group(st, %{} = entry, opts) do
    word = Util.normalize_word(Map.get(entry, "word") || "")

    cond do
      word == "" ->
        st

      st.current_word == nil ->
        %{st | current_word: word, group_entries: [entry]}

      word == st.current_word ->
        %{st | group_entries: [entry | st.group_entries]}

      true ->
        st
        |> flush_group(opts)
        |> then(fn st2 -> %{st2 | current_word: word, group_entries: [entry]} end)
    end
  end

  defp flush_group(%{current_word: nil} = st, _opts), do: st

  defp flush_group(st, opts) do
    entries = Enum.reverse(st.group_entries)
    _ = if st.assume_sorted? == false, do: :ok, else: :ok

    senses = entries_to_senses(entries)

    {cells, counters2} =
      Populate.populate_structs_from_senses_with_counters(
        senses,
        st.counters,
        Keyword.put_new(opts, :fill_missing_relations?, true)
      )

    validate? = Keyword.get(opts, :validate?, false)
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    {rows, invalid_count} =
      if validate? do
        Enum.reduce(cells, {[], 0}, fn cell, {acc, bad} ->
          attrs = Upsert.cell_to_attrs(cell, now)
          cs = Db.BrainCell.changeset(%Db.BrainCell{}, attrs)

          if cs.valid? do
            {[cs.changes | acc], bad}
          else
            {acc, bad + 1}
          end
        end)
        |> then(fn {ok_rows_rev, bad} -> {Enum.reverse(ok_rows_rev), bad} end)
      else
        {Enum.map(cells, &Upsert.cell_to_attrs(&1, now)), 0}
      end

    st =
      st
      |> Map.put(:counters, counters2)
      |> add_pending(rows)
      |> Map.update!(:invalid_skipped, &(&1 + invalid_count))
      |> Map.update!(:words_done, &(&1 + 1))
      |> Map.put(:group_entries, [])
      |> flush_pending(opts)

    if st.limit_words != :infinity and st.words_done >= st.limit_words do
      st
    else
      %{st | current_word: nil, group_entries: []}
    end
  end

  defp entries_to_senses(entries) do
    Enum.flat_map(entries, fn entry ->
      meta =
        %{
          "word" => Map.get(entry, "word"),
          "pos" => Map.get(entry, "pos"),
          "etymology_number" => Map.get(entry, "etymology_number"),
          "entry_synonyms" => Util.flatten_term_list(Map.get(entry, "synonyms")),
          "entry_antonyms" => Util.flatten_term_list(Map.get(entry, "antonyms"))
        }
        |> Enum.reject(fn {_k, v} -> is_nil(v) end)
        |> Map.new()

      entry
      |> Map.get("senses", [])
      |> List.wrap()
      |> Enum.map(fn s -> s |> Util.ensure_map() |> Map.put("_meta", meta) end)
    end)
  end

  defp add_pending(st, rows) do
    %{st | pending_rows: st.pending_rows ++ rows, pending_count: st.pending_count + length(rows)}
  end

  defp flush_pending(st, opts) do
    batch_size = Keyword.get(opts, :batch_size, st.batch_size)
    returning? = Keyword.get(opts, :returning?, false)
    on_db_error = Keyword.get(opts, :on_db_error, :raise)

    if st.pending_count < batch_size do
      st
    else
      {to_send, rest} = Enum.split(st.pending_rows, batch_size)

      case Upsert.safe_insert_all(st.repo, to_send, returning?, on_db_error) do
        {:ok, {n, _returned}} ->
          st
          |> Map.update!(:upserted, &(&1 + n))
          |> Map.update!(:batches, &(&1 + 1))
          |> Map.put(:pending_rows, rest)
          |> Map.put(:pending_count, length(rest))
          |> flush_pending(opts)

        {:error, _e} when on_db_error == :count ->
          st
          |> Map.update!(:db_errors, &(&1 + 1))
          |> Map.update!(:batches, &(&1 + 1))
          |> Map.put(:pending_rows, rest)
          |> Map.put(:pending_count, length(rest))
          |> flush_pending(opts)

        {:error, e} when on_db_error == :halt ->
          throw({:db_error, e, %{upserted_so_far: st.upserted, batches: st.batches}})

        {:error, e} ->
          raise e
      end
    end
  end
end
