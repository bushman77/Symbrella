#!/usr/bin/env elixir

# Symbrella English dictionary bootstrap.
#
# Canonical local corpus files:
#
#   apps/db/priv/raw-wiktextract-data.jsonl.gz
#   apps/db/priv/english.jsonl
#
# Normal behaviour:
#
#   1. If english.jsonl already exists, reuse it.
#   2. If english.jsonl is missing:
#        a. reuse the raw Wiktextract dump if it exists
#        b. otherwise download it
#        c. extract English entries
#   3. Import english.jsonl into brain_cells unless --extract-only is used.
#
# The raw ~20 GB Wiktextract dump is KEPT by default.
#
# Run from the umbrella root:
#
#   mix run apps/db/priv/scripts/bootstrap_english_jsonl.exs --
#
# Useful modes:
#
#   --extract-only
#       Ensure english.jsonl exists but do not import it.
#
#   --import-only
#       Do not download/extract; import existing english.jsonl.
#
#   --force-extract
#       Rebuild english.jsonl from the existing raw corpus.
#
#   --force-download
#       Download a fresh raw corpus and rebuild english.jsonl.
#
#   --delete-big
#       Delete the raw .jsonl.gz after successful processing.
#       This is NOT the default.
#
# Notes:
#
# - Extraction is streaming via gzip/gunzip.
# - The huge raw JSONL is never materialized uncompressed.
# - Candidate English lines are JSON-decoded and checked at the TOP LEVEL.
# - Downloads use a .part file so an incomplete download is never mistaken
#   for a valid corpus.
# - Import remains the responsibility of Db.JSONL.

defmodule SymbrellaBootstrapEnglish do
  @default_url "https://kaikki.org/dictionary/raw-wiktextract-data.jsonl.gz"
  @default_every 200_000

  # This file:
  #
  #   apps/db/priv/scripts/bootstrap_english_jsonl.exs
  #
  # Therefore ".." is the canonical DB priv directory.
  @script_dir __DIR__
  @db_priv_dir Path.expand("..", @script_dir)

  @default_big Path.join(
                 @db_priv_dir,
                 "raw-wiktextract-data.jsonl.gz"
               )

  @default_out Path.join(
                 @db_priv_dir,
                 "english.jsonl"
               )

  # Cheap candidate prefilter.
  #
  # Correctness is enforced afterward by Jason decoding the entire line and
  # checking the top-level language fields.
  @rx_en_candidate ~r/"lang_code"\s*:\s*"en"|"lang"\s*:\s*"English"/

  def main(argv) do
    ensure_mix!()
    ensure_jason!()

    config = parse_args(argv)

    validate_modes!(config)

    File.mkdir_p!(Path.dirname(config.big_path))
    File.mkdir_p!(Path.dirname(config.out_path))

    log_config(config)

    cond do
      config.import_only? ->
        run_import!(
          config.out_path,
          config.opts,
          config.migrate?
        )

      true ->
        ensure_english_corpus!(config)

        unless config.extract_only? do
          run_import!(
            config.out_path,
            config.opts,
            config.migrate?
          )
        end

        if config.delete_big? do
          rm_big!(config.big_path)
        end
    end
  end

  # ===========================================================================
  # Configuration
  # ===========================================================================

  defp parse_args(argv) do
    {opts, _rest, invalid} =
      OptionParser.parse(
        argv,
        strict: [
          url: :string,
          big: :string,
          out: :string,
          every: :integer,
          limit_lines: :integer,

          # Pipeline modes
          extract_only: :boolean,
          import_only: :boolean,

          # Corpus lifecycle
          force_download: :boolean,
          force_extract: :boolean,
          delete_big: :boolean,

          # Kept for compatibility with the old script.
          # Keeping the raw corpus is now the default.
          keep_big: :boolean,

          # DB import
          migrate: :boolean,
          batch_size: :integer,
          validate: :boolean,
          returning: :boolean,
          on_db_error: :string,
          on_decode_error: :string,
          assume_sorted: :boolean
        ],
        aliases: [
          u: :url,
          b: :big,
          o: :out,
          e: :every
        ]
      )

    if invalid != [] do
      IO.puts(
        :stderr,
        "Unknown args: #{inspect(invalid)}"
      )

      System.halt(2)
    end

    limit_lines =
      case opts[:limit_lines] do
        n when is_integer(n) and n > 0 ->
          n

        _ ->
          nil
      end

    # Raw files are retained by default now.
    #
    # --keep-big remains accepted for compatibility and wins if somebody
    # supplies both switches accidentally.
    delete_big? =
      !!opts[:delete_big] and
        not (!!opts[:keep_big])

    %{
      opts: opts,
      url: opts[:url] || @default_url,
      big_path: Path.expand(opts[:big] || @default_big),
      out_path: Path.expand(opts[:out] || @default_out),
      every:
        max(
          1,
          opts[:every] || @default_every
        ),
      limit_lines: limit_lines,
      extract_only?: !!opts[:extract_only],
      import_only?: !!opts[:import_only],
      force_download?: !!opts[:force_download],
      force_extract?: !!opts[:force_extract],
      delete_big?: delete_big?,
      migrate?:
        Keyword.get(
          opts,
          :migrate,
          true
        )
    }
  end

  defp validate_modes!(config) do
    if config.extract_only? and config.import_only? do
      IO.puts(
        :stderr,
        "--extract-only and --import-only cannot be used together."
      )

      System.halt(2)
    end

    if config.import_only? and
         (config.force_download? or config.force_extract?) do
      IO.puts(
        :stderr,
        "--import-only cannot be combined with --force-download or --force-extract."
      )

      System.halt(2)
    end
  end

  defp log_config(config) do
    IO.puts(:stderr, "Symbrella dictionary bootstrap")
    IO.puts(:stderr, "")
    IO.puts(:stderr, "URL : #{config.url}")
    IO.puts(:stderr, "RAW : #{config.big_path}")
    IO.puts(:stderr, "OUT : #{config.out_path}")
    IO.puts(:stderr, "Mode: #{mode_label(config)}")
    IO.puts(:stderr, "")
  end

  # ===========================================================================
  # Corpus lifecycle
  # ===========================================================================

  defp ensure_english_corpus!(config) do
    needs_extract? =
      config.force_download? or
        config.force_extract? or
        is_integer(config.limit_lines) or
        not usable_file?(config.out_path)

    if needs_extract? do
      ensure_raw_corpus!(config)

      extract_english!(
        config.big_path,
        config.out_path,
        config.every,
        config.limit_lines
      )
    else
      size =
        config.out_path
        |> File.stat!()
        |> Map.fetch!(:size)

      IO.puts(:stderr, "English corpus already exists.")
      IO.puts(:stderr, "  FILE: #{config.out_path}")
      IO.puts(:stderr, "  SIZE: #{format_bytes(size)}")
      IO.puts(:stderr, "  ACTION: reuse")
      IO.puts(:stderr, "")
    end
  end

  defp ensure_raw_corpus!(%{force_download?: true} = config) do
    IO.puts(:stderr, "Fresh raw corpus requested.")
    IO.puts(:stderr, "")

    # A forced refresh must not resume an old partial download.
    File.rm(config.big_path <> ".part")

    download_big!(
      config.url,
      config.big_path
    )
  end

  defp ensure_raw_corpus!(config) do
    if usable_file?(config.big_path) do
      size =
        config.big_path
        |> File.stat!()
        |> Map.fetch!(:size)

      IO.puts(:stderr, "Raw Wiktextract corpus already exists.")
      IO.puts(:stderr, "  FILE: #{config.big_path}")
      IO.puts(:stderr, "  SIZE: #{format_bytes(size)}")
      IO.puts(:stderr, "  ACTION: reuse")
      IO.puts(:stderr, "")
    else
      IO.puts(:stderr, "Raw Wiktextract corpus not found.")
      IO.puts(:stderr, "Downloading it now.")
      IO.puts(:stderr, "")

      download_big!(
        config.url,
        config.big_path
      )
    end
  end

  defp usable_file?(path) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :regular, size: size}}
      when size > 0 ->
        true

      _ ->
        false
    end
  end

  # ===========================================================================
  # Download
  # ===========================================================================

  defp download_big!(url, dest) do
    File.mkdir_p!(Path.dirname(dest))

    part = dest <> ".part"

    IO.puts(:stderr, "Downloading Wiktextract corpus…")
    IO.puts(:stderr, "  URL : #{url}")
    IO.puts(:stderr, "  TEMP: #{part}")
    IO.puts(:stderr, "  DEST: #{dest}")
    IO.puts(:stderr, "")

    exe =
      System.find_executable("curl") ||
        System.find_executable("wget")

    unless exe do
      IO.puts(
        :stderr,
        "Missing downloader: install curl or wget."
      )

      System.halt(2)
    end

    {cmd, args} =
      case Path.basename(exe) do
        "curl" ->
          {
            exe,
            [
              "--fail",
              "--location",
              "--retry",
              "3",

              # Resume a previous incomplete .part download.
              "--continue-at",
              "-",
              "--output",
              part,
              url
            ]
          }

        _ ->
          {
            exe,
            [
              "-O",
              part,
              url
            ]
          }
      end

    {_, code} =
      System.cmd(
        cmd,
        args,
        stderr_to_stdout: true,
        into: IO.stream(:stdio, :line)
      )

    if code != 0 do
      IO.puts(
        :stderr,
        "\nDownload failed (exit #{code})."
      )

      IO.puts(
        :stderr,
        "Partial download remains at:"
      )

      IO.puts(
        :stderr,
        "  #{part}"
      )

      System.halt(code)
    end

    unless usable_file?(part) do
      IO.puts(
        :stderr,
        "Download completed but the temporary file is empty or invalid."
      )

      System.halt(2)
    end

    # Only replace a known-good existing raw corpus AFTER the new download
    # completed successfully.
    File.rm(dest)
    File.rename!(part, dest)

    size =
      dest
      |> File.stat!()
      |> Map.fetch!(:size)

    IO.puts(
      :stderr,
      "\nDownloaded: #{format_bytes(size)}"
    )

    IO.puts(:stderr, "")
  end

  defp rm_big!(path) do
    case File.rm(path) do
      :ok ->
        IO.puts(
          :stderr,
          "Deleted raw corpus: #{path}"
        )

        IO.puts(:stderr, "")

      {:error, :enoent} ->
        :ok

      {:error, reason} ->
        IO.puts(
          :stderr,
          "Warning: could not delete raw corpus #{path}: #{inspect(reason)}"
        )

        IO.puts(:stderr, "")
    end
  end

  # ===========================================================================
  # English extraction
  # ===========================================================================

  defp extract_english!(
         gz_path,
         out_path,
         every,
         limit_lines
       ) do
    IO.puts(
      :stderr,
      "Extracting top-level English entries…"
    )

    IO.puts(:stderr, "")

    start_ms =
      System.monotonic_time(:millisecond)

    tmp_out =
      out_path <> ".tmp"

    # A previous failed extraction must never be treated as the finished corpus.
    File.rm(tmp_out)

    {:ok, out} =
      File.open(
        tmp_out,
        [:write, :binary]
      )

    stream =
      gz_lines(gz_path)
      |> Stream.with_index(1)

    stream =
      if is_integer(limit_lines) do
        Stream.take(
          stream,
          limit_lines
        )
      else
        stream
      end

    {total, kept, bad_json} =
      try do
        Enum.reduce(
          stream,
          {0, 0, 0},
          fn {line, idx}, {t, k, bad} ->
            t = t + 1

            result =
              cond do
                line == "" ->
                  {t, k, bad}

                not Regex.match?(
                  @rx_en_candidate,
                  line
                ) ->
                  {t, k, bad}

                true ->
                  case english_top_level?(line) do
                    {:ok, true} ->
                      IO.binwrite(
                        out,
                        [line, "\n"]
                      )

                      {t, k + 1, bad}

                    {:ok, false} ->
                      {t, k, bad}

                    {:error, :bad_json} ->
                      {t, k, bad + 1}
                  end
              end

            progress(
              idx,
              every,
              start_ms,
              elem(result, 0),
              elem(result, 1),
              elem(result, 2)
            )

            result
          end
        )
      after
        File.close(out)
      end

    unless usable_file?(tmp_out) do
      IO.puts(
        :stderr,
        "Extraction produced an empty output file."
      )

      System.halt(2)
    end

    # Publish atomically-ish only after successful extraction.
    File.rm(out_path)
    File.rename!(tmp_out, out_path)

    took_ms =
      System.monotonic_time(:millisecond) - start_ms

    out_size =
      out_path
      |> File.stat!()
      |> Map.fetch!(:size)

    IO.puts(:stderr, "")
    IO.puts(:stderr, "Extraction done.")
    IO.puts(:stderr, "  lines total: #{total}")
    IO.puts(:stderr, "  lines kept : #{kept}")
    IO.puts(:stderr, "  bad JSON   : #{bad_json}")
    IO.puts(:stderr, "  took       : #{format_ms(took_ms)}")
    IO.puts(:stderr, "  output size: #{format_bytes(out_size)}")
    IO.puts(:stderr, "  output file: #{out_path}")
    IO.puts(:stderr, "")
  end

  defp english_top_level?(line) do
    case Jason.decode(line) do
      {:ok, %{} = obj} ->
        lang_code =
          Map.get(
            obj,
            "lang_code"
          )

        lang =
          Map.get(
            obj,
            "lang"
          )

        {:ok,
         lang_code == "en" or
           lang == "English"}

      _ ->
        {:error, :bad_json}
    end
  end

  # ===========================================================================
  # gzip streaming
  # ===========================================================================

  defp gz_lines(gz_path) do
    exe =
      System.find_executable("gzip") ||
        System.find_executable("gunzip")

    unless exe do
      IO.puts(
        :stderr,
        "Missing decompressor: install gzip or gunzip."
      )

      System.halt(2)
    end

    args =
      case Path.basename(exe) do
        "gunzip" ->
          ["-c", gz_path]

        _ ->
          ["-dc", gz_path]
      end

    port =
      Port.open(
        {:spawn_executable, exe},
        [
          :binary,
          :exit_status,
          args: args
        ]
      )

    Stream.resource(
      fn ->
        %{
          port: port,
          buf: ""
        }
      end,
      &next_lines/1,
      fn %{port: p} ->
        safe_close_port(p)
      end
    )
  end

  defp next_lines(%{port: port, buf: buf} = state) do
    receive do
      {^port, {:data, chunk}} ->
        data =
          buf <> chunk

        case :binary.split(
               data,
               "\n",
               [:global]
             ) do
          [only] ->
            {
              [],
              %{state | buf: only}
            }

          parts ->
            remainder =
              List.last(parts)

            lines =
              parts
              |> Enum.drop(-1)
              |> Enum.map(
                &String.trim_trailing(
                  &1,
                  "\r"
                )
              )

            {
              lines,
              %{state | buf: remainder}
            }
        end

      {^port, {:exit_status, 0}} ->
        remainder =
          String.trim_trailing(
            buf,
            "\r"
          )

        if remainder == "" do
          {:halt, state}
        else
          {
            [remainder],
            %{state | buf: ""}
          }
        end

      {^port, {:exit_status, code}} ->
        IO.puts(
          :stderr,
          "gzip exited with status #{code}"
        )

        System.halt(code)
    end
  end

  defp safe_close_port(port) do
    try do
      Port.close(port)
    rescue
      _ ->
        :ok
    end
  end

  # ===========================================================================
  # DB import
  # ===========================================================================

  defp run_import!(
         out_path,
         opts,
         migrate?
       ) do
    unless usable_file?(out_path) do
      IO.puts(
        :stderr,
        "english.jsonl not found or empty at:"
      )

      IO.puts(
        :stderr,
        "  #{out_path}"
      )

      System.halt(2)
    end

    IO.puts(
      :stderr,
      "Starting Symbrella application…"
    )

    Mix.Task.run("app.start")

    if migrate? do
      IO.puts(
        :stderr,
        "Running DB migrations…"
      )

      Mix.Task.run(
        "ecto.migrate",
        ["-r", "Db"]
      )
    end

    IO.puts(
      :stderr,
      "Importing english.jsonl into brain_cells…"
    )

    import_opts =
      [
        path: out_path,
        batch_size:
          opts[:batch_size] ||
            2_000,
        validate?:
          Keyword.get(
            opts,
            :validate,
            true
          ),
        returning?:
          Keyword.get(
            opts,
            :returning,
            false
          ),
        every:
          opts[:every] ||
            10_000,
        assume_sorted?:
          Keyword.get(
            opts,
            :assume_sorted,
            true
          ),
        on_db_error:
          parse_db_err(
            opts[:on_db_error] ||
              "count"
          ),
        on_decode_error:
          parse_decode_err(
            opts[:on_decode_error] ||
              "count"
          )
      ]
      |> Enum.reject(fn {_k, v} ->
        is_nil(v)
      end)

    case Db.JSONL.import_all(import_opts) do
      {:ok, summary} ->
        IO.puts(
          :stderr,
          "\nImport complete:"
        )

        IO.inspect(
          summary,
          pretty: true,
          limit: :infinity
        )

      {:error, reason} ->
        IO.puts(
          :stderr,
          "\nImport failed: #{inspect(reason)}"
        )

        System.halt(2)
    end
  end

  defp parse_db_err("raise"), do: :raise
  defp parse_db_err("halt"), do: :halt
  defp parse_db_err("count"), do: :count

  defp parse_db_err(other) do
    raise(
      ArgumentError,
      "invalid --on-db-error #{inspect(other)}"
    )
  end

  defp parse_decode_err("skip"), do: :skip
  defp parse_decode_err("raise"), do: :raise
  defp parse_decode_err("count"), do: :count

  defp parse_decode_err(other) do
    raise(
      ArgumentError,
      "invalid --on-decode-error #{inspect(other)}"
    )
  end

  # ===========================================================================
  # Guardrails
  # ===========================================================================

  defp ensure_mix! do
    unless Code.ensure_loaded?(Mix) do
      IO.puts(
        :stderr,
        "Run this script with Mix from the Symbrella umbrella root:"
      )

      IO.puts(
        :stderr,
        "  mix run apps/db/priv/scripts/bootstrap_english_jsonl.exs -- [opts]"
      )

      System.halt(2)
    end
  end

  defp ensure_jason! do
    unless Code.ensure_loaded?(Jason) and
             function_exported?(
               Jason,
               :decode,
               1
             ) do
      IO.puts(
        :stderr,
        "Jason is required for safe top-level language filtering."
      )

      System.halt(2)
    end
  end

  # ===========================================================================
  # Display helpers
  # ===========================================================================

  defp mode_label(%{import_only?: true}),
    do: "import existing english.jsonl"

  defp mode_label(%{extract_only?: true, force_download?: true}),
    do: "fresh download → extract only"

  defp mode_label(%{extract_only?: true}),
    do: "ensure corpus → extract only"

  defp mode_label(%{force_download?: true}),
    do: "fresh download → extract → import"

  defp mode_label(%{force_extract?: true}),
    do: "reuse/download raw → force extract → import"

  defp mode_label(_),
    do: "reuse available corpus → import"

  defp progress(
         idx,
         every,
         start_ms,
         total,
         kept,
         bad
       ) do
    if rem(idx, every) == 0 do
      now =
        System.monotonic_time(:millisecond)

      ms =
        max(
          now - start_ms,
          1
        )

      rate =
        total * 1000 / ms

      IO.puts(
        :stderr,
        "… #{idx} lines | kept #{kept} | bad #{bad} | #{Float.round(rate, 1)} lines/s"
      )
    end
  end

  defp format_ms(ms)
       when is_integer(ms) and
              ms < 1_000,
       do: "#{ms}ms"

  defp format_ms(ms)
       when is_integer(ms) and
              ms < 60_000,
       do: "#{Float.round(ms / 1000, 2)}s"

  defp format_ms(ms)
       when is_integer(ms) do
    seconds =
      div(
        ms,
        1000
      )

    minutes =
      div(
        seconds,
        60
      )

    remaining_seconds =
      rem(
        seconds,
        60
      )

    "#{minutes}m#{remaining_seconds}s"
  end

  defp format_bytes(n)
       when is_integer(n) and
              n < 1024,
       do: "#{n} B"

  defp format_bytes(n)
       when is_integer(n) and
              n < 1024 * 1024,
       do: "#{Float.round(n / 1024, 2)} KB"

  defp format_bytes(n)
       when is_integer(n) and
              n < 1024 * 1024 * 1024,
       do: "#{Float.round(n / (1024 * 1024), 2)} MB"

  defp format_bytes(n)
       when is_integer(n),
       do: "#{Float.round(n / (1024 * 1024 * 1024), 2)} GB"
end

SymbrellaBootstrapEnglish.main(System.argv())
