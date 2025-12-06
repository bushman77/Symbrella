#!/usr/bin/env elixir
# Downloads Kaikki/Wiktextract raw data (multi-language), extracts TOP-LEVEL English entries,
# deletes the big download, then imports english.jsonl into brain_cells via Db.JSONL.
#
# Run (from umbrella root):
#   mix run apps/db/priv/scripts/bootstrap_english_jsonl.exs -- [opts]
#
# Default upstream (Kaikki raw data from English Wiktionary edition):
#   https://kaikki.org/dictionary/raw-wiktextract-data.jsonl.gz
#
# Notes:
# - Extraction is streaming (gzip -dc via Port), so we never materialize the huge .jsonl file.
# - Uses Jason to decode candidate lines and verify TOP-LEVEL lang_code/lang. This avoids the
#   false-positive case where a non-English entry contains English translations (nested fields).
# - Import uses Db.JSONL.import_all/1 after starting the app.

defmodule SymbrellaBootstrapEnglish do
  @default_url "https://kaikki.org/dictionary/raw-wiktextract-data.jsonl.gz"
  @default_every 200_000

  # Script lives in apps/db/priv/scripts. Derive priv dir reliably.
  @script_dir __DIR__
  @db_priv_dir Path.expand("..", @script_dir)      # .../apps/db/priv
  @db_priv_tmp_dir Path.join(@db_priv_dir, "tmp")  # .../apps/db/priv/tmp

  @default_big Path.join(@db_priv_tmp_dir, "raw-wiktextract-data.jsonl.gz")
  @default_out Path.join(@db_priv_dir, "english.jsonl")

  # Fast candidate prefilter; correctness is enforced by top-level JSON check.
  @rx_en_candidate ~r/"lang_code"\s*:\s*"en"|"lang"\s*:\s*"English"/

  def main(argv) do
    ensure_mix!()
    ensure_jason!()

    {opts, _rest, invalid} =
      OptionParser.parse(argv,
        strict: [
          url: :string,
          big: :string,
          out: :string,
          every: :integer,
          limit_lines: :integer,
          keep_big: :boolean,
          extract_only: :boolean,
          import_only: :boolean,
          migrate: :boolean,
          batch_size: :integer,
          validate: :boolean,
          returning: :boolean,
          on_db_error: :string,
          on_decode_error: :string,
          assume_sorted: :boolean
        ],
        aliases: [u: :url, b: :big, o: :out, e: :every]
      )

    if invalid != [] do
      IO.puts(:stderr, "Unknown args: #{inspect(invalid)}")
      System.halt(2)
    end

    url = opts[:url] || @default_url
    big_path = Path.expand(opts[:big] || @default_big)
    out_path = Path.expand(opts[:out] || @default_out)
    every = max(1, opts[:every] || @default_every)

    limit_lines =
      case opts[:limit_lines] do
        n when is_integer(n) and n > 0 -> n
        _ -> nil
      end

    keep_big? = !!opts[:keep_big]
    extract_only? = !!opts[:extract_only]
    import_only? = !!opts[:import_only]
    migrate? = Keyword.get(opts, :migrate, true)

    :ok = File.mkdir_p(Path.dirname(big_path))
    :ok = File.mkdir_p(Path.dirname(out_path))

    IO.puts(:stderr, "URL : #{url}")
    IO.puts(:stderr, "BIG : #{big_path}")
    IO.puts(:stderr, "OUT : #{out_path}")
    IO.puts(:stderr, "Mode: #{mode_label(extract_only?, import_only?)}")
    IO.puts(:stderr, "")

    cond do
      import_only? ->
        run_import!(out_path, opts, migrate?)

      true ->
        download_big!(url, big_path)
        extract_english!(big_path, out_path, every, limit_lines)

        unless keep_big? do
          rm_big!(big_path)
        end

        unless extract_only? do
          run_import!(out_path, opts, migrate?)
        end
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Download
  # ─────────────────────────────────────────────────────────────────────────────

  defp download_big!(url, dest) do
    IO.puts(:stderr, "Downloading…")
    exe = System.find_executable("curl") || System.find_executable("wget")

    unless exe do
      IO.puts(:stderr, "Missing downloader: install 'curl' or 'wget'.")
      System.halt(2)
    end

    {cmd, args} =
      case Path.basename(exe) do
        "curl" ->
          {exe,
           ["--fail", "--location", "--retry", "3", "--continue-at", "-", "--output", dest, url]}

        _ ->
          {exe, ["-O", dest, url]}
      end

    {_, code} = System.cmd(cmd, args, stderr_to_stdout: true, into: IO.stream(:stdio, :line))

    if code != 0 do
      IO.puts(:stderr, "\nDownload failed (exit #{code}).")
      System.halt(code)
    end

    size = File.stat!(dest).size
    IO.puts(:stderr, "\nDownloaded: #{format_bytes(size)}\n")
  end

  defp rm_big!(path) do
    case File.rm(path) do
      :ok ->
        IO.puts(:stderr, "Deleted big file: #{path}\n")

      {:error, reason} ->
        IO.puts(:stderr, "Warning: could not delete big file #{path}: #{inspect(reason)}\n")
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Extraction (gzip streaming)
  # ─────────────────────────────────────────────────────────────────────────────

  defp extract_english!(gz_path, out_path, every, limit_lines) do
    IO.puts(:stderr, "Extracting English entries (streaming gzip)…")
    start_ms = System.monotonic_time(:millisecond)

    tmp_out = out_path <> ".tmp"
    {:ok, out} = File.open(tmp_out, [:write, :binary])

    stream =
      gz_lines(gz_path)
      |> Stream.with_index(1)

    stream =
      if is_integer(limit_lines) do
        Stream.take(stream, limit_lines)
      else
        stream
      end

    {total, kept, bad_json} =
      Enum.reduce(stream, {0, 0, 0}, fn {line, idx}, {t, k, bad} ->
        t = t + 1

        cond do
          line == "" ->
            progress(idx, every, start_ms, t, k, bad)
            {t, k, bad}

          not Regex.match?(@rx_en_candidate, line) ->
            progress(idx, every, start_ms, t, k, bad)
            {t, k, bad}

          true ->
            case english_top_level?(line) do
              {:ok, true} ->
                IO.binwrite(out, [line, "\n"])
                k = k + 1
                progress(idx, every, start_ms, t, k, bad)
                {t, k, bad}

              {:ok, false} ->
                progress(idx, every, start_ms, t, k, bad)
                {t, k, bad}

              {:error, :bad_json} ->
                progress(idx, every, start_ms, t, k, bad + 1)
                {t, k, bad + 1}
            end
        end
      end)

    File.close(out)
    File.rename!(tmp_out, out_path)

    took_ms = System.monotonic_time(:millisecond) - start_ms
    out_size = File.stat!(out_path).size

    IO.puts(:stderr, "\nExtraction done.")
    IO.puts(:stderr, "  lines total: #{total}")
    IO.puts(:stderr, "  lines kept : #{kept}")
    IO.puts(:stderr, "  bad JSON   : #{bad_json}")
    IO.puts(:stderr, "  took       : #{format_ms(took_ms)}")
    IO.puts(:stderr, "  output size: #{format_bytes(out_size)}\n")
  end

  defp english_top_level?(line) do
    case Jason.decode(line) do
      {:ok, %{} = obj} ->
        lang_code = Map.get(obj, "lang_code")
        lang = Map.get(obj, "lang")
        {:ok, lang_code == "en" or lang == "English"}

      _ ->
        {:error, :bad_json}
    end
  end

  defp gz_lines(gz_path) do
    exe = System.find_executable("gzip") || System.find_executable("gunzip")

    unless exe do
      IO.puts(:stderr, "Missing decompressor: install 'gzip' (or 'gunzip').")
      System.halt(2)
    end

    args =
      case Path.basename(exe) do
        "gunzip" -> ["-c", gz_path]
        _ -> ["-dc", gz_path]
      end

    port = Port.open({:spawn_executable, exe}, [:binary, :exit_status, args: args])

    Stream.resource(
      fn -> %{port: port, buf: ""} end,
      fn state -> next_lines(state) end,
      fn %{port: p} -> safe_close_port(p) end
    )
  end

  defp next_lines(%{port: port, buf: buf} = state) do
    receive do
      {^port, {:data, chunk}} ->
        data = buf <> chunk

        case :binary.split(data, "\n", [:global]) do
          [only] ->
            {[], %{state | buf: only}}

          parts ->
            remainder = List.last(parts)
            lines = parts |> Enum.drop(-1) |> Enum.map(&String.trim_trailing(&1, "\r"))
            {lines, %{state | buf: remainder}}
        end

      {^port, {:exit_status, 0}} ->
        rem = String.trim_trailing(buf, "\r")

        if rem == "" do
          {:halt, state}
        else
          {[rem], %{state | buf: ""}}
        end

      {^port, {:exit_status, code}} ->
        IO.puts(:stderr, "gzip exited with status #{code}")
        System.halt(code)
    end
  end

  defp safe_close_port(port) do
    try do
      Port.close(port)
    rescue
      _ -> :ok
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Import
  # ─────────────────────────────────────────────────────────────────────────────

  defp run_import!(out_path, opts, migrate?) do
    unless File.exists?(out_path) do
      IO.puts(:stderr, "english.jsonl not found at: #{out_path}")
      System.halt(2)
    end

    IO.puts(:stderr, "Starting app…")
    Mix.Task.run("app.start")

    if migrate? do
      IO.puts(:stderr, "Running migrations…")
      Mix.Task.run("ecto.migrate", ["-r", "Db"])
    end

    IO.puts(:stderr, "Importing into brain_cells…")

    import_opts =
      [
        path: out_path,
        batch_size: opts[:batch_size] || 2_000,
        validate?: Keyword.get(opts, :validate, true),
        returning?: Keyword.get(opts, :returning, false),
        every: opts[:every] || 10_000,
        assume_sorted?: Keyword.get(opts, :assume_sorted, true),
        on_db_error: parse_db_err(opts[:on_db_error] || "count"),
        on_decode_error: parse_decode_err(opts[:on_decode_error] || "count")
      ]
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)

    case Db.JSONL.import_all(import_opts) do
      {:ok, summary} ->
        IO.puts(:stderr, "\nImport complete: #{inspect(summary)}")

      {:error, reason} ->
        IO.puts(:stderr, "\nImport failed: #{inspect(reason)}")
        System.halt(2)
    end
  end

  defp parse_db_err("raise"), do: :raise
  defp parse_db_err("halt"), do: :halt
  defp parse_db_err("count"), do: :count
  defp parse_db_err(other), do: raise(ArgumentError, "invalid --on-db-error #{inspect(other)}")

  defp parse_decode_err("skip"), do: :skip
  defp parse_decode_err("raise"), do: :raise
  defp parse_decode_err("count"), do: :count
  defp parse_decode_err(other),
    do: raise(ArgumentError, "invalid --on-decode-error #{inspect(other)}")

  # ─────────────────────────────────────────────────────────────────────────────
  # Guardrails
  # ─────────────────────────────────────────────────────────────────────────────

  defp ensure_mix! do
    unless Code.ensure_loaded?(Mix) do
      IO.puts(:stderr, "Run this script with Mix from the umbrella root:")
      IO.puts(:stderr, "  mix run apps/db/priv/scripts/bootstrap_english_jsonl.exs -- [opts]")
      System.halt(2)
    end
  end

  defp ensure_jason! do
    unless Code.ensure_loaded?(Jason) and function_exported?(Jason, :decode, 1) do
      IO.puts(:stderr, "Jason is required for safe top-level language filtering.")
      IO.puts(:stderr, "Add :jason to deps (or run in an environment where it is available).")
      System.halt(2)
    end
  end

  defp mode_label(true, _), do: "extract-only"
  defp mode_label(_, true), do: "import-only"
  defp mode_label(_, _), do: "download → extract → delete → import"

  defp progress(idx, every, start_ms, total, kept, bad) do
    if rem(idx, every) == 0 do
      now = System.monotonic_time(:millisecond)
      ms = max(now - start_ms, 1)
      rate = (total * 1000) / ms
      IO.puts(:stderr, "… #{idx} lines | kept #{kept} | bad #{bad} | #{Float.round(rate, 1)} lines/s")
    end
  end

  defp format_ms(ms) when is_integer(ms) and ms < 1_000, do: "#{ms}ms"
  defp format_ms(ms) when is_integer(ms) and ms < 60_000, do: "#{Float.round(ms / 1000, 2)}s"

  defp format_ms(ms) when is_integer(ms) do
    s = div(ms, 1000)
    m = div(s, 60)
    r = rem(s, 60)
    "#{m}m#{r}s"
  end

  defp format_bytes(n) when is_integer(n) and n < 1024, do: "#{n} B"
  defp format_bytes(n) when is_integer(n) and n < 1024 * 1024, do: "#{Float.round(n / 1024, 2)} KB"

  defp format_bytes(n) when is_integer(n) and n < 1024 * 1024 * 1024,
    do: "#{Float.round(n / (1024 * 1024), 2)} MB"

  defp format_bytes(n) when is_integer(n),
    do: "#{Float.round(n / (1024 * 1024 * 1024), 2)} GB"
end

SymbrellaBootstrapEnglish.main(System.argv())

