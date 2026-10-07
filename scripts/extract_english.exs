#!/usr/bin/env elixir
# Extract English entries from a Kaikki/Wiktextract JSONL file (streaming, LINE BY LINE).
#
# Standalone-safe:
# - No Mix deps required.
# - Optional JSON verification is only enabled if Jason is available at runtime.

defmodule ExtractEnglish do
  @rx_lang_code_en ~r/"lang_code"\s*:\s*"en"/
  @rx_lang_english ~r/"lang"\s*:\s*"English"/

  @default_in "~/kaikki.org-dictionary-English.jsonl"
  @default_out "~/english.jsonl"
  @default_every 200_000

  def main(argv) do
    {opts, _rest, invalid} =
      OptionParser.parse(argv,
        strict: [in: :string, out: :string, every: :integer, verify: :boolean],
        aliases: [i: :in, o: :out, e: :every, v: :verify]
      )

    if invalid != [] do
      IO.puts(:stderr, "Unknown args: #{inspect(invalid)}")
    end

    in_path = Path.expand(opts[:in] || @default_in)
    out_path = Path.expand(opts[:out] || @default_out)
    every = max(1, opts[:every] || @default_every)
    verify? = !!opts[:verify]
    have_jason? = json_available?()
    verify? = verify? and have_jason?

    ensure_input!(in_path)

    {:ok, stat_in} = File.stat(in_path)
    {:ok, out} = File.open(out_path, [:write, :binary])

    start_ms = System.monotonic_time(:millisecond)

    IO.puts(:stderr, "IN : #{in_path} (#{format_bytes(stat_in.size)})")
    IO.puts(:stderr, "OUT: #{out_path}")
    IO.puts(:stderr, "Mode: :line#{if verify?, do: " + JSON verify", else: ""}")

    if opts[:verify] && !have_jason? do
      IO.puts(:stderr, "note: --verify requested but Jason isn't available; skipping verification")
    end

    IO.puts(:stderr, "Filtering… (progress every #{every} lines)\n")

    {total, kept, bad_json} =
      in_path
      |> File.stream!([], :line) # ✅ one JSON object per line
      |> Stream.with_index(1)
      |> Enum.reduce({0, 0, 0}, fn {line, idx}, {t, k, bad} ->
        t = t + 1
        line = String.trim_trailing(line)

        cond do
          line == "" ->
            progress(idx, every, start_ms, t, k, bad)
            {t, k, bad}

          not english_line?(line) ->
            progress(idx, every, start_ms, t, k, bad)
            {t, k, bad}

          verify? and not json_ok?(line) ->
            progress(idx, every, start_ms, t, k, bad + 1)
            {t, k, bad + 1}

          true ->
            # Ensure exactly one line per JSON object in output
            IO.binwrite(out, line)
            IO.binwrite(out, "\n")
            k = k + 1
            progress(idx, every, start_ms, t, k, bad)
            {t, k, bad}
        end
      end)

    File.close(out)

    took_ms = System.monotonic_time(:millisecond) - start_ms

    IO.puts(:stderr, "\nDone.")
    IO.puts(:stderr, "  lines total: #{total}")
    IO.puts(:stderr, "  lines kept : #{kept}")
    IO.puts(:stderr, "  bad JSON   : #{bad_json}")
    IO.puts(:stderr, "  took       : #{format_ms(took_ms)}")
    IO.puts(:stderr, "  output size: #{out_path |> File.stat!() |> Map.fetch!(:size) |> format_bytes()}")
  end

  # --- selection ---

  defp english_line?(line) when is_binary(line) do
    # Fast, dependency-free filter. Good enough for Kaikki/Wiktextract lines.
    Regex.match?(@rx_lang_code_en, line) or Regex.match?(@rx_lang_english, line)
  end

  # --- verification (optional) ---

  defp json_available? do
    jason = Module.concat([:Jason])
    Code.ensure_loaded?(jason) and function_exported?(jason, :decode, 1)
  end

  defp json_ok?(line) do
    jason = Module.concat([:Jason])
    match?({:ok, _}, apply(jason, :decode, [line]))
  end

  # --- i/o and progress ---

  defp ensure_input!(path) do
    unless File.exists?(path) do
      IO.puts(:stderr, "Input file not found: #{path}")
      System.halt(2)
    end
  end

  defp progress(idx, every, start_ms, total, kept, bad) do
    if rem(idx, every) == 0 do
      now = System.monotonic_time(:millisecond)
      ms = max(now - start_ms, 1)
      rate = (total * 1000) / ms
      IO.puts(:stderr, "… #{idx} lines | kept #{kept} | bad #{bad} | #{Float.round(rate, 1)} lines/s")
    end
  end

  # --- format helpers ---

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

ExtractEnglish.main(System.argv())

