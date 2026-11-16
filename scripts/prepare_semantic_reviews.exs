# Build blind human-review packets from the actual current Db.BrainCell inventory.
#
# mix run scripts/prepare_semantic_reviews.exs \
#   --cases tmp/semantic_cases.json --out tmp/semantic_review_packet.json
#
# Writes packet plus TWO EMPTY REVIEWER TEMPLATES. It never queries LIFG or
# invents correct senses. These files may contain private utterances; keep local.

alias Core.Agency.SemanticReview

{opts, rest, errors} =
  OptionParser.parse(System.argv(),
    strict: [cases: :string, out: :string, help: :boolean]
  )

cond do
  opts[:help] ->
    IO.puts("Usage: mix run scripts/prepare_semantic_reviews.exs --cases FILE --out FILE")

  errors != [] or rest != [] or
      not is_binary(opts[:cases]) or not is_binary(opts[:out]) ->
    IO.puts(:stderr, "Supply --cases and --out. Use --help for usage.")
    System.halt(2)

  true ->
    input =
      with {:ok, data} <- File.read(opts[:cases]),
           {:ok, %{} = parsed} <- Jason.decode(data) do
        parsed
      else
        _ ->
          IO.puts(:stderr, "Expected --cases to contain a JSON object.")
          System.halt(2)
      end

    case SemanticReview.prepare(input, &Db.brain_cells_for_norm/1) do
      {:error, reason} ->
        IO.puts(:stderr, "Review packet rejected: #{inspect(reason)}")
        System.halt(2)

      {:ok, packet} ->
        packet_path = opts[:out]
        prefix = Path.rootname(packet_path)
        sheet_a = prefix <> ".reviewer_a.blank.json"
        sheet_b = prefix <> ".reviewer_b.blank.json"

        if Enum.any?([packet_path, sheet_a, sheet_b], &File.exists?/1) do
          IO.puts(:stderr, "Refusing to overwrite an existing packet or review sheet.")
          System.halt(2)
        end

        blank = SemanticReview.blank_sheet(packet)
        outputs = [{packet_path, packet}, {sheet_a, blank}, {sheet_b, blank}]

        Enum.each(outputs, fn {path, item} ->
          File.mkdir_p!(Path.dirname(path))

          case File.open(path, [:write, :exclusive, :binary]) do
            {:ok, io} ->
              try do
                :ok = IO.binwrite(io, Jason.encode!(item, pretty: true) <> "\n")
              after
                File.close(io)
              end
              File.chmod!(path, 0o600)

            {:error, reason} ->
              IO.puts(:stderr, "Cannot write #{path}: #{inspect(reason)}")
              System.halt(2)
          end
        end)

        IO.puts("Dictionary-backed review packet: #{packet_path}")
        IO.puts("Blank reviewer sheet A: #{sheet_a}")
        IO.puts("Blank reviewer sheet B: #{sheet_b}")
        IO.puts("Packet SHA-256: #{SemanticReview.packet_sha256(packet)}")
        IO.puts("No model predictions or gold labels were generated.")
    end
end
