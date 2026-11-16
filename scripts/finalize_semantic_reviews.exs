# Freeze two independently filled review sheets against the original
# dictionary-backed packet. DOES NOT authenticate reviewer identity.
#
# mix run scripts/finalize_semantic_reviews.exs \
#   --packet tmp/semantic_review_packet.json \
#   --review-a tmp/review_a.completed.json \
#   --review-b tmp/review_b.completed.json \
#   --development tmp/semantic_development_hashes.json \
#   --out tmp/semantic_holdout.json
#
# All-or-nothing on disagreement: no cherry-picking easy cases.

alias Core.Agency.{SemanticHoldout, SemanticReview}

{opts, rest, errors} =
  OptionParser.parse(System.argv(),
    strict: [packet: :string, review_a: :string, review_b: :string,
             development: :string, out: :string, help: :boolean]
  )

required = [:packet, :review_a, :review_b, :development, :out]

cond do
  opts[:help] ->
    IO.puts("Usage: mix run scripts/finalize_semantic_reviews.exs --packet FILE --review-a FILE --review-b FILE --development FILE --out FILE")

  errors != [] or rest != [] or Enum.any?(required, &(not is_binary(opts[&1]))) ->
    IO.puts(:stderr, "Supply --packet, --review-a, --review-b, --development, and --out.")
    System.halt(2)

  true ->
    read_json = fn path ->
      with {:ok, data} <- File.read(path),
           {:ok, %{} = parsed} <- Jason.decode(data) do
        parsed
      else
        _ ->
          IO.puts(:stderr, "Invalid JSON object: #{path}")
          System.halt(2)
      end
    end

    packet = read_json.(opts[:packet])
    reviewer_a = read_json.(opts[:review_a])
    reviewer_b = read_json.(opts[:review_b])
    development = read_json.(opts[:development])
    hashes = Map.get(development, "development_utterance_sha256")

    case SemanticReview.finalize(packet, reviewer_a, reviewer_b, hashes) do
      {:error, reason} ->
        IO.puts(:stderr, "Review consensus rejected: #{inspect(reason)}")
        System.halt(2)

      {:ok, manifest} ->
        # Check that the actual dictionary has not changed since the review
        # packet was prepared, before freezing and saving a manifest.
        case SemanticReview.verify_current_dictionary(
          manifest, &Db.brain_cells_for_norm/1
        ) do
          :ok -> :ok
          {:error, reason} ->
            IO.puts(:stderr, "Dictionary inventory mismatch: #{inspect(reason)}")
            System.halt(2)
        end

        path = opts[:out]
        File.mkdir_p!(Path.dirname(path))

        case File.open(path, [:write, :exclusive, :binary]) do
          {:ok, io} ->
            try do
              :ok = IO.binwrite(io, Jason.encode!(manifest, pretty: true) <> "\n")
            after
              File.close(io)
            end
            File.chmod!(path, 0o600)

          {:error, reason} ->
            IO.puts(:stderr, "Refusing to overwrite or cannot create #{path}: #{inspect(reason)}")
            System.halt(2)
        end

        IO.puts("Frozen review manifest: #{path}")
        IO.puts("Save this separately BEFORE running the benchmark:")
        IO.puts(SemanticHoldout.seal(manifest))
        IO.puts("The SHA-256 detects changes; it does not authenticate human reviews.")
    end
end
