# Sealed lexical sense holdout experiment.
#
# REVIEW STEP (no model run, does not authenticate reviewer declarations):
#   mix run scripts/benchmark_semantic_holdout.exs --manifest tmp/semantic_holdout.json --print-seal
#
# EVALUATION (compare against a digest stored before the evaluation):
#   mix run scripts/benchmark_semantic_holdout.exs \
#     --manifest tmp/semantic_holdout.json \
#     --seal 64_HEX_CHARACTERS
#
# No model policy updates or explicit episode writes are requested. The normal
# Core production stack can still emit telemetry and transient process state.

alias Core.Agency.SemanticHoldout

{opts, rest, errors} =
  OptionParser.parse(System.argv(),
    strict: [manifest: :string, seal: :string, print_seal: :boolean,
             help: :boolean]
  )

cond do
  opts[:help] ->
    IO.puts("Usage: mix run scripts/benchmark_semantic_holdout.exs --manifest FILE (--print-seal | --seal SHA256)")

  errors != [] or rest != [] or not is_binary(opts[:manifest]) or
      (opts[:print_seal] and opts[:seal]) or
      (not opts[:print_seal] and not is_binary(opts[:seal])) ->
    IO.puts(:stderr, "Invalid arguments. Use --help for usage.")
    System.halt(2)

  true ->
    manifest =
      case File.read(opts[:manifest]) do
        {:ok, raw} ->
          case Jason.decode(raw) do
            {:ok, %{} = data} -> data
            _ ->
              IO.puts(:stderr, "Manifest must be a JSON object.")
              System.halt(2)
          end

        {:error, reason} ->
          IO.puts(:stderr, "Unable to read manifest: #{inspect(reason)}")
          System.halt(2)
      end

    case SemanticHoldout.validate_manifest(manifest) do
      {:error, reason} ->
        IO.puts(:stderr, "Rejected manifest: #{inspect(reason)}")
        System.halt(2)

      {:ok, _} ->
        :ok
    end

    if opts[:print_seal] do
      IO.puts("Manifest content digest (not an independent reviewer signature):")
      IO.puts(SemanticHoldout.seal(manifest))
    else
      # Evidence-gated runtime LIFG comparison. Only the sentence enters the
      # resolver; labels and reviewer declarations remain in the harness.
      resolver = fn sentence ->
        Core.resolve_input(sentence,
          mode: :prod,
          response: :off,
          session_id: nil,
          experience_origin: nil,
          fuzzy: false,
          episodes: false,
          persist_episodes: false,
          hippocampus_encode?: false,
          comprehension_wm?: false,
          neuron_activation?: false,
          notify_brain?: false,
          synapse_shadow?: false,
          synaptic_feedback?: false,
          self_model: :off,
          lifg_opts: [scores: :all, synapse_eval_full_scores?: true]
        )
      end

      result = SemanticHoldout.run(manifest, opts[:seal], resolver)

      IO.inspect(result,
        label: "Sealed lexical holdout result (review provenance unverified)",
        limit: :infinity, printable_limit: :infinity
      )

      if result.status != :evaluated_against_supplied_reviews,
        do: System.halt(2)
    end
end
