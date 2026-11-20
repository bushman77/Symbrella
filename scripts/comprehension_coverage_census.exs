# R-001 Stage 0 (P-022): offline comprehension coverage census.
#
#   mix run scripts/comprehension_coverage_census.exs
#   mix run scripts/comprehension_coverage_census.exs \
#     --corpus docs/collab/r001/census_corpus.md --arm all --out tmp/r001_census.json
#
# Read-only diagnostic. It records which comprehension dimensions succeed for
# each corpus row; it does not judge semantic correctness and never changes
# lexical winners, memory or responses.
#
# Chat rows run Core.resolve_input/2 with the same persistence-disabled options
# as scripts/benchmark_semantic_holdout.exs. As that script documents, the
# production stack can still emit telemetry and transient process state, so
# run this on a development instance, not during live conversations.
# Colony rows run the existing read-only Core.Social.SemanticProbe.
#
# Output contains case IDs, corpus labels and enumerated statuses only, never
# sentences. Do not commit --out files that were produced from private rows.

alias Core.Comprehension.CoverageLadder

defmodule Census.Corpus do
  @max_rows 500
  @max_sentence_bytes 500
  @origins ~w(bradley_chat_sample public_social claude_authored)

  def load(path) do
    with {:ok, raw} <- File.read(path),
         [_, json] <- Regex.run(~r/```json\n(.*?)\n```/s, raw) || {:error, :no_json_block},
         {:ok, %{"version" => 1, "corpus_id" => id, "rows" => rows}}
         when is_binary(id) and is_list(rows) <- Jason.decode(json),
         :ok <- validate(rows) do
      {:ok, %{corpus_id: id, rows: rows, sha256: sha256(raw)}}
    else
      {:error, reason} -> {:error, reason}
      {:ok, _other} -> {:error, :invalid_corpus_shape}
      other -> {:error, {:invalid_corpus, other}}
    end
  end

  defp validate(rows) do
    ids = Enum.map(rows, &Map.get(&1, "case_id"))

    cond do
      length(rows) > @max_rows -> {:error, :too_many_rows}
      length(Enum.uniq(ids)) != length(ids) -> {:error, :duplicate_case_id}
      not Enum.all?(rows, &valid_row?/1) -> {:error, :invalid_row}
      true -> :ok
    end
  end

  defp valid_row?(%{
         "case_id" => id,
         "arm" => arm,
         "family" => family,
         "origin" => origin,
         "representative" => representative,
         "sentence" => sentence
       })
       when is_binary(id) and is_binary(family) and is_binary(sentence) and
              is_boolean(representative) do
    byte_size(id) in 1..64 and byte_size(family) in 1..64 and
      arm in ["chat", "colony"] and origin in @origins and
      byte_size(String.trim(sentence)) in 1..@max_sentence_bytes and
      not (origin == "claude_authored" and representative)
  end

  defp valid_row?(_), do: false

  defp sha256(raw), do: :crypto.hash(:sha256, raw) |> Base.encode16(case: :lower)
end

defmodule Census.Runner do
  @resolver_opts [
    mode: :prod,
    enrich_lexicon?: true,
    lexicon_stage?: true,
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
    self_model: :off
  ]

  def run(%{"arm" => "chat", "sentence" => sentence} = row) do
    sentence
    |> Core.resolve_input(@resolver_opts)
    |> Core.Comprehension.CoverageLadder.assess()
    |> with_meta(row, :chat)
  end

  def run(%{"arm" => "colony", "sentence" => sentence} = row) do
    gate = Core.Comprehension.CoverageLadder.colony_policy_gate(sentence)

    case Core.Social.SemanticProbe.run(sentence, emit_telemetry?: false) do
      {:ok, result} ->
        result |> Core.Comprehension.CoverageLadder.from_probe(gate) |> with_meta(row, :colony)

      {:error, reason} ->
        {:probe_error, %{case_id: row["case_id"], reason: if(is_atom(reason), do: reason, else: :other)}}
    end
  end

  defp with_meta(record, row, arm) do
    Map.merge(record, %{
      case_id: row["case_id"],
      arm: arm,
      family: row["family"],
      origin: row["origin"],
      representative?: row["representative"]
    })
  end
end

{opts, rest, errors} =
  OptionParser.parse(System.argv(), strict: [corpus: :string, arm: :string, out: :string, help: :boolean])

corpus_path = opts[:corpus] || "docs/collab/r001/census_corpus.md"
arm = opts[:arm] || "all"

cond do
  opts[:help] ->
    IO.puts("Usage: mix run scripts/comprehension_coverage_census.exs [--corpus FILE] [--arm chat|colony|all] [--out FILE]")

  errors != [] or rest != [] or arm not in ["chat", "colony", "all"] ->
    IO.puts(:stderr, "Invalid arguments. Use --help for usage.")
    System.halt(2)

  true ->
    corpus =
      case Census.Corpus.load(corpus_path) do
        {:ok, corpus} ->
          corpus

        {:error, reason} ->
          IO.puts(:stderr, "Rejected corpus: #{inspect(reason)}")
          System.halt(2)
      end

    revision =
      case System.cmd("git", ["rev-parse", "HEAD"], stderr_to_stdout: true) do
        {sha, 0} -> String.trim(sha)
        _ -> :unknown
      end

    rows = Enum.filter(corpus.rows, &(arm == "all" or &1["arm"] == arm))
    {records, probe_errors} = rows |> Enum.map(&Census.Runner.run/1) |> Enum.split_with(&is_map/1)
    probe_errors = Enum.map(probe_errors, fn {:probe_error, error} -> error end)

    report = %{
      census: :r001_stage0,
      revision: revision,
      corpus_id: corpus.corpus_id,
      corpus_sha256: corpus.sha256,
      arm: arm,
      rows_requested: length(rows),
      probe_errors: probe_errors,
      aggregate: CoverageLadder.aggregate(records),
      semantic_correctness_assessed?: false,
      factual_authority?: false
    }

    IO.inspect(report, label: "R-001 Stage 0 census (aggregate)", limit: :infinity)

    if is_binary(opts[:out]) do
      File.mkdir_p!(Path.dirname(opts[:out]))
      File.write!(opts[:out], Jason.encode_to_iodata!(Map.put(report, :records, records), pretty: true))
      IO.puts("Per-row records written to #{opts[:out]} (no sentences included).")
    end
end
