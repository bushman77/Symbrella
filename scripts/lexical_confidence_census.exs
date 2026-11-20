# P-030: offline lexical-confidence census (companion to the R-001 coverage census).
#
#   mix run scripts/lexical_confidence_census.exs --corpus FILE [--arm chat|colony|all] [--out FILE]
#
# Read-only diagnostic. For each corpus row it resolves the sentence with the
# same persistence-disabled resolver options as
# scripts/comprehension_coverage_census.exs and records the comprehension
# summary's lexical-confidence fields: weak/total decisions, degraded?,
# uncertain-term count, and a lexical-gap-fallback *proxy*.
#
# Both arms use Core.resolve_input/2 (the coverage census routes Colony rows
# through Core.Social.SemanticProbe instead), so Colony numbers here are
# comparable across revisions of this script, not to the coverage census.
#
# The proxy (`degraded? and uncertain != []`) approximates when
# Core.Response.AttentionFallback would emit the "I can identify ..." reply;
# the real reply also depends on risk bucket, action and topic routing.
#
# Output: aggregates and per-row enumerated values only, never sentences or
# terms. Do not commit --out files produced from private rows.

defmodule LexCensus.Corpus do
  def load(path) do
    with {:ok, raw} <- File.read(path),
         [_, json] <- Regex.run(~r/```json\n(.*?)\n```/s, raw) || {:error, :no_json_block},
         {:ok, %{"corpus_id" => id, "rows" => rows}} when is_binary(id) and is_list(rows) <-
           Jason.decode(json) do
      {:ok, %{corpus_id: id, rows: rows, sha256: :crypto.hash(:sha256, raw) |> Base.encode16(case: :lower)}}
    else
      {:error, reason} -> {:error, reason}
      other -> {:error, {:invalid_corpus, other}}
    end
  end
end

defmodule LexCensus.Runner do
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

  def run(%{"sentence" => sentence} = row) when is_binary(sentence) do
    si = Core.resolve_input(sentence, @resolver_opts)

    summary =
      case Map.get(si, :comprehension) do
        %{stats: %{}} = s -> s
        _ -> Core.Comprehension.Summary.build(si)
      end

    stats = Map.get(summary, :stats, %{})
    uncertain = List.wrap(Map.get(summary, :uncertain))
    degraded? = Map.get(summary, :degraded?, false) == true

    %{
      case_id: row["case_id"],
      arm: row["arm"],
      family: row["family"],
      total: Map.get(stats, :total, 0),
      weak: Map.get(stats, :weak, 0),
      fallback: Map.get(stats, :fallback, 0),
      uncertain_count: length(uncertain),
      understood_count: length(List.wrap(Map.get(summary, :understood))),
      degraded?: degraded?,
      reasons: List.wrap(Map.get(summary, :reasons)),
      lexical_gap_proxy?: degraded? and uncertain != []
    }
  rescue
    e -> {:error, %{case_id: row["case_id"], error: Exception.message(e) |> String.slice(0, 200)}}
  end

  def aggregate([]), do: %{rows: 0}

  def aggregate(records) do
    n = length(records)
    total = sum(records, :total)
    weak = sum(records, :weak)

    %{
      rows: n,
      lifg_decisions: total,
      weak_decisions: weak,
      weak_rate: rate(weak, total),
      mean_row_weak_rate:
        records |> Enum.map(&rate(&1.weak, &1.total)) |> Enum.sum() |> Kernel./(n) |> Float.round(4),
      degraded_rows: count(records, & &1.degraded?),
      degraded_rate: rate(count(records, & &1.degraded?), n),
      lexical_gap_proxy_rows: count(records, & &1.lexical_gap_proxy?),
      lexical_gap_proxy_rate: rate(count(records, & &1.lexical_gap_proxy?), n),
      uncertain_terms: sum(records, :uncertain_count),
      rows_with_uncertain: count(records, &(&1.uncertain_count > 0)),
      understood_terms: sum(records, :understood_count),
      reasons:
        records |> Enum.flat_map(& &1.reasons) |> Enum.frequencies()
    }
  end

  defp sum(rs, k), do: rs |> Enum.map(&(Map.get(&1, k) || 0)) |> Enum.sum()
  defp count(rs, f), do: Enum.count(rs, f)
  defp rate(_, 0), do: 0.0
  defp rate(a, b), do: Float.round(a / b, 4)
end

{opts, rest, errors} =
  OptionParser.parse(System.argv(), strict: [corpus: :string, arm: :string, out: :string, help: :boolean])

arm = opts[:arm] || "all"

cond do
  opts[:help] ->
    IO.puts("Usage: mix run scripts/lexical_confidence_census.exs --corpus FILE [--arm chat|colony|all] [--out FILE]")

  errors != [] or rest != [] or arm not in ["chat", "colony", "all"] or not is_binary(opts[:corpus]) ->
    IO.puts(:stderr, "Invalid arguments (--corpus is required). Use --help for usage.")
    System.halt(2)

  true ->
    corpus =
      case LexCensus.Corpus.load(opts[:corpus]) do
        {:ok, c} -> c
        {:error, reason} -> IO.puts(:stderr, "Rejected corpus: #{inspect(reason)}"); System.halt(2)
      end

    revision =
      case System.cmd("git", ["rev-parse", "--short", "HEAD"], stderr_to_stdout: true) do
        {sha, 0} -> String.trim(sha)
        _ -> :unknown
      end

    rows = Enum.filter(corpus.rows, &(arm == "all" or &1["arm"] == arm))
    {records, errs} = rows |> Enum.map(&LexCensus.Runner.run/1) |> Enum.split_with(&is_map/1)

    report = %{
      census: :p030_lexical_confidence,
      revision: revision,
      corpus_id: corpus.corpus_id,
      corpus_sha256: corpus.sha256,
      arm: arm,
      rows_requested: length(rows),
      errors: Enum.map(errs, fn {:error, e} -> e end),
      config: %{
        lifg_sense_rank_decay: Application.get_env(:brain, :lifg_sense_rank_decay, 0.0),
        lifg_softmax_temperature: Application.get_env(:brain, :lifg_softmax_temperature, 1.0)
      },
      aggregate: LexCensus.Runner.aggregate(records),
      lexical_gap_is_proxy?: true,
      semantic_correctness_assessed?: false
    }

    IO.inspect(report, label: "P-030 lexical-confidence census (aggregate)", limit: :infinity)

    if is_binary(opts[:out]) do
      File.mkdir_p!(Path.dirname(opts[:out]))
      File.write!(opts[:out], Jason.encode_to_iodata!(Map.put(report, :records, records), pretty: true))
      IO.puts("Per-row records written to #{opts[:out]} (no sentences or terms included).")
    end
end
