# COLONY-08: frozen, held-out lexical/grammatical generalization evaluation.
#
# From the EXISTING umbrella root:
#   mix run scripts/colony_generalization_probe.exs
#   mix run scripts/colony_generalization_probe.exs --details
#
# This intentionally does not modify the interpreter to fit new sentences.
# Uses the actual read-only LTM, Core Syntax, Stage1 and pure ATL pipeline.
# No LLM calls, external requests, posting, durable learning or memory writes.
defmodule Colony08Probe do
  @case_path Path.expand("priv/colony08_heldout_cases.json", File.cwd!())
  @historical MapSet.new([
    "The checker produced 13 links.",
    "Ten links contradicted the original claims.",
    "Three links supported the claims.",
    "The checker did not verify those claims.",
    "The checker verified those claims.",
    "Three links did not support the claims.",
    "Three links might support the claims.",
    "Three links supported and contradicted the claims."
  ])

  def run(args) do
    unknown = args -- ["--details", "--list"]

    if unknown != [] do
      raise ArgumentError, "Unknown options: #{inspect(unknown)}. Use --details or --list."
    end

    raw = File.read!(@case_path)
    payload = Jason.decode!(raw)
    cases = Map.fetch!(payload, "cases")
    validate!(cases)
    sha256 = raw |> then(&:crypto.hash(:sha256, &1)) |> Base.encode16(case: :lower)

    IO.puts("\n=== COLONY-08: frozen held-out generalization probe ===")
    IO.puts("Corpus: #{@case_path}")
    IO.puts("SHA-256: #{sha256}")
    IO.puts("Cases: #{length(cases)} (transfer, frontier, safety)")
    IO.puts("Scoring: exact subject/predicate/object/count/relation/polarity and attribution")
    IO.puts("Frontier abstentions are COVERAGE GAPS, not correct generalization.")
    IO.puts("Infrastructure failures cannot count as safe abstentions.\n")

    if "--list" in args do
      Enum.each(cases, fn item ->
        IO.puts("#{item["id"]} [#{item["group"]}] #{item["sentence"]}")
      end)
    else
      results =
        Enum.map(cases, fn item ->
          observed = observe(item["sentence"])
          verdict = assess(item, observed)

          IO.puts("#{item["id"]} [#{item["group"]}] #{verdict} :: #{item["sentence"]}")

          if "--details" in args or verdict not in [:exact, :safe_abstention] do
            IO.inspect(
              %{
                target: item["expected"],
                observed:
                  Map.take(observed,
                    ~w(status reason relation polarity count subject predicate object
                       dictionary_status lifg_status verification trust selected_predicate)
                  ),
                disputed_roles: Map.get(observed, "disputed_roles", []),
                grammar_audit: Map.get(observed, "grammar_audit")
              },
              label: "DIAGNOSTIC",
              pretty: true,
              limit: :infinity
            )
          end

          %{item: item, observed: observed, verdict: verdict}
        end)

      summary(results, sha256)
    end
  end

  defp observe(sentence) do
    case Core.Social.SemanticProbe.run(sentence, emit_telemetry?: false) do
      {:ok, semantic} ->
        p = Map.get(semantic, :reported_proposition, %{})
        role_indices = Map.get(p, :role_indices) || %{}
        predicate_index = Map.get(role_indices, :predicate)

        choice =
          Enum.find(get_in(semantic, [:lifg, :choices]) || [], fn c ->
            Map.get(c, :token_index) == predicate_index and is_integer(predicate_index)
          end)

        %{
          "status" => as_string(Map.get(p, :status)),
          "reason" => as_string(Map.get(p, :reason)),
          "relation" => as_string(Map.get(p, :relation)),
          "polarity" => as_string(Map.get(p, :polarity)),
          "subject" => Map.get(p, :subject),
          "predicate" => Map.get(p, :predicate),
          "object" => Map.get(p, :object),
          "count" => Map.get(p, :reported_count),
          "source" => as_string(Map.get(p, :source)),
          "trust" => as_string(Map.get(p, :trust)),
          "verification" => as_string(Map.get(p, :verification)),
          "kind" => as_string(Map.get(p, :kind)),
          "dictionary_status" => as_string(get_in(semantic, [:syntax, :audit, :ltm_status])),
          "lifg_status" => as_string(get_in(semantic, [:lifg, :status])),
          "selected_predicate" => choice && Map.get(choice, :chosen_id),
          "disputed_roles" =>
            (Map.get(p, :role_audit) || [])
            |> Enum.filter(&(is_map(&1) and Map.get(&1, :agreed?) == false)),
          "grammar_audit" => Map.get(p, :grammar_audit)
        }

      {:error, reason} ->
        %{"status" => "probe_failed", "reason" => as_string(reason)}

      other ->
        %{"status" => "probe_failed", "reason" => inspect(other, limit: 3)}
    end
  end

  defp assess(item, observed) do
    expected = item["expected"]
    group = item["group"]
    status = observed["status"]

    cond do
      status == "probe_failed" or observed["dictionary_status"] != "ready" or
          observed["lifg_status"] != "scored" ->
        :infrastructure_failure

      status == "attributed_structural_interpretation" and
          not correctly_attributed?(observed) ->
        :provenance_violation

      group == "safety" ->
        if status == "not_interpreted" and correctly_abstained?(observed) and
             (is_nil(expected["reason"]) or expected["reason"] == observed["reason"]) do
          :safe_abstention
        else
          :unsafe_acceptance
        end

      status == "attributed_structural_interpretation" ->
        if Enum.all?(~w(relation polarity count subject predicate object), fn key ->
             Map.fetch!(observed, key) == Map.fetch!(expected, key)
           end) do
          :exact
        else
          :semantic_mismatch
        end

      status == "not_interpreted" and correctly_abstained?(observed) ->
        :coverage_gap

      true ->
        :unexpected_result
    end
  end

  defp correctly_attributed?(observed) do
    observed["source"] == "colony_external" and
      observed["trust"] == "untrusted_external" and
      observed["verification"] == "author_claim_not_independently_verified" and
      observed["kind"] == "reported_declarative_proposition"
  end

  defp correctly_abstained?(observed) do
    observed["verification"] == "external_unverified" and
      is_nil(observed["relation"]) and is_nil(observed["polarity"])
  end

  defp summary(results, sha256) do
    groups =
      Enum.map(["transfer", "frontier", "safety"], fn group ->
        items = Enum.filter(results, &(&1.item["group"] == group))
        scores = Enum.frequencies(Enum.map(items, & &1.verdict))

        successes =
          Enum.count(items, fn row ->
            row.verdict ==
              if(group == "safety", do: :safe_abstention, else: :exact)
          end)

        {group, %{total: length(items), success: successes, verdicts: scores}}
      end)
      |> Map.new()

    pairs =
      results
      |> Enum.filter(&Map.has_key?(&1.item, "pair"))
      |> Enum.group_by(& &1.item["pair"])
      |> Enum.map(fn {pair, members} ->
        observed_polarities = Enum.map(members, & &1.observed["polarity"])
        verdict =
          cond do
            length(members) != 2 -> :invalid_pair
            Enum.all?(members, &(&1.verdict == :exact)) and
                Enum.sort(observed_polarities) == ["negative", "positive"] -> :preserved
            observed_polarities == ["positive", "positive"] or
                observed_polarities == ["negative", "negative"] -> :polarity_conflation
            true -> :unresolved
          end

        {pair, verdict}
      end)
      |> Enum.sort_by(&elem(&1, 0))

    counts = Enum.frequencies(Enum.map(results, & &1.verdict))
    bad = [:unsafe_acceptance, :semantic_mismatch, :provenance_violation,
           :infrastructure_failure, :unexpected_result, :polarity_conflation]
    critical = Enum.sum(Enum.map(bad, &Map.get(counts, &1, 0)))

    IO.puts("\n=== COLONY-08 BASELINE SUMMARY ===")
    IO.inspect(%{
      frozen_sha256: sha256,
      total: length(results),
      transfer_exact: {groups["transfer"].success, groups["transfer"].total},
      frontier_exact: {groups["frontier"].success, groups["frontier"].total},
      safety_abstentions: {groups["safety"].success, groups["safety"].total},
      critical_failures: critical,
      by_verdict: counts,
      paired_polarity: pairs
    }, label: "GENERALIZATION", pretty: true, limit: :infinity)

    IO.puts("\nInterpretation: transfer misses and frontier coverage gaps reveal")
    IO.puts("current grammar/lexical limitations; they are not safety passes.")
    IO.puts("No changes were made to the corpus, dictionary, memory or Colony.")
  end

  defp validate!(cases) when is_list(cases) do
    ids = Enum.map(cases, &Map.fetch!(&1, "id"))
    sentences = Enum.map(cases, &Map.fetch!(&1, "sentence"))

    groups = Enum.frequencies(Enum.map(cases, &Map.fetch!(&1, "group")))

    valid? =
      length(ids) == 37 and
        length(Enum.uniq(ids)) == length(ids) and
        length(Enum.uniq(sentences)) == length(sentences) and
        groups == %{"transfer" => 18, "frontier" => 7, "safety" => 12} and
        Enum.all?(cases, fn item ->
          sentence = item["sentence"]
          expected = item["expected"]
          is_binary(sentence) and byte_size(sentence) <= 420 and
            not MapSet.member?(@historical, sentence) and
            is_map(expected) and
            case item["group"] do
              "safety" -> expected["outcome"] == "abstain"
              _ ->
                expected["outcome"] == "interpret" and
                  Enum.all?(~w(relation polarity subject predicate object count),
                    &Map.has_key?(expected, &1))
            end
        end)

    unless valid?, do: raise("Invalid or changed COLONY-08 held-out suite")
    :ok
  end

  defp validate!(_), do: raise("COLONY-08 cases must be a list")
  defp as_string(nil), do: nil
  defp as_string(value) when is_atom(value), do: Atom.to_string(value)
  defp as_string(value) when is_binary(value), do: value
  defp as_string(value), do: inspect(value, limit: 3)
end

Colony08Probe.run(System.argv())
