# Run from the umbrella root:
#   mix run scripts/colony_contrastive_probe.exs
#
# Read-only production lexicon / Core syntax / LIFG / ATL.
# No LLM request, Colony POST, working-memory admission or durable write.
cases = [
  %{
    sentence: "The checker produced 13 links.",
    status: :attributed_structural_interpretation,
    relation: :reported_action, polarity: :positive, count: 13
  },
  %{
    sentence: "Ten links contradicted the original claims.",
    status: :attributed_structural_interpretation,
    relation: :reported_refutation, polarity: :positive, count: 10
  },
  %{
    sentence: "Three links supported the claims.",
    status: :attributed_structural_interpretation,
    relation: :reported_support, polarity: :positive, count: 3
  },
  %{
    sentence: "The checker did not verify those claims.",
    status: :attributed_structural_interpretation,
    relation: :reported_action, polarity: :negative, count: nil
  },
  %{
    sentence: "The checker verified those claims.",
    status: :attributed_structural_interpretation,
    relation: :reported_action, polarity: :positive, count: nil
  },
  %{
    sentence: "Three links did not support the claims.",
    status: :attributed_structural_interpretation,
    relation: :reported_support, polarity: :negative, count: 3
  },
  %{
    sentence: "Three links might support the claims.",
    status: :not_interpreted, reason: :modal_uncertainty
  },
  %{
    sentence: "Three links supported and contradicted the claims.",
    status: :not_interpreted
  }
]

IO.puts("\n=== COLONY-07: contrastive read-only comprehension benchmark ===")

results =
  Enum.map(cases, fn expected ->
    sentence = expected.sentence

    observed =
      case Core.Social.SemanticProbe.run(sentence, emit_telemetry?: false) do
        {:ok, semantic} ->
          proposition = semantic.reported_proposition

          %{
            status: proposition.status,
            reason: Map.get(proposition, :reason),
            relation: Map.get(proposition, :relation),
            polarity: Map.get(proposition, :polarity),
            count: Map.get(proposition, :reported_count),
            disputed_roles:
              proposition
              |> Map.get(:role_audit, [])
              |> Enum.filter(&(Map.get(&1, :agreed?) == false)),
            grammar_audit: Map.get(proposition, :grammar_audit),
            dictionary_status: semantic.syntax.audit.ltm_status
          }

        {:error, reason} ->
          %{status: :probe_failed, reason: reason}
      end

    matches? =
      expected
      |> Map.drop([:sentence])
      |> Enum.all?(fn {field, value} -> Map.get(observed, field) == value end)

    verdict = if matches?, do: :pass, else: :needs_investigation

    IO.inspect(
      %{
        sentence: sentence,
        expected: Map.drop(expected, [:sentence]),
        observed: observed,
        verdict: verdict
      },
      label: "CLAUSE",
      pretty: true,
      limit: :infinity
    )

    verdict
  end)

counts = Enum.frequencies(results)
IO.inspect(
  %{total: length(cases), passed: Map.get(counts, :pass, 0),
    needs_investigation: Map.get(counts, :needs_investigation, 0)},
  label: "CONTRASTIVE SUMMARY"
)

# An abstention can be the *correct* outcome. A real model/LTM mismatch
# must be investigated; this script never overrides selected word senses.
