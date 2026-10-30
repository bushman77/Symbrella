# Run from umbrella root:
#   mix run priv/lambda_requirement_evidence_probe.exs
#
# Offline: reads saved context evidence, no calls to Core.resolve_input.
# Reports requirement *candidate witnesses*, not verified compatibility,
# LIFG fixes, semantic truth, or evidence of contradiction.
# Writes only priv/lambda_requirement_evidence_results.jsonl.

alias Core.Semantic.Composition.RequirementEvidence

source = "priv/lambda_context_evidence_results.jsonl"
unless File.regular?(source), do: raise("Missing #{source}")

records =
  source
  |> File.stream!()
  |> Stream.reject(&(String.trim(&1) == ""))
  |> Enum.map(&Jason.decode!/1)

if records == [], do: raise("No context evidence records found")

reports =
  Enum.map(records, fn context ->
    findings =
      context
      |> Map.get("preposition_denotations", [])
      |> Enum.flat_map(fn
        %{"id" => id, "requirements" => requirements} when is_list(requirements) ->
          Enum.map(requirements, fn hypothesis ->
            audit = RequirementEvidence.audit(context, hypothesis)

            %{
              sense_id: id,
              relation_family: hypothesis["relation_family"],
              subtype: hypothesis["subtype"],
              required_context: hypothesis["required_context"],
              source_span: hypothesis["source_span"],
              status: audit.status,
              checks: audit.requirement_checks,
              predicate_form_of: audit.predicate_form_of,
              predicate_selected_sense_id: audit.predicate_selected_sense_id,
              object_selected_sense_id: audit.object_selected_sense_id,
              semantic_compatibility_proven?: false,
              modifies_lifg?: false
            }
          end)

        _ -> []
      end)

    %{
      name: context["name"],
      sentence: context["sentence"],
      preposition_selected_sense_id: get_in(context, ["preposition", "chosen_id"]),
      predicate_selected_sense_id: get_in(context, ["predicate", "chosen_id"]),
      object_selected_sense_id: get_in(context, ["object", "chosen_id"]),
      findings: findings,
      requirements_tested: length(findings),
      all_witnessed:
        Enum.count(findings,
          &(&1.status == :all_requirements_have_candidate_witnesses)),
      some_witnessed:
        Enum.count(findings,
          &(&1.status == :some_requirements_have_candidate_witnesses)),
      unresolved: Enum.count(findings, &(&1.status == :unresolved))
    }
  end)

output = "priv/lambda_requirement_evidence_results.jsonl"
File.write!(output, Enum.map_join(reports, "", &(Jason.encode!(&1) <> "\n")))

interesting = MapSet.new([
  "on|preposition|1",
  "on|preposition|4",
  "on|preposition|7",
  "on|preposition|12",
  "on|preposition|19",
  "on|preposition|28",
  "on|preposition|38",
  "on|preposition|39"
])

IO.inspect(%{
  saved_to: output,
  samples:
    Enum.map(reports, fn report ->
      %{
        name: report.name,
        predicate_selected_sense_id: report.predicate_selected_sense_id,
        object_selected_sense_id: report.object_selected_sense_id,
        requirements_tested: report.requirements_tested,
        all_witnessed: report.all_witnessed,
        some_witnessed: report.some_witnessed,
        unresolved: report.unresolved,
        selected_preposition: report.preposition_selected_sense_id,
        examples:
          report.findings
          |> Enum.filter(&MapSet.member?(interesting, &1.sense_id))
          |> Enum.map(&Map.take(&1, [
            :sense_id, :subtype, :status, :checks, :predicate_form_of
          ]))
      }
    end)
}, label: "Observed context requirement witnesses", limit: :infinity)
