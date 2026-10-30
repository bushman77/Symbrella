# Run from the Symbrella umbrella root, using the existing full slate audit:
#   mix run priv/lambda_denotation_inventory.exs
#
# Reads priv/lambda_full_slate_audit_results.jsonl without re-running Core.
# Writes priv/lambda_denotation_inventory_results.jsonl only.
# All annotations are tentative dictionary-gloss hypotheses, never gold
# sense labels, verified semantic parses, or modified LIFG decisions.

alias Core.Semantic.Composition.SenseDenotations

source = "priv/lambda_full_slate_audit_results.jsonl"
unless File.regular?(source) do
  raise "Missing #{source}; run priv/lambda_full_slate_audit.exs first"
end

rows =
  source
  |> File.stream!()
  |> Stream.reject(&(String.trim(&1) == ""))
  |> Enum.map(&Jason.decode!/1)

if rows == [] do
  raise "The input inventory contains no scored senses"
end

reports =
  Enum.map(rows, fn row ->
    finding = SenseDenotations.propose(%{
      id: row["id"],
      norm: "on",
      pos: "preposition",
      definition: row["definition"]
    })

    %{
      sense_id: row["id"],
      definition: row["definition"],
      selected?: row["selected?"] == true,
      inventory: finding
    }
  end)

recognized = Enum.filter(reports, &(not &1.inventory.unresolved?))
unresolved = Enum.filter(reports, & &1.inventory.unresolved?)
multiple = Enum.filter(reports, & &1.inventory.multiple_readings?)

family_occurrences =
  reports
  |> Enum.flat_map(fn r -> Enum.map(r.inventory.candidates, & &1.relation_family) end)
  |> Enum.frequencies()
  |> Enum.sort_by(fn {family, count} -> {-count, Atom.to_string(family)} end)

preview_ids = MapSet.new([
  "on|preposition|1", "on|preposition|7", "on|preposition|12",
  "on|preposition|19", "on|preposition|38", "on|preposition|39"
])

previews =
  reports
  |> Enum.filter(&MapSet.member?(preview_ids, &1.sense_id))
  |> Enum.map(fn r ->
    %{
      id: r.sense_id,
      hypotheses: Enum.map(r.inventory.candidates, fn c ->
        Map.take(c, [:relation_family, :subtype, :required_context,
          :matched_text, :verification])
      end)
    }
  end)

output = "priv/lambda_denotation_inventory_results.jsonl"

File.write!(output, Enum.map_join(reports, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(%{
  input_rows: length(reports),
  candidate_denotation_rows: length(recognized),
  unresolved_rows: length(unresolved),
  multiple_candidate_rows: length(multiple),
  family_occurrences_not_unique_sense_counts: family_occurrences,
  unresolved_ids: Enum.map(unresolved, & &1.sense_id),
  selected_sense: reports
    |> Enum.find(& &1.selected?)
    |> case do
      nil -> nil
      r -> %{sense_id: r.sense_id,
             denotations: Enum.map(r.inventory.candidates, & &1.subtype)}
    end,
  previews: previews,
  saved_to: output,
  modifies_lifg?: false
}, label: "Provisional denotation inventory", limit: :infinity, printable_limit: :infinity)
