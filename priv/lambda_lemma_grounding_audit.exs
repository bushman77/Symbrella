# Run from the Symbrella umbrella root:
#   mix run priv/lambda_lemma_grounding_audit.exs
#
# Observational pilot: follow independently attested dictionary "form-of"
# pointers to BASE VERB definitions in the existing brain_cells corpus.
# Reads saved context evidence and queries Db.brain_cells_for_norm/1 only.
# It does NOT rerun Core, select a verb sense, modify weights, save memory,
# assert an event, or authorize a lambda proof.
# Writes only priv/lambda_lemma_grounding_results.jsonl.

alias Core.Semantic.Composition.RequirementEvidence

source = "priv/lambda_context_evidence_results.jsonl"

unless File.regular?(source) do
  raise "Expected saved context evidence at #{source}"
end

contexts =
  source
  |> File.stream!()
  |> Stream.reject(&(String.trim(&1) == ""))
  |> Enum.map(&Jason.decode!/1)

if contexts == [], do: raise("No context evidence rows available")

requests =
  Enum.map(contexts, fn context ->
    witness = RequirementEvidence.audit(context, %{required_context: []})
    provenance = witness.predicate_form_of

    lemma =
      if is_map(provenance),
        do: provenance.attested_form_of_lemma,
        else: nil

    %{
      name: context["name"],
      sentence: context["sentence"],
      selected_predicate_id: get_in(context, ["predicate", "chosen_id"]),
      attested_morphology: provenance,
      requested_lemma: lemma
    }
  end)

lemmas =
  requests
  |> Enum.map(& &1.requested_lemma)
  |> Enum.filter(&(is_binary(&1) and Regex.match?(~r/\A\p{L}+(?:-\p{L}+)*\z/u, &1)))
  |> Enum.uniq()
  |> Enum.sort()

# One bounded LTM lookup per unique, independently witnessed lemma. We retain
# all returned verb senses; a form-of pointer alone identifies a LEMMA, not
# its dictionary sense or exact lexical semantic role in the sentence.
base_senses =
  Map.new(lemmas, fn lemma ->
    rows =
      lemma
      |> Db.brain_cells_for_norm()
      |> Enum.filter(fn row ->
        Map.get(row, :pos) == "verb" and Map.get(row, :norm) == lemma and
          is_binary(Map.get(row, :id)) and
          is_binary(Map.get(row, :definition))
      end)
      |> Enum.sort_by(&Map.get(&1, :id))
      |> Enum.map(fn row ->
        %{
          id: row.id,
          norm: row.norm,
          pos: row.pos,
          definition: row.definition,
          example: Map.get(row, :example),
          semantic_atoms: Map.get(row, :semantic_atoms),
          gram_function: Map.get(row, :gram_function),
          status: Map.get(row, :status)
        }
      end)

    {lemma, rows}
  end)

report =
  Enum.map(requests, fn request ->
    senses = Map.get(base_senses, request.requested_lemma, [])

    %{
      sample: request.name,
      sentence: request.sentence,
      original_selected_predicate: request.selected_predicate_id,
      morphology_witness: request.attested_morphology,
      lemma: request.requested_lemma,
      lemma_verb_sense_count: length(senses),
      all_lemma_verb_senses: senses,
      lemma_found?: senses != [],
      exact_lemma_sense_selected?: false,
      event_semantics_verified?: false,
      modifies_lifg?: false
    }
  end)

output = "priv/lambda_lemma_grounding_results.jsonl"

File.write!(output, Enum.map_join(report, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(%{
  samples:
    Enum.map(report, fn r ->
      %{
        name: r.sample,
        selected_predicate: r.original_selected_predicate,
        morphology_witness: r.morphology_witness,
        lemma: r.lemma,
        lemma_verb_sense_count: r.lemma_verb_sense_count,
        lemma_senses_preview:
          r.all_lemma_verb_senses
          |> Enum.take(16)
          |> Enum.map(&Map.take(&1, [:id, :definition, :gram_function])),
        exact_lemma_sense_selected?: false
      }
    end),
  saved_to: output
}, label: "Dictionary lemma grounding evidence",
  limit: :infinity, printable_limit: 1300)
