# Run from the Symbrella umbrella root:
#   mix run priv/lemma_grounded_predicate_pilot.exs
#
# V1 live observational experiment. Resolve two sentences with full LIFG
# scores, discover independently attested form-of verb senses, then fetch
# canonical lemma rows using Db.brain_cells_for_norm/1 (READ ONLY).
# No LIFG scores, dictionary rows, saved memory, or lambda proof authorizations
# are deliberately modified by this script. The normal resolve_input pipeline
# can have its usual side effects. Output only under priv/.

alias Core.Semantic.Composition.LemmaGroundedPredicate, as: Predicate
alias Core.Semantic.Composition

samples = [
  %{id: "known_progressive", sentence: "The bird is sitting on the fence."},
  %{id: "novel_past", sentence: "The fisherman sat on the bank of the river."}
]

results =
  Enum.map(samples, fn sample ->
    si =
      Core.resolve_input(sample.sentence,
        fuzzy: false,
        neuron_activation?: false,
        notify_brain?: false,
        lifg_opts: [
          scores: :all,
          synapse_eval_full_scores?: true,
          spatial_pp_senses?: false
        ]
      )

    tokens = List.wrap(Map.get(si, :tokens))
    choices = List.wrap(Map.get(si, :lifg_choices))

    on_token =
      Enum.find(tokens, fn
        %{phrase: phrase, n: 1} when is_binary(phrase) ->
          String.downcase(phrase) == "on"
        _ -> false
      end)

    unless is_map(on_token) do
      raise "Expected a unigram 'on' in #{sample.id}"
    end

    # V1's construction hypothesis is strictly the preceding token.
    # It does NOT assert that the preceding token is the governing predicate
    # in arbitrary sentences. Both chosen examples have this shape.
    predicate_index = Map.get(on_token, :index) - 1

    word =
      Enum.find_value(tokens, fn token ->
        if is_map(token) and Map.get(token, :index) == predicate_index and
             Map.get(token, :n, 1) == 1 do
          Map.get(token, :phrase)
        end
      end)

    discovery = Predicate.discover(si, predicate_index)

    # Query only lemmas explicitly requested by a verified morphological
    # witness; never synthesize a lemma by stemming the surface.
    lemmas = Map.get(discovery, :requested_lemmas, [])
    lemma_rows =
      lemmas
      |> Enum.flat_map(&Db.brain_cells_for_norm/1)

    grounded = Predicate.ground(discovery, lemma_rows)

    choice =
      Enum.find(choices, fn c ->
        is_map(c) and Map.get(c, :token_index) == predicate_index
      end)

    formally_verified? = match?({:ok, _proof}, Composition.verified(si))

    %{
      sample_id: sample.id,
      sentence: sample.sentence,
      predicate_index: predicate_index,
      predicate_surface: word,
      original_lifg_choice: if(choice, do: Map.get(choice, :id), else: nil),
      canonical_lemma_queries: lemmas,
      lemma_db_rows_returned: length(lemma_rows),
      lemma_verb_senses_retained: length(Map.get(grounded, :lemma_senses, [])),
      discovered_form_senses: Map.get(grounded, :forms, []),
      status: Map.get(grounded, :status),
      lemma_senses: Map.get(grounded, :lemma_senses, []),
      predicate_feature_candidates: Map.get(grounded, :feature_hypotheses, []),
      compositional_proof_verified?: formally_verified?,
      semantic_compatibility_proven?: false,
      selected_lifg_unchanged?: true
    }
  end)

output = "priv/lemma_grounded_predicate_pilot_results.jsonl"
File.write!(output, Enum.map_join(results, "", &(Jason.encode!(&1) <> "\n")))

IO.inspect(
  %{
    saved_to: output,
    samples: Enum.map(results, fn r ->
      %{
        sample_id: r.sample_id,
        predicate_surface: r.predicate_surface,
        original_lifg_choice: r.original_lifg_choice,
        status: r.status,
        canonical_lemma_queries: r.canonical_lemma_queries,
        form_witness_count: length(r.discovered_form_senses),
        lemma_db_rows_returned: r.lemma_db_rows_returned,
        lemma_verb_senses_retained: r.lemma_verb_senses_retained,
        classified_lemma_sense_count:
          Enum.count(r.lemma_senses, & &1.classified?),
        features: Enum.map(r.predicate_feature_candidates, fn h ->
          Map.take(h, [:feature, :source_lemma_sense_id, :supporting_text])
        end),
        proof_verified_in_original_pipeline?: r.compositional_proof_verified?
      }
    end)
  },
  label: "Lemma-grounded predicate semantics V1",
  limit: :infinity
)
