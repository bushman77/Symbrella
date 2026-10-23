defmodule Core.Social.SemanticProbe do
  @moduledoc """
  Bounded, observation-only lexical and grammatical analysis of external text.

  Does not call Core.resolve_input(mode: :prod), because that path can write
  Hippocampus, focus WM, notify neurons, and record synaptic experiences.
  Instead we reuse Core's tokenizer, read-only LTM lookup, syntax evidence and
  LIFG Stage1 scoring with learning and normal Stage1 telemetry disabled.
  A selected lexical candidate is NOT a verified meaning or a factual claim.
  """

  @max_input_chars 600
  @max_tokens 64
  # Keep the complete bounded 64-token slate for ATL. Truncating to twelve
  # choices discarded the predicate of longer Colony headlines.
  @max_choices 64
  @low_margin 0.15

  @doc """
  Analyze a short excerpt. Returns only a bounded diagnostic, never the
  underlying SemanticInput, candidate slates, or external instructions.
  """
  def run(text, opts \\ [])

  def run(text, opts) when is_binary(text) and is_list(opts) do
    excerpt = text |> String.trim() |> String.slice(0, @max_input_chars)

    if excerpt == "" do
      {:error, :empty_social_text}
    else
      analyze(excerpt, opts)
    end
  end

  def run(_, _), do: {:error, :invalid_social_text}

  defp analyze(excerpt, opts) do
    all_tokens =
      excerpt
      |> Core.LIFG.Input.tokenize(fuzzy: false, max_wordgram_n: 1)
      |> Enum.filter(&is_map/1)

    tokens = Enum.take(all_tokens, @max_tokens)

    si = %{
      sentence: excerpt,
      tokens: tokens,
      source: :colony_external,
      experience_origin: :colony_external,
      active_cells: [],
      sense_candidates: %{},
      trace: []
    }

    # Checked, bounded *read*: a failed DB query must not look like a
    # legitimate empty lexicon. Never write or activate retrieved BrainCells.
    {si, ltm_lookup} =
      if Keyword.get(opts, :ltm?, true) do
        reader = Keyword.get(opts, :ltm_reader, &Core.Pipeline.LTM.Source.checked_ltm/2)

        # Retain a bounded spread of attested dictionary POS candidates.
        # The default LTM ordering is unchanged outside this read-only path.
        case reader.(si, limit_per_norm: 8, pos_diverse?: true) do
          {:ok, %{rows: rows, db_hits: %MapSet{} = db_hits} = data} when is_list(rows) ->
            enriched = Core.Pipeline.LTM.enrich(si, rows, db_hits)
            {enriched,
             %{
               status: if(rows == [], do: :no_matches, else: :ready),
               source_rows: length(rows),
               missing_norms: data |> Map.get(:missing_norms, []) |> List.wrap() |> length()
             }}

          {:error, reason} when is_atom(reason) ->
            {si, %{status: :lookup_failed, reason: reason, source_rows: 0}}

          _ ->
            {si, %{status: :lookup_failed, reason: :invalid_ltm_result, source_rows: 0}}
        end
      else
        {si, %{status: :disabled, source_rows: 0}}
      end

    si =
      si
      |> Core.Syntax.Signatures.attach()
      |> Core.Syntax.RoleCompetition.attach()
      |> Core.Syntax.SentenceCompetition.attach()
      # Relation-edge lookup reads existing lexical DB rows. No related
      # BrainCells are activated and no external text is persisted.
      |> Core.Relations.attach_edges(attach_related_cells?: false)

    signatures =
      si
      |> Map.get(:sentence_signatures, [])
      |> Enum.take(12)
      |> Enum.map(fn sig ->
        Map.take(sig, [:id, :span, :roles])
      end)

    grammar_audit =
      si
      |> grammar_audit(all_tokens)
      |> Map.put(:ltm_status, ltm_lookup.status)
      |> Map.put(:ltm_source_rows, ltm_lookup.source_rows)
      |> Map.put(:ltm_reason, Map.get(ltm_lookup, :reason))
      |> Map.put(:missing_norms_count, Map.get(ltm_lookup, :missing_norms, 0))

    {lifg, choices} = score_stage1(si, opts)

    # Pure ATL reducer: never call Brain.ATL.ingest/3 here. That operation
    # updates its GenServer and is inappropriate for untrusted social text.
    projection = project_choices(choices)
    slate = Brain.ATL.reduce(projection, tokens, Map.get(si, :sentence_signatures, []))

    semantic_si =
      si
      |> Map.put(:lifg_choices, projection)
      |> Map.put(:atl_slate, slate)

    # SentenceMeaning's general participant gate intentionally refuses
    # untrusted external events. The narrower external proposition interpreter
    # can still report a structurally supported, *unverified* assertion.
    meaning = Core.Semantic.SentenceMeaning.build(semantic_si)
    reported_proposition = Core.Social.ReportedProposition.interpret(semantic_si)
    summary = Core.Comprehension.Summary.build(semantic_si)

    result = %{
       stage: :social_semantic_shadow,
       input_chars: String.length(excerpt),
       token_count: length(tokens),
       syntax: %{status: :observed, signatures: signatures, audit: grammar_audit},
       lifg: lifg,
       atl: %{
         status: :read_only_reduced,
         winner_count: Map.get(slate, :winner_count, 0),
         resolved_constituents: slate |> Map.get(:constituents, []) |> List.wrap() |>
           Enum.count(&(Map.get(&1, :status) == :resolved)),
         semantic_relations: si |> get_in([:evidence, :relations]) |>
           List.wrap() |> Enum.take(8) |> length()
       },
       question_composition: Core.Semantic.SentenceMeaning.question_diagnostic(semantic_si),
       question_evidence: %{
         signature_count: Enum.count(signatures, &(Map.get(&1, :id) in [:wh_aux_topic_question, :wh_aux_object_question])),
         resolved_constituents: slate
           |> Map.get(:constituents, [])
           |> List.wrap()
           |> Enum.count(fn constituent ->
             Map.get(constituent, :id) in [:wh_aux_topic_question, :wh_aux_object_question] and
               Map.get(constituent, :status) == :resolved
           end)
       },
       sentence_meaning: %{
         status: Map.get(meaning, :status, :not_yet_interpreted),
         kind: Map.get(meaning, :kind, :unknown),
         speech_act: Map.get(meaning, :speech_act),
         definition: Map.get(meaning, :definition),
         source_frame: Map.get(meaning, :source_frame),
         role_surfaces: bounded_role_surfaces(meaning),
         topic_structure: bounded_topic_structure(meaning),
         embedded_candidates: Core.Semantic.EmbeddedClauseEvidence.candidates(semantic_si, meaning),
         unresolved_roles: meaning |> Map.get(:unresolved, []) |> List.wrap() |> Enum.take(8),
         provenance: :external_unverified
       },
       reported_proposition: reported_proposition,
       declarative_evidence: Core.Social.DeclarativeEvidence.analyze(semantic_si),
       comprehension: %{
         recognized_topics: Enum.take(summary.recognized_topics, 6),
         selected_lexical_candidates: Enum.take(summary.selected_lexical_candidates, 6),
         understood: Enum.take(summary.understood, 6),
         uncertain: Enum.take(summary.uncertain, 6),
         degraded?: summary.degraded?,
         reasons: Enum.take(summary.reasons, 6)
       },
       acc_shadow: conflict_shadow(lifg)
     }

    # The standalone read-only probe also deserves visibility in Brain.
    # Pure lexical analysis is NOT neuron activation, working-memory use,
    # memory writing, or proof of the external claim.
    if Keyword.get(opts, :emit_telemetry?, true) do
      emit_shadow_telemetry(result)
    end

    {:ok, result}
  end

  defp emit_shadow_telemetry(result) do
    audit = result.syntax.audit
    proposition = result.reported_proposition

    :telemetry.execute(
      [:core, :social, :cognitive_shadow],
      %{
        lifg_choices: result.lifg.chosen_count,
        atl_winners: result.atl.winner_count,
        syntax_frames: audit.signature_count,
        dictionary_rows: audit.dictionary_rows,
        pos_covered_tokens: audit.pos_covered_tokens,
        processed_tokens: audit.processed_tokens,
        disputed_roles:
          proposition
          |> Map.get(:role_audit, [])
          |> Enum.count(&(Map.get(&1, :agreed?) == false))
      },
      %{
        source: :colony_external,
        probe_mode: :standalone_read_only,
        post_id: nil,
        reported_relation: Map.get(proposition, :embedded_relation, :not_interpreted),
        sentence_status: result.sentence_meaning.status,
        provenance: :external_unverified,
        grammar_outcome: audit.outcome,
        ltm_status: audit.ltm_status,
        ltm_reason: audit.ltm_reason,
        proposition_status: proposition.status,
        proposition_reason: Map.get(proposition, :reason)
      }
    )
  end

  # This is a loss-of-evidence audit, not a semantic verdict. It helps
  # distinguish missing dictionary POS from unsupported grammatical patterns
  # without printing untrusted text or private BrainCell definitions.
  defp grammar_audit(si, all_tokens) do
    tokens = Map.get(si, :tokens, [])
    rows = Map.get(si, :active_cells, [])
    signatures = Map.get(si, :sentence_signatures, [])

    tagged =
      Enum.filter(rows, fn row ->
        is_map(row) and is_binary(Map.get(row, :pos)) and
          String.trim(Map.get(row, :pos)) != ""
      end)

    indices =
      tagged
      |> Enum.map(&Map.get(&1, :token_index))
      |> Enum.filter(&is_integer/1)
      |> MapSet.new()

    categories =
      tagged
      |> Enum.map(&Map.get(&1, :pos))
      |> Enum.frequencies()

    pos_covered =
      tokens
      |> Enum.filter(&MapSet.member?(indices, Map.get(&1, :index)))
      |> length()

    %{
      source_tokens: length(all_tokens),
      processed_tokens: length(tokens),
      token_limit_reached?: length(all_tokens) > length(tokens),
      dictionary_rows: length(rows),
      pos_tagged_rows: length(tagged),
      pos_covered_tokens: pos_covered,
      pos_counts: Map.take(categories, ~w(noun verb pronoun determiner det adjective adj preposition prep auxiliary aux proper_noun)),
      signature_count: length(signatures),
      outcome:
        cond do
          tokens == [] -> :no_tokens
          tagged == [] -> :missing_dictionary_pos
          pos_covered == 0 -> :token_index_alignment_missing
          signatures == [] -> :no_supported_grammar_pattern
          true -> :grammar_candidates_detected
        end
    }
  end

  defp score_stage1(si, opts) do
    # A caller-injected evaluator is useful in tests; the normal implementation
    # always uses the real Brain.LIFG.Stage1 engine with isolated telemetry.
    scorer = Keyword.get(opts, :scorer, &Brain.LIFG.Stage1.run/2)

    shadow_opts = [
      cerebellum_learn?: false,
      reanalysis: false,
      reanalysis?: false,
      mwe_fallback: false,
      stage1_local_mwe_fallback?: false,
      sense_focus?: false,
      semantic_control?: false,
      stage1_stop_event: [:core, :social, :shadow_stage1_stop],
      chargram_event: [:core, :social, :shadow_chargram],
      boundary_event: [:core, :social, :shadow_boundary],
      mwe_event: [:core, :social, :shadow_mwe]
    ]

    case scorer.(si, shadow_opts) do
      {:ok, %{choices: choices, audit: audit}} when is_list(choices) and is_map(audit) ->
        selected =
          choices
          |> Enum.filter(&is_map/1)
          |> Enum.take(@max_choices)

        {%{
           status: :scored,
           chosen_count: length(choices),
           missing_candidates: Map.get(audit, :missing_candidates, 0),
           choices: Enum.map(selected, &compact_choice/1)
         }, selected}

      {:error, _} ->
        {%{status: :unavailable, chosen_count: 0, missing_candidates: nil, choices: []}, []}

      _ ->
        {%{status: :unavailable, chosen_count: 0, missing_candidates: nil, choices: []}, []}
    end
  end

  defp compact_choice(choice) do
    %{
      token_index: Map.get(choice, :token_index),
      chosen_id: limited_id(Map.get(choice, :chosen_id)),
      score: probability(Map.get(choice, :score) || Map.get(choice, :prob)),
      margin: bounded_number(Map.get(choice, :margin)),
      probability_margin: bounded_number(Map.get(choice, :prob_margin)),
      weak?: Map.get(choice, :weak?) == true,
      reliability: Map.get(choice, :reliability)
    }
  end

  defp project_choices(choices) do
    Enum.map(choices, fn choice ->
      id = Map.get(choice, :chosen_id)

      %{
        id: limited_id(id),
        lemma: lemma(id),
        token_index: Map.get(choice, :token_index),
        # LIFG's selected posterior (p1) is a choice score. The
        # difference between p1 and the runner-up is a *margin*, not a
        # substitute for p1. A weak margin can coexist with a large p1.
        # This does not convert the selected sense into verified meaning.
        score: probability(Map.get(choice, :score) || Map.get(choice, :prob)),
        weak?: Map.get(choice, :weak?) == true,
        reliability: Map.get(choice, :reliability)
      }
    end)
  end

  defp lemma(id) when is_binary(id) do
    id
    |> String.split("|", parts: 2)
    |> List.first()
    |> String.slice(0, 80)
  end

  defp lemma(_), do: nil

  defp limited_id(id) when is_binary(id), do: String.slice(id, 0, 128)
  defp limited_id(_), do: nil
  defp probability(n) when is_number(n), do: n |> max(0.0) |> min(1.0)
  defp probability(_), do: 0.0

  defp bounded_number(n) when is_number(n), do: n * 1.0
  defp bounded_number(_), do: 0.0

  defp conflict_shadow(%{status: :scored, choices: choices, missing_candidates: missing}) do
    low_margin =
      Enum.count(choices, fn %{margin: margin} -> margin < @low_margin end)

    %{
      status: :shadow_only,
      low_margin_choices: low_margin,
      missing_candidates: missing,
      uncertain?: low_margin > 0 or (is_integer(missing) and missing > 0),
      note: :lexical_ambiguity_not_factual_contradiction
    }
  end

  defp conflict_shadow(_) do
    %{status: :unavailable, uncertain?: true, note: :lexical_evaluation_missing}
  end
  # A semantic role is shown only if SentenceMeaning explicitly produced it.
  # These are attributed grammatical surfaces, not verified entity identities.
  @bounded_role_keys [:question_word, :subject, :actor, :predicate, :object,
    :patient, :topic, :preposition, :position, :location]

  defp bounded_role_surfaces(%{status: status, roles: roles})
       when status in [:structurally_interpreted, :interpreted, :understood, :resolved]
       and is_map(roles) do
    Enum.reduce(@bounded_role_keys, %{}, fn key, acc ->
      case Map.get(roles, key) do
        %{surface: surface} when is_binary(surface) ->
          Map.put(acc, key, String.slice(surface, 0, 160))
        _ ->
          acc
      end
    end)
  end

  defp bounded_role_surfaces(_), do: %{}

  # The complement is not an assertion. A surface-only topic has no verified
  # nominal or embedded-clause reading even when the matrix WH frame resolves.
  defp bounded_topic_structure(%{
         proposition: %{
           type: :information_request,
           epistemic_status: :question_not_assertion,
           content: %{complement: %{
             kind: :prepositional_topic,
             argument: %{type: type, source: source, grounding_status: grounding}
           }}
         }
       }) do
    %{
      type: type,
      source: source,
      grounding_status: grounding,
      assertion_status: :not_asserted,
      embedded_clause_status: :not_interpreted
    }
  end

  defp bounded_topic_structure(_), do: nil


end
