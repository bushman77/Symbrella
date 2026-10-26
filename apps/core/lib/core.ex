defmodule Core do
  @moduledoc """
  ...
  """

  alias Core.Intent.Selection
  alias Core.LIFG.Attach, as: LifgAttach
  alias Core.Response.Attach, as: ResponseAttach
  alias Core.SemanticInput
  alias Core.StateDump
  alias Core.TokenFilters
  alias Core.Brain.Introspection, as: BrainIntrospection
  @type opts :: keyword()

  @si_template %SemanticInput{}
  @si_fields @si_template |> Map.from_struct() |> Map.keys() |> MapSet.new()

  defp coerce_si(%SemanticInput{} = si), do: si

  defp coerce_si(%{} = m) do
    filtered =
      m
      |> Enum.filter(fn {k, _} -> is_atom(k) and MapSet.member?(@si_fields, k) end)
      |> Map.new()

    struct(SemanticInput, filtered)
  end

  defp coerce_si(other) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp coerce_si(_), do: %SemanticInput{}

  @spec resolve_input(String.t(), opts()) :: SemanticInput.t()
  def resolve_input(phrase, opts \\ []) when is_binary(phrase) do
    mode = Keyword.get(opts, :mode, :prod)
    max_n = Keyword.get(opts, :max_wordgram_n, 3)
    lifg_opts = build_lifg_opts(opts)
    fuzzy? = Keyword.get(opts, :fuzzy, true)

    fuzzy = if fuzzy?, do: Core.Text.Fuzzy.interpret(phrase, fuzzy_opts(opts)), else: nil

    si0 =
      phrase
      |> Core.LIFG.Input.tokenize(max_wordgram_n: max_n, fuzzy: fuzzy?)
      |> wrap_si(phrase)
      |> maybe_put_fuzzy(fuzzy)
      |> TokenFilters.rebuild_word_ngrams(max_n)
      |> Map.put(:source, if(mode == :prod, do: :prod, else: :test))
      |> Map.put(:experience_origin, Keyword.get(opts, :experience_origin))
      |> Map.put(:session_id, Keyword.get(opts, :session_id))
      |> Map.put_new(:trace, [])
      |> Map.put(:lifg_opts, lifg_opts)
      |> StateDump.capture(:initial)

    si1 =
      si0
      |> apply_intent_selection(opts)
      |> Core.Brain.WM.Comprehension.recall(opts)
      |> Core.Discourse.References.attach()
      |> StateDump.capture(:intent_selected)

    out =
      case mode do
        :prod -> run_prod_pipeline(si1, opts, lifg_opts)
        _ -> si1
      end

    out
    |> StateDump.capture(:pipeline_raw)
    |> coerce_si()
    |> StateDump.capture(:final)
  end

  defp maybe_put_fuzzy(si, nil), do: si

  defp maybe_put_fuzzy(si, fuzzy) do
    si
    |> Map.put(:fuzzy_text, fuzzy)
    |> Map.put(:fuzzy_corrections, fuzzy.corrections)
    |> Map.put(:fuzzy_aliases, fuzzy.aliases)
    |> Map.put(:fuzzy_confidence, fuzzy.confidence)
  end

  defp build_lifg_opts(opts) do
    lifg_defaults = Application.get_env(:brain, :lifg_defaults, [])
    Keyword.merge(lifg_defaults, Keyword.get(opts, :lifg_opts, []))
  end

  defp fuzzy_opts(opts) do
    [
      max_distance: Keyword.get(opts, :fuzzy_max_distance, 2),
      obsolete_synonym?: false,
      protected_words: MapSet.new(["symbrella"]),
      known_word?: fn word ->
        Code.ensure_loaded?(Db) and function_exported?(Db, :word_exists?, 1) and
          apply(Db, :word_exists?, [word])
      end
    ]
  end

  defp apply_intent_selection(%{} = si, opts) do
    aliases = Map.get(si, :fuzzy_aliases, [])

    case Selection.select(si, Keyword.put(opts, :fuzzy_aliases, aliases)) do
      {si2, _res} -> coerce_si(si2)
      si2 -> coerce_si(si2)
    end
  end

  defp run_prod_pipeline(%SemanticInput{} = si, opts, lifg_opts),
    do: run_prod_pipeline(Map.from_struct(si), opts, lifg_opts)

  defp run_prod_pipeline(%{} = si, opts, lifg_opts) when is_list(opts) and is_list(lifg_opts) do
    atl_opts = Keyword.get(opts, :atl_opts, [])

    si
    |> Core.Brain.STM.run()
    |> StateDump.capture(:stm)
    |> Core.Pipeline.Candidates.reset(:post_stm)
    |> StateDump.capture(:candidates_reset)
    |> TokenFilters.keep_only_word_boundary_tokens()
    |> StateDump.capture(:word_boundaries)
    |> Core.MWE.Stage.run(:early, opts)
    |> StateDump.capture(:mwe_early)
    |> Core.Pipeline.LTM.run(opts)
    |> StateDump.capture(:ltm)
    # Preserve the STM runtime activity separately from lexical DB candidates.
    # Feed exact BrainCell IDs into LIFG as bounded prior activation, and only
    # recruit complete, small candidate slates (large ambiguous sets are skipped).
    |> Core.Brain.Activation.prime(opts)
    |> StateDump.capture(:neuron_priming)
    |> TokenFilters.keep_only_word_boundary_tokens()
    |> StateDump.capture(:word_boundaries)
    |> Core.MWE.Stage.run(:late, opts)
    |> StateDump.capture(:mwe_late)
    |> Core.MWE.Lexicon.attach(opts)
    |> StateDump.capture(:mwe_lexicon)
    |> Core.Syntax.Signatures.attach()
    |> StateDump.capture(:sentence_signatures)
    |> Core.Syntax.RoleCompetition.attach()
    |> StateDump.capture(:role_competition)
    |> Core.Syntax.SentenceCompetition.attach()
    |> StateDump.capture(:sentence_competition)
    |> Selection.refine_from_syntax()
    |> StateDump.capture(:syntax_intent)
    |> Core.MWE.Discovery.run(opts)
    |> StateDump.capture(:mwe_discovery)
    |> Core.TokenResolution.resolve(opts)
    |> StateDump.capture(:token_resolution)
    |> Core.Relations.attach_edges()
    |> StateDump.capture(:relation_edges)
    |> Core.Pipeline.Evidence.drop_empty()
    |> StateDump.capture(:evidence)
    |> Core.Brain.Episodes.attach(opts)
    |> StateDump.capture(:episodes)
    |> Core.Pipeline.Perception.run(opts)
    |> StateDump.capture(:perception)
    |> Core.Brain.Amygdala.react(opts)
    |> StateDump.capture(:amygdala)
    |> Core.LIFG.FuzzyEntities.attach(opts)
    |> StateDump.capture(:fuzzy_entities)
    |> Core.Brain.Activation.overlay_candidate_slates()
    |> LifgAttach.run_and_attach(lifg_opts)
    |> StateDump.capture(:lifg)
    |> Core.MWE.SenseResolution.rerank_contiguous()
    |> StateDump.capture(:mwe_senses)
    |> Core.MWE.ObjectSense.refine()
    |> StateDump.capture(:object_senses)
    |> Core.Semantic.EventFrames.attach(opts)
    |> StateDump.capture(:event_frames)
    |> BrainIntrospection.update_self_model(opts)
    |> StateDump.capture(:self_model)
    |> Core.Brain.ATL.ingest(atl_opts)
    |> StateDump.capture(:atl)
    |> Core.Brain.ATL.attach_lifg_pairs(atl_opts)
    |> StateDump.capture(:atl_pairs)
    |> Core.LIFG.RelationalFeedback.attach(
      Keyword.get(opts, :lifg_relational_feedback_opts, []))
    |> StateDump.capture(:lifg_relational_feedback)
    |> Core.MWE.SenseResolution.attach_verified(opts)
    |> StateDump.capture(:verified_mwe_senses)
    # Read-only episodic projection provides independently revalidated
    # participant roles before SentenceMeaning composes the proposition.
    # No database write or response is performed by either of these stages.
    |> Core.Semantic.EpisodicProjection.attach()
    |> StateDump.capture(:episodic_projection)
    |> Core.Semantic.SentenceMeaning.attach()
    |> StateDump.capture(:sentence_meaning)
    # Prior, independently reviewed experience may propose a bounded same-POS
    # revision. Default is audit-only; live selection requires two explicit gates.
    |> Core.Brain.SynapticFeedback.attach(opts)
    |> StateDump.capture(:synaptic_feedback)
    # Intent is about the speaker's act, not a claimed verified verb sense.
    # Revalidate complete assertions before replacing low-confidence :unknown.
    |> Core.Intent.StructuralStatement.attach()
    |> StateDump.capture(:structural_statement)
    # Read-only semantic sense validation for every eligible live chat turn.
    # Reuses exact LIFG/ATL roles and their selected noun glosses; publishes
    # ONLY a bounded Explain trace, never a replacement semantic winner.
    |> Core.Brain.SemanticSenseShadow.attach(opts)
    |> StateDump.capture(:semantic_sense_shadow)
    |> Core.Comprehension.Summary.attach(opts)
    |> StateDump.capture(:comprehension)
    # Read-only post-LIFG speech-act and vocative binding. No scoring changes.
    |> Core.Semantic.SpeechActs.attach()
    |> StateDump.capture(:speech_act)
    |> Core.Brain.WM.Comprehension.store(opts)
    |> StateDump.capture(:wm_comprehension)
    |> Core.Brain.Prefrontal.attach(opts)
    |> StateDump.capture(:prefrontal)
    |> Core.Brain.ActionSelection.attach(opts)
    |> StateDump.capture(:action_selection)
    |> Core.Brain.WM.focus_prompt_topics(opts)
    |> StateDump.capture(:working_memory)
    |> maybe_build_response_plan(opts)
    |> StateDump.capture(:response_plan)
    |> Core.Brain.Hippocampus.encode(opts)
    |> StateDump.capture(:hippocampus_encode)
    |> Core.Brain.Hippocampus.persist(opts)
    |> StateDump.capture(:hippocampus_persist)
    |> Core.Brain.Activation.notify(opts)
    |> StateDump.capture(:activation)
    # The read-only A/B projection must run BEFORE this turn is recorded,
    # or the learner could appear to predict an event it just observed.
    |> Core.Brain.SynapticPropagation.attach(opts)
    |> StateDump.capture(:synaptic_propagation)
    # Record only evidence-qualified user experiences in the shadow ledger.
    # Neither step changes BrainCell weights or LIFG decisions.
    |> Core.Brain.SynapticExperience.attach(opts)
    |> StateDump.capture(:synaptic_experience)
    # Explicit patient corrections are *proposals* only. They must not
    # automatically retract prior receipts or train a new lexical association.
    |> Core.Brain.SynapticCorrection.attach(opts)
    |> StateDump.capture(:synaptic_correction)
  end

  defp run_prod_pipeline(other, _opts, _lifg_opts), do: other

  @brain_merge_blocklist MapSet.new([
                           :sentence,
                           :sentence_signatures,
                           :source,
                           :tokens,
                           :token_structs,
                           :pos_list,
                           :cells,
                           :pattern_roles,
                           :phrase_matches,
                           :sense_candidates,
                           :candidates_by_token
                         ])

  def __merge_brain_out__(%SemanticInput{} = si, %{} = brain_out) do
    allowed =
      brain_out
      |> Enum.filter(fn {key, _value} ->
        is_atom(key) and MapSet.member?(@si_fields, key) and
          not MapSet.member?(@brain_merge_blocklist, key) and key != :trace
      end)
      |> Map.new()

    trace =
      List.wrap(Map.get(brain_out, :trace)) ++
        List.wrap(Map.get(si, :trace))

    si
    |> Map.from_struct()
    |> Map.merge(allowed)
    |> Map.put(:trace, trace)
    |> then(&struct(SemanticInput, &1))
  end

  def __merge_brain_out__(si, brain_out) when is_map(si) and is_map(brain_out) do
    si
    |> coerce_si()
    |> __merge_brain_out__(brain_out)
  end

  defp maybe_build_response_plan(%{} = si, opts),
    do: ResponseAttach.maybe_build_response_plan(si, opts)

  defp maybe_build_response_plan(si, _opts), do: si

  defp wrap_si(%SemanticInput{} = si, _orig_sentence), do: si

  defp wrap_si(tokens, sentence) when is_list(tokens),
    do: %SemanticInput{sentence: sentence, tokens: tokens, source: :test, trace: []}

  defp wrap_si(other, _sentence) when is_binary(other),
    do: %SemanticInput{sentence: other, tokens: [], source: :test, trace: []}

  defp wrap_si(_other, sentence),
    do: %SemanticInput{sentence: sentence, tokens: [], source: :test, trace: []}
end
