defmodule Core.Brain.SynapticFeedback do
  @moduledoc """
  Evidence-gated, post-comprehension synaptic feedback.

  Always computes a bounded A/B audit for eligible user-origin SVO statements.
  By default this is READ ONLY. Applying a new sense requires both a runtime
  configuration switch and an explicit per-turn option, at least one reviewed
  prior experience, a complete scored LIFG slate, two real same-surface verb
  senses and a narrow baseline margin.

  This is a secondary, experimental lexical selection, not trained LIFG logits
  or proof of a dictionary meaning. A pure ATL reconstruction plus independent
  episode/meaning revalidation must pass before any choice is returned.
  """
  alias Core.Brain.{SynapticDisambiguation, SynapticExperience, SynapticPropagation}
  alias Core.Pipeline.Trace
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @max_raw_gap 0.02
  @min_projected_margin 0.02
  @min_reviewed_signal 0.01
  @max_signal 0.03

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    enabled? =
      Keyword.get(opts, :synapse_shadow?, true) != false and
        Keyword.get(opts, :synaptic_feedback?, true) != false

    if enabled? do
      report = SynapticDisambiguation.evaluate(si, opts)
      {result, status, reason, applied} = maybe_apply(si, report, opts)

      Trace.append(result, :synaptic_feedback,
        decision: status,
        reason: reason,
        meta: %{
          mode: if(live_enabled?(opts), do: :explicit_live_opt_in, else: :observe_only),
          diagnostic_status: Map.get(report, :status),
          diagnostic_reason: Map.get(report, :reason),
          candidate_count: Map.get(report, :candidate_count, 0),
          verb_sense_count: Map.get(report, :verb_sense_count, 0),
          baseline: compact_decision(Map.get(report, :baseline)),
          counterfactual: compact_decision(Map.get(report, :counterfactual)),
          changed_decision?: Map.get(report, :changed_decision?, false),
          hypothetical_additive_probability?: true,
          applied_to_lifg?: status == :applied,
          brain_cells_stimulated?: false,
          change: applied,
          guard: reason,
          evaluated_before_experience_admission?: true
        })
    else
      si
    end
  end

  def attach(si, _opts), do: si

  defp live_enabled?(opts) do
    Application.get_env(:core, :synaptic_feedback_live_enabled?, false) == true and
      Keyword.get(opts, :synaptic_feedback_live?, false) == true
  end

  defp maybe_apply(si, %{status: :skipped, reason: reason}, _opts),
    do: {si, :skipped, reason, nil}

  defp maybe_apply(si, report, opts) do
    cond do
      not live_enabled?(opts) ->
        {si, :observed, :live_gate_closed, nil}

      Map.get(si, :source) != :prod or
          Map.get(si, :experience_origin) != :user_turn ->
        {si, :guarded, :not_live_user_turn, nil}

      report.status != :unlabeled_diagnostic ->
        {si, :guarded, :not_unlabeled_counterfactual, nil}

      true ->
        case eligible_change(si, report, opts) do
          {:ok, index, source_id, target_id, signal} ->
            case reproject(si, index, source_id, target_id, signal) do
              {:ok, revised} ->
                change = %{token_index: index, from: source_id, to: target_id,
                           reviewed_signal: signal.reviewed_signal,
                           provisional_signal: signal.provisional_signal,
                           evidence_tier: :human_reviewed_prior_experience,
                           lexical_meaning_verified?: false}
                {revised, :applied, :reviewed_role_conditioned_prior, change}

              {:error, reason} ->
                {si, :guarded, reason, nil}
            end

          {:error, reason} ->
            {si, :guarded, reason, nil}
        end
    end
  end

  defp eligible_change(si, report, opts) do
    index = Map.get(report, :predicate_index)
    before = Map.get(report, :baseline, %{})
    after_signal = Map.get(report, :counterfactual, %{})
    target_id = Map.get(after_signal, :decision)

    with true <- is_integer(index) and index >= 0,
         %{id: source_id, candidate_probs: scores} = choice <-
           unique_choice(Map.get(si, :lifg_choices), index),
         true <- is_binary(source_id) and is_map(scores),
         true <- before.top_id == source_id,
         true <- is_binary(target_id) and target_id != source_id,
         true <- same_surface_verb?(source_id, target_id),
         true <- Map.has_key?(scores, target_id),
         true <- abs(Map.fetch!(scores, source_id) -
                     Map.fetch!(scores, target_id)) <= @max_raw_gap,
         true <- is_number(after_signal.margin) and
                   after_signal.margin >= @min_projected_margin,
         true <- unshadowed_semantic_choice?(si, index, source_id),
         true <- not revised_by_atl?(si, index),
         true <- available_candidate?(si, index, target_id),
         %{status: :shadow_comparison, propagation_on: %{candidates: rows}} <-
           SynapticPropagation.compare(si, opts),
         %{id: ^target_id} = signal <-
           Enum.find(rows, &(Map.get(&1, :id) == target_id)),
         true <- is_number(signal.reviewed_signal) and
                   signal.reviewed_signal >= @min_reviewed_signal,
         true <- is_number(signal.shadow_signal) and
                   signal.shadow_signal <= @max_signal,
         true <- Map.get(signal, :reviewed_contexts, 0) > 0,
         true <- Map.get(choice, :kind) != :mwe do
      {:ok, index, source_id, target_id, signal}
    else
      _ -> {:error, :insufficient_reviewed_same_pos_evidence}
    end
  end

  defp reproject(si, index, source_id, target_id, signal) do
    try do
      candidate = Enum.find(candidate_bucket(si, index),
        &(field(&1, :id) == target_id))

      choices = Map.get(si, :lifg_choices)
      semantic = Map.get(si, :semantic_choices)
      tokens = Map.get(si, :tokens)
      signatures = Map.get(si, :sentence_signatures)

      with true <- is_map(candidate) and is_list(choices) and
                     is_list(semantic) and is_list(tokens) and is_list(signatures),
           updated_choices <- update_choices(choices, index, source_id, candidate,
             signal),
           updated_semantic <- update_choices(semantic, index, source_id, candidate,
             signal),
           # The ATL reducer is pure: no second write to the ATL GenServer.
           %{} = slate <- Brain.ATL.reduce(updated_semantic, tokens, signatures),
           revised <- si
             |> Map.put(:lifg_choices, updated_choices)
             |> Map.put(:semantic_choices, updated_semantic)
             |> Map.put(:atl_slate, slate)
             |> Brain.ATL.attach_lifg_pairs([])
             |> EpisodicProjection.attach()
             |> SentenceMeaning.attach(),
           true <- independently_revalidated?(si, revised, index, target_id) do
        {:ok, revised}
      else
        _ -> {:error, :reprojection_not_independently_revalidated}
      end
    rescue
      _ -> {:error, :reprojection_failed}
    catch
      _, _ -> {:error, :reprojection_failed}
    end
  end

  defp independently_revalidated?(before, after_si, index, target_id) do
    before_event = EpisodicProjection.validated_participant(before)
    after_event = EpisodicProjection.validated_participant(after_si)
    before_meaning = Map.get(before, :sentence_meaning, %{})
    after_meaning = Map.get(after_si, :sentence_meaning, %{})

    same_actors? =
      is_map(before_event) and is_map(after_event) and
        Map.get(before_event, :source_frame) == :finite_present_svo and
        Map.get(after_event, :source_frame) == :finite_present_svo and
        Enum.all?(["actor", "patient", "surface_predicate"], fn role ->
          get_in(before_event, [:participants, role]) ==
            get_in(after_event, [:participants, role])
        end)

    same_structure? =
      is_map(before_meaning) and is_map(after_meaning) and
        Enum.all?([:status, :kind, :event_type, :source_frame], fn key ->
          Map.get(before_meaning, key) == Map.get(after_meaning, key)
        end) and after_meaning.status == :structurally_interpreted

    revalidated? =
      case SynapticExperience.extract(after_si) do
        {:ok, %{edges: edges}} ->
          Enum.any?(edges, fn {verb, _noun, role} ->
            verb == target_id and role == :patient
          end)

        _ -> false
      end

    same_actors? and same_structure? and revalidated? and
      match?(%{id: ^target_id}, unique_choice(Map.get(after_si, :lifg_choices), index))
  end

  defp update_choices(choices, index, source_id, candidate, signal) do
    Enum.map(choices, fn
      %{token_index: ^index, id: ^source_id} = choice ->
        score = Map.get(Map.get(choice, :candidate_probs, %{}), field(candidate, :id),
          Map.get(choice, :score, 0.0))

        choice
        |> Map.put(:id, field(candidate, :id))
        |> Map.put(:pos, "verb")
        |> Map.put(:score, score)
        |> Map.put(:definition, field(candidate, :definition))
        |> Map.put(:synaptic_feedback_uncertain?, true)
        |> Map.put(:synaptic_feedback, %{
          source: :reviewed_shadow_synapse,
          calibrated_lifg_score?: false,
          reviewed_signal: signal.reviewed_signal
        })

      choice -> choice
    end)
  end

  defp unique_choice(choices, index) when is_list(choices) do
    case Enum.filter(choices, fn
      %{token_index: ^index} -> true
      _ -> false
    end) do
      [choice] -> choice
      _ -> nil
    end
  end

  defp unique_choice(_, _), do: nil

  defp unshadowed_semantic_choice?(si, index, source_id) do
    case unique_choice(Map.get(si, :semantic_choices), index) do
      %{id: ^source_id} = choice -> Map.get(choice, :kind) != :mwe
      _ -> false
    end
  end

  defp revised_by_atl?(si, index) do
    revisions = get_in(si, [:lifg_feedback, :applied_revisions])
    Enum.any?(List.wrap(revisions), &(Map.get(&1, :token_index) == index))
  end

  defp candidate_bucket(si, index) do
    case Map.get(si, :sense_candidates) do
      %{} = buckets ->
        case Map.get(buckets, index, Map.get(buckets, to_string(index))) do
          rows when is_list(rows) -> rows
          _ -> []
        end
      _ -> []
    end
  end

  defp available_candidate?(si, index, id) do
    Enum.any?(candidate_bucket(si, index), &(field(&1, :id) == id)) and
      Enum.any?(List.wrap(Map.get(si, :active_cells)),
        &(field(&1, :id) == id))
  end

  defp same_surface_verb?(one, two) do
    case {String.split(one, "|", parts: 3), String.split(two, "|", parts: 3)} do
      {[surface, "verb", sense_one], [other_surface, "verb", sense_two]} ->
        surface == other_surface and surface != "" and
          sense_one != "" and sense_two != "" and
          sense_one != "fallback" and sense_two != "fallback"
      _ -> false
    end
  end

  defp field(%{} = map, key),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp field(_, _), do: nil

  defp compact_decision(%{} = report),
    do: Map.take(report, [:top_id, :top_probability, :margin, :decision,
                         :abstained?])
  defp compact_decision(_), do: nil
end
