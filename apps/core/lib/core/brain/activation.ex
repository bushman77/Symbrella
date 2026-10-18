defmodule Core.Brain.Activation do
  @moduledoc """
  Boundary between comprehension and transient Brain.Cell excitation.

  Two phases are deliberately separate:
    * prime/2: attach previous-turn neuron activity to the *exact same* lexical
      sense IDs, and weakly recruit only small, complete candidate slates;
    * notify/2: after comprehension, preferentially stimulate independently
      verified lexical meanings, while weakly representing unresolved choices.

  Neither phase verifies a sense, promotes facts, persists neuron activity or
  manufactures missing synaptic relationships. LIFG still owns sense scoring.
  """

  @initial_delta 0.01
  @max_initial_per_token 8
  @max_initial_total 32
  @max_feedback 24
  @verified_delta 0.12
  @differentiated_delta 0.05
  @uncertain_delta 0.015
  @runtime_feature_boost 0.4

  @doc """
  Use previously observed activation only for the matching BrainCell sense ID.

  :active_cells continues to mean DB sense candidates; :neuron_activation
  remains the read-only runtime snapshot from STM. A missing neuron report
  leaves the candidate's original features unchanged.

  Small unambiguous *slates* receive weak exploratory stimulation. Large
  ambiguous slates are skipped rather than arbitrarily selecting the first N
  senses or spinning up hundreds of processes.
  """
  @spec prime(map(), keyword()) :: map()
  def prime(si, opts \\ [])

  def prime(%{} = si, opts) when is_list(opts) do
    if Keyword.get(opts, :neuron_activation?, true) == false do
      # Explicit controls (benchmark/tests) must not borrow previous-turn
      # activity or emit new neural stimuli.
      si |> Map.put(:neuron_activation, %{}) |> Map.put(:semantic_pulses, %{})
    else
      # A diagnostic can supply an isolated prior without mutating Brain's
      # singleton state. Ordinary chat always uses the live STM snapshot.
      prior =
        case Keyword.fetch(opts, :neuron_probe_snapshot) do
          {:ok, %{} = isolated} -> isolated
          _ -> map_or_empty(Map.get(si, :neuron_activation))
        end

      semantic =
        if Keyword.get(opts, :semantic_propagation?, true) == false do
          %{}
        else
          case Keyword.fetch(opts, :semantic_probe_snapshot) do
            {:ok, %{} = snapshot} -> snapshot
            _ -> map_or_empty(Map.get(si, :semantic_pulses))
          end
        end
      si = si |> Map.put(:neuron_activation, prior) |> Map.put(:semantic_pulses, semantic)
      rows = list_or_empty(Map.get(si, :active_cells))
      updated = Enum.map(rows, &put_runtime_prior(&1, prior, semantic))
      recruited = initial_ids(updated)

      if Keyword.get(opts, :notify_brain?, true) and recruited != [] do
        _ = Core.Brain.activate_cells(recruited, %{delta: @initial_delta, via: :lexical_recruitment})
      end

      si
      |> Map.put(:active_cells, updated)
      |> Core.Pipeline.Trace.append(:neuron_priming,
        decision: :bounded,
        reason: :lexical_recruitment_and_prior_activation,
        meta: %{
          candidates: length(updated),
          previously_active: Enum.count(updated, fn
            %{runtime_primed?: true} -> true
            _ -> false
          end),
          recruited: length(recruited),
          skipped_ambiguous_slates?: length(recruited) < length(Enum.uniq_by(rows, &cell_id/1))
        }
      )
    end
  end

  def prime(si, _opts), do: si

  @doc "Return the exact small-slate recruitment IDs used by prime/2, without stimulation."
  @spec recruitment_ids(map()) :: [String.t()]
  def recruitment_ids(%{} = si), do: initial_ids(list_or_empty(Map.get(si, :active_cells)))
  def recruitment_ids(_), do: []

  @doc "The fixed weak recruitment signal, exposed to the read-only A/B probe."
  @spec recruitment_delta() :: float()
  def recruitment_delta, do: @initial_delta * 1.0

  @doc """
  Apply the same exact-ID prior to prebuilt candidate buckets immediately
  before LIFG. Stage1 gives sense_candidates and candidates_by_token higher
  precedence than active_cells, so annotating LTM rows alone is insufficient.
  """
  @spec overlay_candidate_slates(map()) :: map()
  def overlay_candidate_slates(%{} = si) do
    prior = map_or_empty(Map.get(si, :neuron_activation))
    semantic = map_or_empty(Map.get(si, :semantic_pulses))

    Enum.reduce([:sense_candidates, :candidates_by_token], si, fn field, acc ->
      case Map.get(acc, field) do
        %{} = slate ->
          modified =
            Map.new(slate, fn {idx, rows} ->
              candidates = if is_list(rows), do: Enum.map(rows, &put_runtime_prior(&1, prior, semantic)), else: rows
              {idx, candidates}
            end)

          Map.put(acc, field, modified)

        _ ->
          acc
      end
    end)
  end

  def overlay_candidate_slates(si), do: si

  @doc "Return a bounded, testable stimulation plan without sending neural signals."
  @spec feedback_plan(map(), keyword()) :: [map()]
  def feedback_plan(si, opts \\ [])

  def feedback_plan(%{} = si, opts) when is_list(opts) do
    eligible =
      si
      |> Map.get(:active_cells)
      |> list_or_empty()
      |> Enum.map(&cell_id/1)
      |> Enum.filter(&real_id?/1)
      |> MapSet.new()

    verified =
      si
      |> verified_ids()
      |> MapSet.new()

    selected =
      case Map.get(si, :semantic_choices) do
        choices when is_list(choices) -> choices
        _ -> list_or_empty(Map.get(si, :lifg_choices))
      end

    selected_plan =
      selected
      |> Enum.filter(&is_map/1)
      |> Enum.flat_map(fn choice ->
        id = cell_id(choice)

        cond do
          not real_id?(id) -> []
          not MapSet.member?(eligible, id) and not MapSet.member?(verified, id) -> []
          # In particular, don't carry a wrong adjective/noun or noun/verb
          # selection into the next turn when independent grammar contradicts
          # its POS. This is only a feedback veto; LIFG choices remain visible.
          grammatically_contradicted?(si, choice, id) -> []
          MapSet.member?(verified, id) ->
            [%{id: id, delta: @verified_delta, evidence: :verified_lexical_construction}]

          discriminated?(choice) ->
            [%{id: id, delta: @differentiated_delta, evidence: :ranked_not_verified}]

          true ->
            [%{id: id, delta: @uncertain_delta, evidence: :unresolved_sense_selection}]
        end
      end)

    # A verified lexical construction may cover several tokens and therefore
    # have no unigram entry among semantic_choices.
    verified_plan =
      Enum.map(verified, fn id ->
        %{id: id, delta: @verified_delta, evidence: :verified_lexical_construction}
      end)

    role_plan =
      if Keyword.get(opts, :semantic_propagation?, true) == false,
        do: [], else: Core.Brain.SemanticPropagation.role_plan(si)

    (selected_plan ++ verified_plan ++ role_plan)
    |> Enum.group_by(& &1.id)
    |> Enum.map(fn {_id, contributions} -> Enum.max_by(contributions, & &1.delta) end)
    |> Enum.sort_by(fn entry -> {-entry.delta, entry.id} end)
    |> Enum.take(positive_limit(Keyword.get(opts, :max_feedback, @max_feedback)))
  end

  def feedback_plan(_, _opts), do: []

  @doc "Notify the runtime of evidence-tiered post-comprehension stimulation."
  @spec notify(map(), keyword()) :: map()
  def notify(si, opts \\ [])

  def notify(%{} = si, opts) when is_list(opts) do
    plan = feedback_plan(si, opts)
    emitted? =
      Keyword.get(opts, :neuron_activation?, true) != false and
        Keyword.get(opts, :notify_brain?, true)

    pulse =
      if Keyword.get(opts, :semantic_propagation?, true) != false,
        do: Core.Brain.SemanticPropagation.lemma_pulse(si), else: nil

    if emitted? do
      if is_map(pulse) and is_binary(Map.get(si, :session_id)) and
           is_pid(Process.whereis(Brain)) do
        _ = Core.Brain.Runtime.apply_if_exported(Brain, :semantic_pulse,
          [si.session_id, pulse.lemma, pulse.source_id, pulse.delta], :ok)
      end

      plan
      |> Enum.group_by(& &1.delta)
      |> Enum.each(fn {delta, entries} ->
        _ =
          Core.Brain.activate_cells(Enum.map(entries, & &1.id), %{
            delta: delta,
            via: :comprehension_feedback
          })
      end)
    end

    counts = %{
      verified: Enum.count(plan, &(&1.evidence == :verified_lexical_construction)),
      differentiated: Enum.count(plan, &(&1.evidence == :ranked_not_verified)),
      unresolved: Enum.count(plan, &(&1.evidence == :unresolved_sense_selection)),
      structural: Enum.count(plan, &(&1.evidence == :structural_role_not_lexically_verified))
    }

    si
    |> Core.Pipeline.Trace.append(:neuron_feedback,
      decision: if(emitted?, do: :stimulated, else: :suppressed),
      reason: :post_comprehension_evidence_tiers,
      meta: Map.merge(counts, %{
        planned: length(plan),
        emitted?: emitted?,
        # Bounded audit for Explain; does not claim these IDs were verified.
        targets: Enum.take(plan, 8),
        lemma_pulse: pulse,
        lemma_pulse_emitted?: emitted? and is_map(pulse) and
          is_binary(Map.get(si, :session_id))
      })
    )
    |> Map.update(:trace, [], fn trace ->
      [{:activated,
        %{
          rows: if(emitted?, do: length(plan), else: 0),
          planned: length(plan),
          emitted?: emitted?,
          lifg_choices: length(list_or_empty(Map.get(si, :lifg_choices))),
          shape: :selective_comprehension_feedback,
          verified: counts.verified,
          differentiated: counts.differentiated,
          unresolved: counts.unresolved,
          structural: counts.structural
        }} | trace]
    end)
  end

  def notify(si, _opts), do: si

  defp put_runtime_prior(%{} = cell, prior, semantic) do
    id = cell_id(cell)
    exact = if real_id?(id), do: number(Map.get(prior, id), 0.0), else: 0.0
    hub = semantic_candidate_pulse(id, semantic)
    observed = max(exact, hub)

    if observed > 0 and not Map.has_key?(cell, :activation) do
      # Existing base-lemma verb candidates all receive the same small hub
      # feature. No sense ID is created and no individual base sense is verified.
      baseline = number(Map.get(cell, :score), 0.5)
      activation = clamp01(baseline + @runtime_feature_boost * clamp01(observed))

      cell
      |> Map.put(:activation, activation)
      |> Map.put(:runtime_primed?, true)
      |> Map.put(:prior_neuron_activation, exact)
      |> Map.put(:prior_semantic_activation, hub)
    else
      cell
    end
  end

  defp put_runtime_prior(cell, _prior, _semantic), do: cell

  defp semantic_candidate_pulse(id, semantic) when is_binary(id) and is_map(semantic) do
    case String.split(id, "|", parts: 3) do
      [lemma, "verb", sense] when sense != "" and sense != "fallback" ->
        semantic |> Map.get(lemma) |> number(0.0) |> max(0.0) |> min(0.03)
      _ -> 0.0
    end
  end

  defp semantic_candidate_pulse(_, _), do: 0.0

  defp initial_ids(rows) do
    rows
    |> Enum.filter(&is_map/1)
    |> Enum.group_by(&Map.get(&1, :token_index))
    |> Enum.sort_by(fn {index, _} -> if is_integer(index), do: index, else: -1 end)
    |> Enum.flat_map(fn
      {index, group} when is_integer(index) ->
        ids = group |> Enum.map(&cell_id/1) |> Enum.filter(&real_id?/1) |> Enum.uniq()
        if length(ids) <= @max_initial_per_token, do: ids, else: []

      _ -> []
    end)
    |> Enum.uniq()
    |> Enum.take(@max_initial_total)
  end

  defp grammatically_contradicted?(si, choice, id) do
    index = Map.get(choice, :token_index)
    competition = map_or_empty(Map.get(si, :role_competition))
    roles = map_or_empty(Map.get(competition, :roles))

    case {Map.get(competition, :source), Map.get(roles, index), String.split(id, "|", parts: 3)} do
      {:core_syntax_and_attested_dictionary_pos,
       %{status: :supported, pos: expected}, [_, selected, _]}
      when is_binary(expected) -> selected != expected

      _ -> false
    end
  end

  defp discriminated?(choice) do
    id = cell_id(choice)
    scores = Map.get(choice, :candidate_probs)
    control = map_or_empty(Map.get(choice, :semantic_control))

    # A probability gap by itself is not independent evidence: an earlier
    # activation prior can create such a gap and repeatedly reinforce itself.
    # Only a context-derived SemanticControl adjustment can license this tier.
    independent_context? =
      Map.get(control, :applied?) == true and
        Enum.any?(list_or_empty(Map.get(control, :adjustments)), fn
          %{id: ^id, source: :original_sentence_dictionary_context} -> true
          _ -> false
        end)

    if independent_context? do
      case is_map(scores) && Map.get(scores, id) do
        p when is_number(p) and p > 0 and map_size(scores) > 1 ->
          other =
            scores
            |> Enum.reject(fn {other_id, _} -> other_id == id end)
            |> Enum.map(fn {_, value} -> if is_number(value), do: value, else: 0.0 end)
            |> Enum.max(fn -> 0.0 end)

          p - other >= 0.001 and (p - other) / p >= 0.12

        _ ->
          false
      end
    else
      false
    end
  end

  defp verified_ids(si) do
    comprehension = map_or_empty(Map.get(si, :comprehension))

    comprehension
    |> Map.get(:lexical_meanings, [])
    |> list_or_empty()
    |> Enum.map(&cell_id/1)
    |> Enum.filter(&real_id?/1)
    |> Enum.uniq()
  end

  defp cell_id(%{} = row), do: Map.get(row, :id) || Map.get(row, "id")
  defp cell_id(id) when is_binary(id), do: id
  defp cell_id(_), do: nil

  defp real_id?(id) when is_binary(id) do
    case String.split(id, "|") do
      [norm, pos] -> norm != "" and pos != "" and pos != "fallback"
      [norm, pos, suffix] -> norm != "" and pos != "" and suffix != "fallback"
      _ -> false
    end
  end

  defp real_id?(_), do: false

  defp map_or_empty(%{} = m), do: m
  defp map_or_empty(_), do: %{}
  defp list_or_empty(list) when is_list(list), do: list
  defp list_or_empty(_), do: []
  defp number(x, _) when is_number(x), do: x * 1.0
  defp number(_, default), do: default
  defp clamp01(x), do: x |> max(0.0) |> min(1.0)
  defp positive_limit(n) when is_integer(n) and n > 0, do: min(n, @max_feedback)
  defp positive_limit(_), do: @max_feedback
end
