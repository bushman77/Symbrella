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
    prior = map_or_empty(Map.get(si, :neuron_activation))
    rows = list_or_empty(Map.get(si, :active_cells))
    updated = Enum.map(rows, &put_runtime_prior(&1, prior))
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

  def prime(si, _opts), do: si

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

    (selected_plan ++ verified_plan)
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

    if Keyword.get(opts, :notify_brain?, true) do
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

    Map.update(si, :trace, [], fn trace ->
      [{:activated,
        %{
          rows: length(plan),
          lifg_choices: length(list_or_empty(Map.get(si, :lifg_choices))),
          shape: :selective_comprehension_feedback,
          verified: Enum.count(plan, &(&1.evidence == :verified_lexical_construction)),
          differentiated: Enum.count(plan, &(&1.evidence == :ranked_not_verified)),
          unresolved: Enum.count(plan, &(&1.evidence == :unresolved_sense_selection))
        }} | trace]
    end)
  end

  def notify(si, _opts), do: si

  defp put_runtime_prior(%{} = cell, prior) do
    id = cell_id(cell)
    observed = if real_id?(id), do: Map.get(prior, id), else: nil

    if is_number(observed) and observed > 0 and not Map.has_key?(cell, :activation) do
      # Runtime activity is a small PRIOR, never a probability of correctness.
      # Keep the original LTM score for diagnostics; do not alter dictionary data.
      baseline = number(Map.get(cell, :score), 0.5)
      activation = clamp01(baseline + @runtime_feature_boost * clamp01(observed))

      cell
      |> Map.put(:activation, activation)
      |> Map.put(:runtime_primed?, true)
      |> Map.put(:prior_neuron_activation, observed)
    else
      cell
    end
  end

  defp put_runtime_prior(cell, _prior), do: cell

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

  defp discriminated?(choice) do
    id = cell_id(choice)
    scores = Map.get(choice, :candidate_probs)

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
