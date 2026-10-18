defmodule Core.Brain.SynapticPropagation do
  @moduledoc """
  Read-only role-conditioned propagation into *existing* LIFG verb candidates.

  Only a structurally revalidated human-origin SVO event can bind a candidate
  predicate token to an already-selected noun patient or actor. The source
  of signal is the persistent shadow ledger, read BEFORE this turn's new
  experience is admitted. No cell stimulation or lexical reranking occurs.

  This is a diagnostic A/B result, not evidence of comprehension improvement.
  """

  alias Brain.Synapses.Propagation
  alias Core.Brain.SynapticExperience
  alias Core.Semantic.SVOEvent
  alias Core.Pipeline.Trace

  @max_candidates 256
  @max_trace_rows 12

  @spec compare(map(), keyword()) :: map()
  def compare(si, opts \\ [])

  def compare(%{} = si, opts) when is_list(opts) do
    with {:ok, %{edges: edges}} <- SynapticExperience.extract(si),
         %{roles: %{predicate: predicate_i}} <- SVOEvent.complete_signature(si),
         true <- is_integer(predicate_i),
         %{} = buckets <- Map.get(si, :sense_candidates),
         rows when is_list(rows) <-
           Map.get(buckets, predicate_i, Map.get(buckets, to_string(predicate_i))),
         [{selected_predicate, patient_id, :patient}] <-
           Enum.filter(edges, fn {_, _, role} -> role == :patient end),
         {:ok, candidates} <- candidate_ids(rows, selected_predicate),
         true <- length(candidates) <= @max_candidates do
      server = Keyword.get(opts, :synapse_shadow_server, Brain.Synapses.Shadow)

      enabled = Propagation.compare(candidates, patient_id, :patient,
        server: server, propagation?: true)
      disabled = Propagation.compare(candidates, patient_id, :patient,
        propagation?: false)

      %{
        status: if(enabled.status == :shadow_only,
          do: :shadow_comparison, else: :shadow_unavailable),
        context_role: :patient,
        patient_id: patient_id,
        predicate_index: predicate_i,
        selected_predicate_id: selected_predicate,
        before_learning?: true,
        candidates_checked: length(candidates),
        propagation_on: enabled,
        propagation_off: disabled,
        applied_to_lifg?: false,
        brain_cells_stimulated?: false,
        comprehension_improvement_measured?: false
      }
    else
      _ ->
        %{status: :ineligible, applied_to_lifg?: false,
          brain_cells_stimulated?: false}
    end
  end

  def compare(_, _), do:
    %{status: :ineligible, applied_to_lifg?: false,
      brain_cells_stimulated?: false}

  @doc "Attach compact read-only evidence before admission of the current turn."
  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    enabled? =
      Keyword.get(opts, :synapse_shadow?, true) != false and
      Keyword.get(opts, :synapse_propagation?, true) != false

    if enabled? do
      result = compare(si, opts)

      Trace.append(si, :synaptic_propagation,
        decision: result.status,
        reason: :prior_experience_role_conditioned_shadow,
        meta: compact(result))
    else
      si
    end
  end

  def attach(other, _), do: other

  defp candidate_ids(rows, selected) when is_list(rows) do
    # Do not manufacture a base-lemma sense ID or let a wrong-POS candidate
    # become a synaptic target. Also exclude fallback dictionary entries.
    surface = selected |> String.split("|", parts: 3) |> List.first()

    ids =
      rows
      |> Enum.flat_map(fn
        %{id: id} when is_binary(id) -> [id]
        %{"id" => id} when is_binary(id) -> [id]
        _ -> []
      end)
      |> Enum.uniq()
      |> Enum.filter(fn id ->
        case String.split(id, "|", parts: 3) do
          [^surface, "verb", sense] ->
            sense != "" and sense != "fallback"
          _ -> false
        end
      end)

    if selected in ids and length(ids) <= @max_candidates do
      {:ok, ids}
    else
      :ineligible
    end
  end

  defp compact(%{status: :shadow_comparison} = result) do
    on = result.propagation_on
    off = result.propagation_off

    %{
      mode: :shadow_only,
      context_role: result.context_role,
      predicate_id: result.selected_predicate_id,
      patient_id: result.patient_id,
      candidates_checked: result.candidates_checked,
      supported_on: on.supported_count,
      supported_off: off.supported_count,
      strongest_supported: on.candidates
        |> Enum.filter(&(&1.shadow_signal > 0.0))
        |> Enum.sort_by(fn row -> {-row.shadow_signal, row.id} end)
        |> Enum.take(@max_trace_rows)
        |> Enum.map(&Map.take(&1,
          [:id, :shadow_signal, :provisional_signal, :reviewed_signal])),
      applied_to_lifg?: false,
      brain_cells_stimulated?: false,
      comprehension_improvement_measured?: false,
      evaluated_before_experience_admission?: true
    }
  end

  defp compact(other) do
    %{mode: :shadow_only, status: Map.get(other, :status, :ineligible),
      applied_to_lifg?: false, brain_cells_stimulated?: false}
  end
end
