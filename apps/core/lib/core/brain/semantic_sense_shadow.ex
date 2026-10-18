defmodule Core.Brain.SemanticSenseShadow do
  @moduledoc """
  Live, read-only semantic sense diagnostics for the Explain trace.

  Executes *after* canonical LIFG/ATL and sentence meaning have been built,
  and *before* subsequent comprehension/response generation. The result
  only appends one compact trace entry. Its hypothetical sense choice is
  never fed back into the SemanticInput, LIFG, ATL, BrainCells or response.

  The live diagnostic enables bounded one-hop actor/patient noun evidence;
  specialist-domain restrictions still require original utterance cues.
  """

  alias Core.Brain.SemanticSenseValidator
  alias Core.Pipeline.Trace

  @max_exclusions 8
  @max_ranked 3

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    if Keyword.get(opts, :semantic_sense_shadow?, true) == false do
      si
    else
      result = safe_evaluate(si, opts)

      Trace.append(si, :semantic_sense_shadow,
        decision: decision(result),
        reason: reason(result),
        meta: compact(result))
    end
  end

  def attach(other, _), do: other

  @doc "Compact, bounded read-only diagnostic. No full dictionary sense inventory."
  @spec compact(map()) :: map()
  def compact(%{status: status} = result)
      when status in [:provisional_hypothesis, :abstained] do
    %{
      mode: :shadow_only,
      status: status,
      baseline_id: Map.get(result, :baseline_id),
      proposed_id: Map.get(result, :proposed_id),
      original_surface_id: Map.get(result, :original_surface_id),
      attested_base_lexeme: Map.get(result, :attested_base_lexeme),
      sense_count: Map.get(result, :sense_count),
      excluded_count: Map.get(result, :excluded_count),
      baseline_gap: Map.get(result, :baseline_score_gap),
      evaluated_gap: Map.get(result, :validator_score_gap),
      role_expansion_enabled?: Map.get(result, :role_expansion_enabled?, false),
      role_expansion_status: Map.get(result, :role_expansion_status),
      role_sources:
        result
        |> Map.get(:role_expansion_sources, [])
        |> List.wrap()
        |> Enum.take(2)
        |> Enum.map(&Map.take(&1, [:role, :status, :noun_sense_id,
          :selected_probability, :terms])),
      top_candidates:
        result
        |> Map.get(:eligible_top, [])
        |> List.wrap()
        |> Enum.take(@max_ranked)
        |> Enum.map(&Map.take(&1, [:id, :lexical_score, :patient_support,
          :actor_support, :combined_diagnostic_score, :grammar, :one_hop_witnesses])),
      exclusions:
        result
        |> Map.get(:exclusions, [])
        |> List.wrap()
        |> Enum.take(@max_exclusions)
        |> Enum.map(&Map.take(&1, [:id, :grammar, :lexical_score,
          :missing_restrictions, :exclusions])),
      sense_verified?: false,
      gold_label_provided?: false,
      applied_to_lifg?: false,
      brain_cells_stimulated?: false,
      synaptic_weight_transferred?: false,
      response_changed?: false
    }
  end

  def compact(result) when is_map(result) do
    %{
      mode: :shadow_only,
      status: Map.get(result, :status, :skipped),
      skip_reason: Map.get(result, :reason),
      role_expansion_enabled?: true,
      sense_verified?: false,
      applied_to_lifg?: false,
      brain_cells_stimulated?: false,
      synaptic_weight_transferred?: false,
      response_changed?: false
    }
  end

  def compact(_), do: %{mode: :shadow_only, status: :invalid_result,
    applied_to_lifg?: false, response_changed?: false}

  defp safe_evaluate(si, opts) do
    # Internal injection is useful to test the read-only boundary with a
    # self-contained, genuine dictionary sense fixture. Live calls use DB.
    evaluator_opts =
      opts
      |> Keyword.take([:fetch_base_senses])
      |> Keyword.put(:role_context?, true)

    SemanticSenseValidator.evaluate(si, evaluator_opts)
  rescue
    _ -> %{status: :skipped, reason: :diagnostic_unavailable}
  catch
    :exit, _ -> %{status: :skipped, reason: :diagnostic_unavailable}
    :throw, _ -> %{status: :skipped, reason: :diagnostic_unavailable}
  end

  defp decision(%{status: status}) when status in
    [:provisional_hypothesis, :abstained, :skipped], do: status
  defp decision(_), do: :skipped

  defp reason(%{reason: reason}) when is_atom(reason), do: reason
  defp reason(_), do: :shadow_only_semantic_evidence
end
