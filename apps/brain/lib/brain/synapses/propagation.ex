defmodule Brain.Synapses.Propagation do
  @moduledoc """
  One-hop, role-conditioned *counterfactual* activation from learned receipts.

  A previously observed verb->noun typed edge may lend bounded support to
  that *same existing verb sense ID* when the noun fires in the same role.
  This is NOT actual Brain.Cell stimulation, verified sense meaning, a
  synaptic update, or a LIFG score change.

  Human-reviewed and provisional contributions are carried separately.
  Unreviewed experience gets a smaller exploratory gain. No reverse
  lexical-morphology bridge, multi-hop traversal or general coactivation.
  """

  alias Brain.Synapses.Shadow

  @max_candidates 256
  @max_signal 0.03
  @provisional_gain 0.25
  @reviewed_gain 1.0

  @spec compare([String.t()], String.t(), :actor | :patient, keyword()) :: map()
  def compare(candidates, noun_id, relation, opts \\ [])

  def compare(candidates, noun_id, relation, opts)
      when is_list(candidates) and is_binary(noun_id) and
           relation in [:actor, :patient] and is_list(opts) do
    ids = Enum.uniq(candidates)

    if ids != [] and length(ids) <= @max_candidates and
         Enum.all?(ids, &valid_verb_id?/1) do
      server = Keyword.get(opts, :server, Shadow)
      propagation? = Keyword.get(opts, :propagation?, true) != false

      preview =
        if propagation? do
          try do
            Shadow.preview(ids, noun_id, relation, server)
          catch
            :exit, _ -> %{status: :unavailable, candidates: []}
          end
        else
          %{status: :propagation_off, candidates: []}
        end

      case preview do
        %{status: status, candidates: samples}
            when status in [:shadow_only, :propagation_off] and is_list(samples) ->
          by_id = Map.new(samples, &{&1.id, &1})

          rows =
            Enum.map(ids, fn id ->
              sample = Map.get(by_id, id, %{})
              provisional = number(sample, :provisional_support)
              reviewed = number(sample, :reviewed_support)
              signal =
                min(@max_signal,
                  provisional * @provisional_gain + reviewed * @reviewed_gain)

              %{
                id: id, shadow_signal: signal,
                provisional_signal: provisional * @provisional_gain,
                reviewed_signal: reviewed * @reviewed_gain,
                provisional_contexts: Map.get(sample, :provisional_contexts, 0),
                reviewed_contexts: Map.get(sample, :reviewed_contexts, 0),
                exact_sense_verified?: false
              }
            end)

          %{
            status: if(propagation?, do: :shadow_only, else: :propagation_off),
            from_noun_id: noun_id,
            to_role: relation,
            baseline_candidate_order: ids,
            signal_only_order: Enum.sort_by(rows, fn r -> {-r.shadow_signal, r.id} end)
              |> Enum.map(& &1.id),
            candidates: rows,
            supported_count: Enum.count(rows, &(&1.shadow_signal > 0)),
            max_signal: @max_signal,
            applied_to_lifg?: false,
            brain_cells_stimulated?: false,
            learned_meaning_verified?: false
          }

        _ ->
          %{
            status: :unavailable,
            candidates: [],
            applied_to_lifg?: false,
            brain_cells_stimulated?: false
          }
      end
    else
      %{status: :invalid_or_oversized_slate, candidates: [],
        applied_to_lifg?: false, brain_cells_stimulated?: false}
    end
  end

  def compare(_, _, _, _),
    do: %{status: :invalid_or_oversized_slate, candidates: [],
      applied_to_lifg?: false, brain_cells_stimulated?: false}

  defp number(sample, key) do
    case Map.get(sample, key) do
      v when is_number(v) and v > 0 -> min(v * 1.0, 0.08)
      _ -> 0.0
    end
  end

  defp valid_verb_id?(id) when is_binary(id) and byte_size(id) in 3..128 do
    case String.split(id, "|", parts: 3) do
      [surface, "verb", sense] ->
        surface != "" and sense != "" and sense != "fallback"
      _ -> false
    end
  end

  defp valid_verb_id?(_), do: false
end
