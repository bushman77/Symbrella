defmodule Core.Social.BrainObservation do
  @moduledoc """
  Explicit, bounded live-neuron response to externally observed language.

  Colony is untrusted: the same dictionary-backed, context-gated lexical
  candidates used by chat can *transiently* excite Brain.Cell processes, but
  never enter hippocampal memory, WM, synapses or verified knowledge here.
  This module does not call Core.resolve_input/2 or Brain.ATL.ingest/3.

  Unlike the existing social activity counters, these are real neuron
  activation requests. No raw source text or sense IDs are published.
  """

  alias Core.Brain.Activation
  alias Core.Brain.Runtime

  @max_targets_per_sentence 4
  @max_delta 0.03

  @spec observe(map(), keyword()) :: map()
  def observe(si, opts \\ [])

  def observe(%{source: :colony_external, experience_origin: :colony_external} = si, opts)
      when is_list(opts) do
    enabled? = Keyword.get(opts, :enabled?, false) == true

    available? =
      case Keyword.fetch(opts, :brain_available?) do
        {:ok, value} -> value == true
        :error -> Runtime.pid_alive?(Brain)
      end

    cond do
      not enabled? ->
        result(:disabled, 0)

      not available? ->
        result(:brain_unavailable, 0)

      true ->
        attested =
          si
          |> Map.get(:active_cells, [])
          |> List.wrap()
          |> Enum.flat_map(fn
            %{id: id} when is_binary(id) -> [id]
            _ -> []
          end)
          |> MapSet.new()

        plan =
          Activation.feedback_plan(si,
            semantic_propagation?: false,
            max_feedback: @max_targets_per_sentence
          )
          |> Enum.filter(fn
            %{id: id, delta: delta} ->
              is_binary(id) and MapSet.member?(attested, id) and
                is_number(delta) and delta > 0 and delta <= 1.0 and
                not String.ends_with?(id, "|fallback")

            _ ->
              false
          end)
          |> Enum.take(@max_targets_per_sentence)

        activator = Keyword.get(opts, :activator, &Core.Brain.activate_cells/2)

        plan
        |> Enum.group_by(fn entry -> min(entry.delta, @max_delta) end)
        |> Enum.each(fn {delta, entries} ->
          activator.(Enum.map(entries, & &1.id), %{
            delta: delta,
            via: :colony_external_observation
          })
        end)

        # One aggregate telemetry receipt is issued by SocialBrainActivity
        # after the full thread completes. Avoid flooding the Brain blackboard
        # with dozens of indistinguishable per-sentence events.

        result(if(plan == [], do: :no_attested_targets, else: :stimuli_issued), length(plan))
    end
  end

  def observe(_si, _opts), do: result(:unsupported_source, 0)

  defp result(status, count) do
    %{
      status: status,
      source: :colony_external,
      mode: :transient_neural_observation,
      stimuli_issued: count,
      maximum_delta: @max_delta,
      memory_written?: false,
      synapses_modified?: false,
      factual_authority?: false
    }
  end
end
