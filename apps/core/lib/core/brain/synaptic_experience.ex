defmodule Core.Brain.SynapticExperience do
  @moduledoc """
  Extract experience-eligible typed relations from *revalidated* user reports.

  This is not a correctness oracle. The event is structurally supported but
  its lexical senses and external truth remain unverified. Only two bounded
  provisional edges are allowed: predicate -> actor and predicate -> patient.

  The shadow ledger never provides input to LIFG and never updates BrainCells.
  """

  alias Core.Brain.SemanticPropagation
  alias Core.Semantic.{EpisodicProjection, SVOEvent, SentenceMeaning}
  alias Core.Pipeline.Trace

  @spec extract(map()) :: {:ok, map()} | :ineligible
  def extract(%{} = si) do
    with source when source in [:user, :prod] <- Map.get(si, :source),
         true <- source == :user or Map.get(si, :experience_origin) == :user_turn,
         %{status: :structurally_interpreted, kind: :participant_assertion,
           event_type: :action, source_frame: :finite_present_svo} = meaning <-
           Map.get(si, :sentence_meaning),
         true <- SentenceMeaning.build(si) == meaning,
         %{roles: %{subject_head: actor_i, predicate: verb_i, object_head: patient_i}} <-
           SVOEvent.complete_signature(si),
         %{status: :asserted, source_frame: :finite_present_svo,
           participants: %{"actor" => actor, "predicate" => predicate,
                           "patient" => patient}} <-
           EpisodicProjection.validated_participant(si),
         true <- is_binary(actor) and actor != "" and
                 is_binary(patient) and patient != "" and
                 is_binary(predicate) and predicate != "",
         true <- Enum.all?([actor_i, verb_i, patient_i], &is_integer/1),
         plan when is_list(plan) <- SemanticPropagation.role_plan(si),
         true <- length(plan) == 3,
         {:ok, actor_id} <- role_id(si, plan, actor_i, "noun"),
         {:ok, verb_id} <- role_id(si, plan, verb_i, "verb"),
         {:ok, patient_id} <- role_id(si, plan, patient_i, "noun") do
      # This binary is used only momentarily in Brain.Synapses.Shadow to
      # derive a BEAM-lifetime-keyed HMAC; no raw event is kept in the ledger.
      evidence = :erlang.term_to_binary({
        :svo_user_report_v1,
        String.downcase(String.trim(actor)),
        String.downcase(String.trim(predicate)),
        String.downcase(String.trim(patient)),
        actor_id, verb_id, patient_id
      })

      {:ok, %{
        provenance: :user_reported,
        evidence: evidence,
        edges: [
          {verb_id, actor_id, :actor},
          {verb_id, patient_id, :patient}
        ]
      }}
    else
      _ -> :ineligible
    end
  end

  def extract(_), do: :ineligible

  @doc "Attach an audit event for shadow-only experience processing."
  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    enabled? = Keyword.get(opts, :synapse_shadow?,
      Application.get_env(:brain, :synapse_shadow_enabled?, true))

    outcome =
      cond do
        not enabled? ->
          :disabled

        true ->
          case extract(si) do
            {:ok, observation} ->
              if Code.ensure_loaded?(Brain.Synapses.Shadow) do
                Brain.Synapses.Shadow.observe(observation)
              else
                {:error, :not_running}
              end

            :ineligible ->
              :ineligible
          end
      end

    decision =
      case outcome do
        {:ok, :recorded} -> :recorded
        {:ok, :duplicate} -> :duplicate
        :disabled -> :disabled
        :ineligible -> :ineligible
        {:error, _} -> :unavailable
      end

    Trace.append(si, :synaptic_experience,
      decision: decision,
      reason: :shadow_only_experience,
      meta: %{
        mode: :shadow_only,
        provenance: :user_reported_structural,
        learned_senses_verified?: false,
        applied_to_lifg?: false
      })
  end

  def attach(other, _), do: other

  defp role_id(si, plan, index, pos) do
    with [%{id: id}] <- Enum.filter(plan, fn
           %{id: id} -> String.split(id, "|", parts: 3) |> Enum.at(1) == pos and
             selected_for_index?(si, index, id)
           _ -> false
         end),
         true <- Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
           %{id: ^id} -> true
           _ -> false
         end) do
      {:ok, id}
    else
      _ -> :error
    end
  end

  defp selected_for_index?(si, index, id) do
    case Enum.filter(List.wrap(Map.get(si, :lifg_choices)), fn
           %{token_index: ^index, id: ^id} -> true
           _ -> false
         end) do
      [_] -> true
      _ -> false
    end
  end
end
