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
        session_id: Map.get(si, :session_id),
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

  @doc """
  Explicit, trusted human review of an *already recorded* experience.

  This is a programmatic reviewer boundary, not an NLP command parser.
  A chat utterance claiming "actually..." must not directly revoke weights.
  The same independently revalidated SI is required to locate its receipt.
  """
  @spec review(map(), :confirm | :reject, String.t()) ::
          {:ok, :confirmed | :rejected | :unchanged} | {:error, atom()}
  def review(%{} = si, decision, reviewer)
      when decision in [:confirm, :reject] and is_binary(reviewer) do
    case extract(si) do
      {:ok, %{evidence: evidence}} ->
        Brain.Synapses.Shadow.review(%{
          evidence: evidence,
          decision: decision,
          provenance: :human_review,
          reviewer: reviewer
        })

      :ineligible ->
        {:error, :ineligible}
    end
  end

  def review(_, _, _), do: {:error, :invalid_review}

  @doc """
  Read-only context-specific edge support for a handful of *existing*
  lexical verb candidates. No winner change, LIFG bonus, or verification.
  """
  @spec preview(map(), [String.t()]) :: map()
  def preview(%{} = si, candidate_ids) when is_list(candidate_ids) do
    with {:ok, %{edges: edges}} <- extract(si),
         [{_selected, noun_id, :patient}] <-
           Enum.filter(edges, fn {_, _, role} -> role == :patient end),
         %{roles: %{predicate: verb_index}} <- SVOEvent.complete_signature(si),
         true <- is_integer(verb_index),
         true <- candidate_ids != [] and length(candidate_ids) <= 64,
         %{} = buckets <- Map.get(si, :sense_candidates),
         rows when is_list(rows) <-
           Map.get(buckets, verb_index, Map.get(buckets, to_string(verb_index))),
         allowed <- MapSet.new(Enum.map(rows, &Map.get(&1, :id))),
         true <- Enum.all?(candidate_ids, &MapSet.member?(allowed, &1)) do
      Brain.Synapses.Shadow.preview(candidate_ids, noun_id, :patient)
    else
      _ -> %{status: :ineligible_or_incomplete_slate, candidates: []}
    end
  end

  def preview(_, _), do: %{status: :invalid_candidates, candidates: []}

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
