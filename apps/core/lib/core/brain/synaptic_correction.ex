defmodule Core.Brain.SynapticCorrection do
  @moduledoc """
  Conservative, read-only contradiction *proposals* from explicit corrections.

  Recognizes only "Actually, the SUBJECT VERB a NEW, not a OLD." with a
  single-word subject, verb and each object in a determined five-token SVO.
  Both reconstructed positive clauses must independently survive the normal
  Core -> LIFG -> ATL -> SentenceMeaning -> EpisodicProjection validation.

  A contradiction *proposal* is not an automatic retraction or replacement:
  the same actor may have participated in two different real events.
  Existing human-reviewed receipts and connection weights remain unchanged.
  """

  alias Core.Brain.SynapticExperience
  alias Core.Pipeline.Trace

  defp regex_correction, do: ~r/\A\s*actually\s*,\s*(?<subject>(?:the|a|an)\s+\p{L}+)\s+(?<verb>\p{L}+)\s+(?<new_det>a|an|the)\s+(?<new_patient>\p{L}+)\s*,\s*not\s+(?<old_det>a|an|the)\s+(?<old_patient>\p{L}+)\.?\s*\z/iu

  @doc "Parse an explicit, bounded surface correction without inventing meanings."
  @spec parse(String.t()) :: {:ok, map()} | :not_a_supported_correction
  def parse(text) when is_binary(text) and byte_size(text) <= 256 do
    case Regex.named_captures(regex_correction(), text) do
      %{"subject" => subject, "verb" => verb,
        "new_det" => new_det, "new_patient" => new_patient,
        "old_det" => old_det, "old_patient" => old_patient} ->
        old_phrase = String.downcase(old_det <> " " <> old_patient)
        new_phrase = String.downcase(new_det <> " " <> new_patient)

        if old_phrase != new_phrase do
          {:ok, %{
            prior: "#{subject} #{verb} #{old_det} #{old_patient}.",
            replacement: "#{subject} #{verb} #{new_det} #{new_patient}.",
            evidence_source: :explicit_patient_negation
          }}
        else
          :not_a_supported_correction
        end

      _ ->
        :not_a_supported_correction
    end
  end

  def parse(_), do: :not_a_supported_correction

  @doc """
  Compare independently verified prior/replacement clause interpretations
  against actual shadow receipts. Does not change any connection.
  """
  @spec evaluate(map(), map()) :: map()
  def evaluate(%{} = prior_si, %{} = replacement_si) do
    with {:ok, old_observation} <- SynapticExperience.extract(prior_si),
         {:ok, new_observation} <- SynapticExperience.extract(replacement_si) do
      case Brain.Synapses.Shadow.propose_correction(old_observation, new_observation) do
        %{} = result -> result
        _ -> %{status: :shadow_unavailable}
      end
    else
      _ -> %{status: :independent_structure_not_established}
    end
  end

  def evaluate(_, _), do: %{status: :independent_structure_not_established}

  @doc "Run only for explicit, human-origin user corrections in production."
  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{} = si, opts) when is_list(opts) do
    enabled? =
      Keyword.get(opts, :synapse_shadow?,
        Application.get_env(:brain, :synapse_shadow_enabled?, true)) and
        Keyword.get(opts, :synapse_correction?, true)

    with true <- enabled?,
         :prod <- Map.get(si, :source),
         :user_turn <- Map.get(si, :experience_origin),
         {:ok, parts} <- parse(Map.get(si, :sentence)),
         {:ok, previous} <- read_only_comprehension(parts.prior, opts),
         {:ok, replacement} <- read_only_comprehension(parts.replacement, opts) do
      result = evaluate(previous, replacement)

      # Keep plaintext and opaque HMAC evidence out of the Explain trace.
      Trace.append(si, :synaptic_correction,
        decision: Map.get(result, :status, :unavailable),
        reason: :explicit_patient_correction_shadow,
        meta: result
          |> Map.take([:status, :source, :actor_id, :predicate_id,
                       :previous_patient_id, :proposed_patient_id,
                       :previous_review, :old_patient_support,
                       :applied?, :affects_lifg?, :externally_verified?])
          |> Map.put(:correction_applied?, false)
          |> Map.put(:memory_modified?, false)
      )
    else
      _ -> si
    end
  end

  def attach(other, _), do: other

  # Nested comprehension is intentionally side-effect-free: no recruitment,
  # stimulation, persistence, self-learning, action, response or shadow write.
  # Only the returned independently revalidated semantic structures are used.
  defp read_only_comprehension(sentence, caller_opts) do
    # Preserve the caller's lexical ranking configuration so that a
    # reconstruction cannot silently change the sense-ID interpretation.
    lifg_opts =
      case Keyword.get(caller_opts, :lifg_opts, []) do
        list when is_list(list) ->
          Keyword.merge(list,
            cerebellum_learn?: false, cerebellum_snapshot_path: nil)

        _ ->
          [cerebellum_learn?: false, cerebellum_snapshot_path: nil]
      end

    opts = [
      mode: :prod,
      experience_origin: :user_turn,
      response: :off,
      episodes: false,
      persist_episodes: false,
      hippocampus_encode?: false,
      self_model: :off,
      neuron_activation?: false,
      notify_brain?: false,
      semantic_propagation?: false,
      synapse_shadow?: false,
      synapse_correction?: false,
      comprehension_wm?: false,
      focus_prompt_topics?: false,
      prefrontal_control?: false,
      execute_agency_commands?: false,
      record_agency_commands?: false,
      fuzzy: false,
      lifg_opts: lifg_opts
    ]

    case Core.resolve_input(sentence, opts) do
      %Core.SemanticInput{} = si -> {:ok, si}
      _ -> {:error, :not_interpreted}
    end
  rescue
    _ -> {:error, :not_interpreted}
  catch
    :exit, _ -> {:error, :not_interpreted}
  end
end
