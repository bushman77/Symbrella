defmodule Core.Cognition.MemoryBoundary do
  @moduledoc """
  Core-side memory invocation and response privacy boundary.

  The hippocampus owns episodic cueing; the consolidated semantic store owns
  durable source-supported conclusions. Core verifies every recalled premise.
  This module is the single gateway that response generation uses for
  question-triggered semantic recall. A future Hippocampus association hook
  can invoke the same API without putting replay/curiosity output into chat.

  Internal cognitive objects remain in Core's private context for reasoning
  and diagnostics. They must not become model instructions, public response
  metadata, or spontaneous conversational content.
  """

  alias Core.Response.{Consolidation, PopulationComparison}
  alias Brain.Hippocampus.SemanticAssociations
  alias Db.ConsolidatedConclusion

  @private_keys [
    :consolidation_issue,
    :composition,
    :hippocampal_association,
    :reasoning_trace,
    :hippocampal_replay,
    :curiosity_investigation
  ]

  @doc "Recall only when the current interpreted question has a relevant relation."
  @spec recall(map()) :: map()
  def recall(context) when is_map(context) do
    case PopulationComparison.comparison_subjects(context) do
      [first, second] = subjects when is_binary(first) and is_binary(second) ->
        context
        |> maybe_associate(subjects)
        |> Consolidation.recall()

      _ -> context
    end
  end

  def recall(other), do: other

  # Hippocampus merely proposes an opaque pointer. A matching DB record can
  # establish a cue association, NOT the truth of the claim. Consolidation
  # remains responsible for all independent source/proof revalidation.
  defp maybe_associate(context, subjects) do
    if Application.get_env(:core, :semantic_associative_recall_enabled?, false) and
         not is_map(Map.get(context, :web_evidence)) do
      subject_key = pair_key(subjects)

      pointer =
        subjects
        |> SemanticAssociations.cues(8)
        |> Enum.find(fn fingerprint ->
          case Db.get_by(ConsolidatedConclusion, fingerprint: fingerprint) do
            %ConsolidatedConclusion{kind: "population_comparison",
                                     provenance: "source_supported_symbolic_derivation",
                                     pair_key: ^subject_key} ->
              true

            _ -> false
          end
        end)

      if is_binary(pointer) do
        Map.put(context, :hippocampal_association, %{
          kind: :semantic_proof_pointer, fingerprint: pointer
        })
      else
        context
      end
    else
      context
    end
  rescue
    _ -> context
  catch
    :exit, _ -> context
  end

  defp pair_key(names) do
    names
    |> Enum.map(&(String.downcase(String.trim(&1))))
    |> Enum.sort()
    |> Enum.join("|")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end

  @doc """
  Strip cognitive-workflow records from prompt-facing context. Supported,
  authorized source evidence stays available for answering the user's question.
  """
  @spec for_expression(map()) :: map()
  def for_expression(context) when is_map(context) do
    context
    |> Map.drop(@private_keys)
    |> Map.update(:features, %{}, fn
      %{} = features -> Map.drop(features, @private_keys)
      other -> other
    end)
  end

  def for_expression(other), do: other

  @doc """
  A relevant but unresolved population comparison gets a neutral Core
  response without invoking Qwen, repeating investigation traces, or
  broadcasting curiosity. This is not a generic refusal for unrelated turns.
  """
  @spec unresolved_answer(map()) :: String.t() | nil
  def unresolved_answer(%{consolidation_issue: %{status: status}} = context)
      when status in [:open, :investigating] do
    # The caller invokes this only AFTER the existing authoritative answer
    # planner has declined to certify an answer. Even a partial source group
    # with :source_supported resolution is not proof of comparability.
    case PopulationComparison.comparison_subjects(context) do
      [_first, _second] ->
        "I don't have sufficient source-supported evidence to make that " <>
          "population comparison reliably."

      _ ->
        nil
    end
  end

  def unresolved_answer(_), do: nil
end
