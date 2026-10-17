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
  alias Core.Cognition.AssociativeReasoning

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

  # Hints returned by Hippocampus can refer either to a directly relevant
  # stored proof or to two parents linked by a shared intermediate subject.
  # Nothing below declares an answer; Consolidation revalidates the complete
  # proof graph against all relevant DB records before any result is emitted.
  defp maybe_associate(context, subjects) do
    if is_map(Map.get(context, :web_evidence)) do
      context
    else
      case AssociativeReasoning.propose(subjects) do
        %{kind: _} = hint -> Map.put(context, :hippocampal_association, hint)
        _ -> context
      end
    end
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
