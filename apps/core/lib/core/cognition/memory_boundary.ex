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

  @private_keys [
    :consolidation_issue,
    :composition,
    :reasoning_trace,
    :hippocampal_replay,
    :curiosity_investigation
  ]

  @doc "Recall only when the current interpreted question has a relevant relation."
  @spec recall(map()) :: map()
  def recall(context) when is_map(context) do
    case PopulationComparison.comparison_subjects(context) do
      [_first, _second] -> Consolidation.recall(context)
      _ -> context
    end
  end

  def recall(other), do: other

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
  def unresolved_answer(%{consolidation_issue: %{status: :open}} = context) do
    case {PopulationComparison.comparison_subjects(context),
          Map.get(context, :web_evidence)} do
      {[_first, _second], %{resolution: :source_supported}} ->
        # If new retrieval supplied supported evidence, let the normal
        # authoritative gate evaluate it instead of relying on old issues.
        nil

      {[_first, _second], _} ->
        "I don't have sufficient source-supported evidence to make that " <>
          "population comparison reliably."

      _ ->
        nil
    end
  end

  def unresolved_answer(_), do: nil
end
