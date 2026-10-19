defmodule Core.Intent.StructuralStatement do
  @moduledoc """
  Correct a low-information :unknown intent ONLY when the already-built
  sentence meaning is a revalidated, complete participant assertion.

  This is a speech-act reconciliation, not semantic sense selection.
  It never stores events, changes LIFG/ATL, or authorizes memory writes.
  """
  alias Core.Pipeline.Trace
  alias Core.Semantic.{EpisodicProjection, SentenceMeaning}

  @statement_confidence 0.68

  @spec attach(map()) :: map()
  def attach(%{intent: :unknown, sentence_meaning: meaning} = si)
      when is_map(meaning) do
    with %{kind: :participant_assertion,
           status: :structurally_interpreted,
           speech_act: :assertion,
           polarity: :positive,
           source_frame: :finite_present_svo} <- meaning,
         true <- Map.get(si, :source) in [:prod, :user, :test],
         true <- Map.get(si, :experience_origin) in [:user_turn, nil],
         %{source_frame: :finite_present_svo} <-
           EpisodicProjection.validated_participant(si),
         ^meaning <- SentenceMeaning.build(si) do
      si
      |> Map.put(:intent, :statement)
      |> Map.put(:conversation_act, :statement)
      |> Map.put(:confidence, @statement_confidence)
      |> reconcile_context()
      |> Trace.append(:structural_statement,
        decision: :statement,
        reason: :revalidated_participant_assertion,
        scores: %{confidence: @statement_confidence},
        meta: %{previous_intent: :unknown, writes_memory?: false,
                selects_verb_sense?: false})
    else
      _ -> si
    end
  end

  def attach(si), do: si

  defp reconcile_context(si) do
    context =
      si
      |> Map.get(:context_frame)
      |> case do
        frame when is_map(frame) ->
          frame
          |> Map.put(:intent, :statement)
          |> Map.put(:conversation_act, :statement)
          |> Map.put(:certainty, :moderate)

        _ ->
          %{source: :symbolic, intent: :statement,
            conversation_act: :statement, certainty: :moderate}
      end

    Map.put(si, :context_frame, context)
  end
end
