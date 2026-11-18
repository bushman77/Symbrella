defmodule Core.Integration.Coverage do
  @moduledoc """
  Read-only coverage reporting for canonical semantic input.

  Reports sentence-level structural interpretation separately from
  whole-message comprehension and factual authority.
  """

  alias Core.Semantic.Passage

  @version 2

  @spec assess(map()) :: map()
  def assess(%{} = si) do
    with {:ok, projection} <- Passage.project(si),
         {:ok, observed} <- observe(si, projection.segments) do
      segments = projection.segments
      count = length(segments)

      whole_meaning =
        case Map.get(si, :sentence_meaning) do
          %{kind: kind, status: status} ->
            %{kind: kind, status: status}

          _ ->
            %{kind: :unknown, status: :not_observed}
        end

      %{
        version: @version,
        status: coverage_status(count, whole_meaning, observed),
        sentence_count: count,
        question_count:
          Enum.count(
            segments,
            &(&1.terminator == :question_mark)
          ),
        source_alignment: :validated,
        whole_input_semantics: whole_meaning,
        per_sentence_semantics: observed,
        discourse_resolution: :not_evaluated,
        question_answer_binding: :not_evaluated,
        factual_authority?: false
      }
    else
      {:error, reason} ->
        unavailable(reason)
    end
  end

  def assess(_), do: unavailable(:invalid_input)

  defp observe(si, segments) do
    if length(segments) > 1 and lexical_evidence?(si) do
      case Passage.analyze(si) do
        {:ok, analysis} ->
          readings =
            analysis.segments
            |> Enum.with_index(1)
            |> Enum.map(fn {segment, index} ->
              meaning = segment.sentence_meaning

              %{
                index: index,
                text: segment.text,
                source_span: segment.source_span,
                source_word_range: segment.source_word_range,
                kind: Map.get(meaning, :kind, :unknown),
                status:
                  Map.get(
                    meaning,
                    :status,
                    :not_yet_interpreted
                  ),
                source_frame: Map.get(meaning, :source_frame),
                unresolved: Map.get(meaning, :unresolved, []),
                factual_authority?: false
              }
            end)

          provisional =
            Enum.count(readings, fn reading ->
              reading.kind == :structural_hypothesis and
                reading.status == :provisional
            end)

          {:ok,
           %{
             status: :read_only_observed,
             provisional_count: provisional,
             unresolved_count: length(readings) - provisional,
             segments: readings
           }}

        {:error, reason} ->
          {:error, reason}
      end
    else
      {:ok, :not_run}
    end
  end

  defp lexical_evidence?(si) do
    choices = Map.get(si, :lifg_choices)

    winners =
      case Map.get(si, :atl_slate) do
        %{winners: rows} when is_list(rows) -> rows
        _ -> []
      end

    is_list(choices) and choices != [] and winners != []
  end

  defp coverage_status(count, whole, observed) do
    cond do
      count > 1 and is_map(observed) and
          observed.provisional_count == count ->
        :all_sentences_provisionally_interpreted

      count > 1 and is_map(observed) and
          observed.provisional_count > 0 ->
        :partially_interpreted

      count > 1 ->
        :segmented_only

      whole.status == :not_observed ->
        :not_analyzed

      whole.status == :not_yet_interpreted ->
        :uninterpreted

      true ->
        :whole_input_interpretation_observed
    end
  end

  defp unavailable(reason) do
    %{
      version: @version,
      status: :unavailable,
      reason: reason,
      factual_authority?: false
    }
  end
end
