defmodule Brain.LIFG.SemanticControl do
  @moduledoc """
  Evidence-gated semantic control for LIFG Stage1.

  ContextFit already compares the *original sentence* with dictionary sense
  metadata. This module adds only a small discriminating signal when one
  candidate meaning has demonstrably better independent lexical context than
  competing meanings of the same token and POS.

  It does not inspect downstream ATL/propositions, assert facts, create senses,
  change POS, or recalibrate probabilities. Ambiguity remains ambiguity when
  context fit is uniform or insufficient.
  """

  @default_max_delta 0.10
  @default_min_fit 0.18
  @default_min_gap 0.15

  @spec apply(map(), [map()], keyword()) :: {map(), map()}
  def apply(scores, rows, opts \\ [])

  def apply(scores, rows, opts)
      when is_map(scores) and is_list(rows) and is_list(opts) do
    enabled? = Keyword.get(opts, :enabled?, true)
    max_delta = bounded(opts, :max_delta, @default_max_delta)
    min_fit = bounded(opts, :min_fit, @default_min_fit)
    min_gap = bounded(opts, :min_gap, @default_min_gap)
    focus_pos =
      case Keyword.get(opts, :focus_pos) do
        value when is_binary(value) -> value
        _ -> nil
      end

    if not enabled? or max_delta == 0.0 do
      {scores, %{applied?: false, reason: :disabled, adjustments: []}}
    else
      eligible =
        Enum.filter(rows, fn
          %{id: id, context_evidence: fit}
          when is_binary(id) and is_number(fit) ->
            Map.has_key?(scores, id) and fit >= 0.0 and fit <= 1.0 and
              not String.ends_with?(id, "|fallback") and valid_sense_id?(id) and
              (is_nil(focus_pos) or sense_pos(id) == focus_pos)
          _ -> false
        end)

      by_word_and_pos =
        Enum.group_by(eligible, fn %{id: id} ->
          [word, pos, _number] = String.split(id, "|", parts: 3)
          {word, pos}
        end)

      adjustments =
        by_word_and_pos
        |> Enum.sort_by(fn {key, _} -> key end)
        |> Enum.flat_map(fn {_key, group} ->
          discriminating_adjustment(group, max_delta, min_fit, min_gap)
        end)

      updated =
        Enum.reduce(adjustments, scores, fn %{id: id, delta: delta}, acc ->
          Map.update!(acc, id, &min(1.0, max(0.0, &1 + delta)))
        end)

      {updated, %{
        applied?: adjustments != [],
        reason: if(adjustments == [], do: :insufficient_discriminating_evidence,
          else: :contextual_sense_discrimination),
        adjustments: adjustments
      }}
    end
  end

  def apply(scores, _rows, _opts),
    do: {scores, %{applied?: false, reason: :invalid_input, adjustments: []}}

  defp discriminating_adjustment(group, max_delta, min_fit, min_gap)
       when length(group) >= 2 do
    ordered =
      Enum.sort_by(group, fn %{id: id, context_evidence: fit} ->
        {-fit, id}
      end)

    [best, runner_up | _] = ordered
    gap = best.context_evidence - runner_up.context_evidence

    if best.context_evidence >= min_fit and gap >= min_gap and gap > 0.0 do
      # A logit-scale adjustment, not an invented probability. It is intentionally
      # capped; downstream Stage1 still uses its original softmax and uncertainty.
      delta = min(max_delta, gap * max_delta)

      if delta > 0.0 do
        [%{
          id: best.id,
          delta: Float.round(delta, 6),
          context_fit: best.context_evidence,
          next_fit: runner_up.context_evidence,
          evidence_gap: Float.round(gap, 6),
          source: :original_sentence_dictionary_context
        }]
      else
        []
      end
    else
      []
    end
  end

  defp discriminating_adjustment(_, _, _, _), do: []

  defp sense_pos(id) do
    case String.split(id, "|", parts: 3) do
      [_word, pos, _sense] -> pos
      _ -> nil
    end
  end

  defp valid_sense_id?(id) do
    case String.split(id, "|", parts: 3) do
      [word, pos, sense] -> word != "" and pos != "" and sense != ""
      _ -> false
    end
  end

  defp bounded(opts, key, default) do
    case Keyword.get(opts, key, default) do
      n when is_number(n) -> min(1.0, max(0.0, n * 1.0))
      _ -> default
    end
  end
end
