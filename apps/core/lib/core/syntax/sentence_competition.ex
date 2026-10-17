defmodule Core.Syntax.SentenceCompetition do
  @moduledoc """
  Bounded sentence-level competition between a real dictionary multiword
  reading and a complete, independently observed compositional clause.

  Sentence signatures are grammatical hypotheses; only a high-evidence
  complete frame may outrank an overlapping lexical phrase. Dictionary
  senses and original tokens are never deleted or relabeled.

  This is intentionally NOT another LIFG unigram pass. Scores here measure
  structural/lexical *compatibility*, not LIFG softmax probabilities.
  """

  alias Brain.LIFG.ContextFit
  alias Core.Pipeline.Trace

  @max_phrases 48
  @max_senses 8
  @composition_threshold 0.80
  @min_discrimination 0.16

  @spec attach(map()) :: map()
  def attach(%{} = si) do
    frames =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.filter(&complete_topic_question?/1)
      |> Enum.take(8)

    assessments =
      si
      |> Map.get(:mwe_lexicon, [])
      |> List.wrap()
      |> Enum.take(@max_phrases)
      |> Enum.flat_map(fn unit ->
        case assess(si, unit, frames) do
          nil -> []
          finding -> [finding]
        end
      end)

    rejected_spans =
      assessments
      |> Enum.filter(&(&1.winner == :compositional_clause))
      |> Enum.map(& &1.span)
      |> Enum.uniq()

    state = %{
      version: 1,
      competing_frames: length(frames),
      comparisons: assessments,
      rejected_spans: rejected_spans
    }

    si
    |> Map.put(:sentence_competition, state)
    |> Trace.append(:sentence_competition,
      decision: if(rejected_spans == [], do: :preserve_lexical, else: :composition_wins),
      reason: :bounded_sentence_vs_lexical_comparison,
      meta: state
    )
  end

  def attach(si), do: si

  @doc "Whether a lexical MWE lost against a complete sentence frame."
  @spec reject_phrase?(map(), {integer(), integer()} | term()) :: boolean()
  def reject_phrase?(si, span) when is_map(si) and is_tuple(span) do
    case Map.get(si, :sentence_competition) do
      %{rejected_spans: spans} when is_list(spans) -> span in spans
      _ -> false
    end
  end

  def reject_phrase?(_si, _span), do: false

  defp assess(si, %{span: {first, last}} = unit, frames)
       when is_integer(first) and is_integer(last) and first >= 0 and last >= first do
    case Enum.find_value(frames, fn frame ->
           case crossed_roles(frame, first, last) do
             [] -> nil
             roles -> {frame, roles}
           end
         end) do
      nil ->
        nil

      {frame, crossed} ->
        senses = unit |> Map.get(:senses, []) |> List.wrap() |> Enum.take(@max_senses)
        phrase = Map.get(unit, :phrase, "")
        sentence = Map.get(si, :sentence, "")
        fit =
          senses
          |> Enum.map(fn sense ->
            ContextFit.score(sentence, phrase, sense)
          end)
          |> Enum.max(fn -> 0.0 end)

        # Both scores are local, bounded compatibility heuristics, *not*
        # probabilities and not interchangeable with LIFG's winning score.
        composition = 0.90
        lexical = min(1.0, 0.25 + 0.60 * fit)
        margin = composition - lexical

        winner =
          if senses != [] and composition >= @composition_threshold and
               margin >= @min_discrimination,
            do: :compositional_clause,
            else: :lexical_mwe

        %{
          span: {first, last},
          phrase: phrase,
          dictionary_sense_ids: Enum.map(senses, &Map.get(&1, :id)),
          frame_id: frame.id,
          frame_span: frame.span,
          crossed_roles: crossed,
          role_boundary_conflict?: true,
          lexical_context_fit: fit,
          compositional_compatibility: composition,
          lexical_compatibility: lexical,
          discrimination_margin: margin,
          winner: winner,
          reason: if(winner == :compositional_clause,
            do: :complete_question_structure_outweighs_lexical_fit,
            else: :insufficient_structural_margin)
        }
    end
  end

  defp assess(_, _, _), do: nil

  # Preserve independently evidenced sentence roles, not just the span
  # containing the original long MWE. A phrase that fuses :auxiliary and
  # :subject, or :predicate and :preposition, competes against the *same*
  # sentence-wide reading. A phrase entirely inside the topic remains
  # eligible (e.g. "turn off" in the question's topic complement).
  #
  # Topic words beyond the explicit preposition form one logical scope;
  # they are not each separate roles. This prevents overblocking ordinary
  # noun phrases and legitimate MWEs inside the topic.
  # Optional adjuncts belong to their host grammatical role for competition.
  # "what exactly" and "really know" are allowed lexical spans; "exactly do"
  # and "know about" cross independently evidenced clause-role boundaries.
  @role_order [:wh, :wh_modifier, :auxiliary, :subject, :negation,
               :predicate_modifier, :predicate, :preposition]

  defp crossed_roles(
         %{span: {frame_start, frame_end}, roles: roles},
         first,
         last
       )
       when is_map(roles) and is_integer(frame_start) and
              is_integer(frame_end) and first >= frame_start and
              last <= frame_end and last > first do
    labels =
      first..last
      |> Enum.map(fn index ->
        case Enum.find(@role_order, &(Map.get(roles, &1) == index)) do
          :wh_modifier -> :wh
          :predicate_modifier -> :predicate
          :negation -> :predicate
          nil ->
            if index > Map.get(roles, :preposition, frame_end),
              do: :topic_complement,
              else: nil
          role -> role
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    if length(labels) >= 2, do: labels, else: []
  end

  defp crossed_roles(_, _, _), do: []

  defp complete_topic_question?(%{
         id: :wh_aux_topic_question,
         roles: %{wh: wh, auxiliary: aux, subject: subject,
                  predicate: predicate, preposition: prep} = roles,
         evidence: evidence
       })
       when is_integer(wh) and is_integer(aux) and is_integer(subject) and
              is_integer(predicate) and is_integer(prep) and is_list(evidence) do
    wh == 0 and
      filled_gap?(roles, wh, aux, [:wh_modifier]) and
      subject == aux + 1 and
      filled_gap?(roles, subject, predicate, [:negation, :predicate_modifier]) and
      prep == predicate + 1 and
      Enum.all?([:lexical_verb_available, :explicit_prepositional_topic,
                  :question_punctuation], &(&1 in evidence))
  end

  defp complete_topic_question?(_), do: false

  # Every intervening position must be accounted for by an observed optional
  # role. No arbitrary skips, duplicate indices, or reordered core positions.
  defp filled_gap?(roles, first, last, optional_roles)
       when is_integer(first) and is_integer(last) and last > first do
    intervening = if last == first + 1, do: [], else: Enum.to_list((first + 1)..(last - 1))

    observed =
      optional_roles
      |> Enum.map(&Map.get(roles, &1))
      |> Enum.reject(&is_nil/1)
      |> Enum.sort()

    observed == intervening
  end

  defp filled_gap?(_, _, _, _), do: false
end
