defmodule Brain.LIFG.PrepositionSemantics do
  @moduledoc """
  Evidence-gated preposition *sense compatibility* for a physical-location PP.

  A PP signature only proves that a preposition has a nominal complement; it
  does not establish whether the relation denotes location or topic. A small
  spatial-sense bonus therefore additionally requires two independent, attested
  observations:

    * the immediately governing predicate has a dictionary verb gloss about
      physical posture or position; and
    * a noun sense of the PP object has a location-related gloss with contextual
      support from the actual sentence.

  All evidence comes from existing Phase 1 tokens, Core syntax, and retrieved
  BrainCell senses. A dictionary definition licenses compatibility, never a
  verified event, position, referent or a forced word-sense winner.

  A topic PP, incomplete signature, missing gloss, or uncorroborated object
  causes abstention. Scores retain exactly the same candidate IDs.
  """

  alias Brain.LIFG.ContextFit

  @default_bonus 0.12
  @max_cells 512

  # These are *dictionary gloss* cues, never hardcoded source-word mappings.
  # Require direct predicate adjacency to the PP and an independently
  # corroborated physical-location object. "sat on the committee" will not
  # qualify merely because a predicate involves sitting.
  @posture_gloss ~r/\b(?:sit|sitting|seated|seat|rest|resting|stand|standing|lie|lying|recline|perch|perching|land|landing|positioned|posture)\b/ui
  @place_gloss ~r/\b(?:river|watercourse|lake|shore|coast|bank|edge|surface|ground|land|terrain|location|place|road|building|floor|chair|table|seat|bed)\b/ui
  @spatial_gloss ~r/\b(?:position|location|surface|spatial|adjacent|alongside|touching|resting|contact)\b/ui

  @spec adjust(map(), [map()], map(), non_neg_integer(), keyword()) :: {map(), map()}
  def adjust(scores, candidates, si, index, opts \\ [])

  def adjust(scores, candidates, %{} = si, index, opts)
      when is_map(scores) and is_list(candidates) and is_integer(index) and
           index >= 0 and is_list(opts) do
    bonus = Keyword.get(opts, :bonus, @default_bonus)
    enabled? = Keyword.get(opts, :enabled?, true) == true
    cells = Map.get(si, :active_cells, [])
    cells = if is_list(cells), do: cells, else: []

    cond do
      not enabled? -> {scores, abstain(:disabled)}
      not is_number(bonus) or bonus <= 0 -> {scores, abstain(:disabled)}
      length(cells) > @max_cells -> {scores, abstain(:large_candidate_slate)}
      true ->
        run(scores, candidates, si, index, cells, min(bonus * 1.0, 0.18))
    end
  end

  def adjust(scores, _candidates, _si, _index, _opts),
    do: {scores, abstain(:invalid_input)}

  defp run(scores, candidates, si, index, cells, bonus) do
    tokens =
      si
      |> Map.get(:tokens, [])
      |> case do
        ts when is_list(ts) -> ts
        _ -> []
      end
      |> Enum.filter(&is_map/1)
      |> Map.new(fn token -> {Map.get(token, :index), normalize(Map.get(token, :phrase))} end)

    target = Map.get(tokens, index)

    signatures =
      case Map.get(si, :sentence_signatures) do
        sigs when is_list(sigs) -> sigs
        _ -> []
      end

    # Syntax intentionally retains competing constructions. A bare PP
    # ("on the" when "the" has a dictionary noun sense) may precede the
    # complete PP ("on the bank") in the signature list. Prefer a fully
    # witnessed composed noun phrase, not the first hypothesis.
    pp =
      signatures
      |> Enum.filter(&valid_pp?(&1, index, tokens))
      |> Enum.sort_by(fn %{span: {first, last}, evidence: evidence} ->
        {if(:noun_phrase_complement in evidence, do: 0, else: 1),
         -(last - first), last}
      end)
      |> List.first()

    cond do
      is_nil(pp) or not is_binary(target) ->
        {scores, abstain(:missing_attested_prepositional_phrase)}

      not is_binary(Map.get(si, :sentence)) ->
        {scores, abstain(:missing_sentence)}

      true ->
        object_index = pp.roles.object
        predicate_index = index - 1

        predicate_word = Map.get(tokens, predicate_index)
        object_word = Map.get(tokens, object_index)

        predicate =
          Enum.find(cells, fn cell ->
            same_word_and_pos?(cell, predicate_index, predicate_word, "verb") and
              gloss_match?(field(cell, :definition), @posture_gloss)
          end)

        object =
          Enum.find(cells, fn cell ->
            same_word_and_pos?(cell, object_index, object_word, "noun") and
              gloss_match?(field(cell, :definition), @place_gloss) and
              ContextFit.score(si.sentence, object_word, cell) >= 0.18
          end)

        cond do
          is_nil(predicate) -> {scores, abstain(:missing_posture_predicate_witness)}
          is_nil(object) ->
            diagnostic = object_witness_diagnostic(cells, object_index, object_word, si.sentence)
            {scores,
             Map.merge(abstain(:missing_contextual_location_object_witness), diagnostic)}
          true ->
            adjustments =
              candidates
              |> Enum.filter(fn cand ->
                id = field(cand, :id)
                is_binary(id) and Map.has_key?(scores, id) and
                  same_word_and_pos?(cand, index, target, "preposition", false) and
                  gloss_match?(field(cand, :definition), @spatial_gloss)
              end)
              |> Enum.uniq_by(&field(&1, :id))
              |> Enum.map(fn cand ->
                %{id: field(cand, :id), delta: bonus,
                  source: :attested_spatial_pp_and_dictionary_gloss}
              end)

            updated =
              Enum.reduce(adjustments, scores, fn %{id: id, delta: delta}, acc ->
                Map.update!(acc, id, &(&1 + delta))
              end)

            report = %{
              applied?: adjustments != [],
              reason: if(adjustments == [], do: :no_spatial_preposition_gloss,
                else: :supported_spatial_prepositional_compatibility),
              predicate_index: predicate_index,
              object_index: object_index,
              predicate_witness: field(predicate, :id),
              object_witness: field(object, :id),
              adjustments: adjustments,
              semantic_accuracy: :not_measured
            }
            {updated, report}
        end
    end
  end

  # Validate the original token roles and a complete PP shape before
  # comparing dictionary meanings. An attested noun-phrase complement must
  # name its own head as the object. Bare complements remain possible when no
  # complete phrase is available, but cannot shadow a stronger structure.
  defp valid_pp?(
         %{id: :prepositional_phrase, source: :core_syntax,
           span: {first, last}, roles: roles, evidence: evidence},
         index, tokens)
       when is_integer(first) and is_integer(last) and is_map(roles) and
            is_list(evidence) do
    object = Map.get(roles, :object)

    first == index and
      is_integer(object) and object > index and object <= last and
      :preposition in evidence and is_binary(Map.get(tokens, index)) and
      case evidence do
        ev when :noun_phrase_complement in ev ->
          Map.get(roles, :head) == object and
            object == last and
            last >= index + 2 and
            Enum.all?(index..last, &is_binary(Map.get(tokens, &1)))

        ev when :bare_nominal_complement in ev ->
          object == index + 1 and last == object and
            is_binary(Map.get(tokens, object))

        _ ->
          false
      end
  end

  defp valid_pp?(_, _, _), do: false

  # Diagnostic only: report why a location-complement witness was unavailable.
  # No synthetic senses, fallback guesses, or relaxed thresholds.
  defp object_witness_diagnostic(cells, index, word, sentence) do
    nouns =
      Enum.filter(cells, &same_word_and_pos?(&1, index, word, "noun"))

    place_readings =
      nouns
      |> Enum.filter(&gloss_match?(field(&1, :definition), @place_gloss))
      |> Enum.map(fn cell ->
        %{id: field(cell, :id),
          context_fit: ContextFit.score(sentence, word, cell)}
      end)

    best =
      Enum.max_by(place_readings, & &1.context_fit, fn -> nil end)

    %{
      object_index: index,
      object_word: word,
      matching_noun_candidates: length(nouns),
      place_gloss_candidates: length(place_readings),
      best_place_sense: if(is_map(best), do: best.id, else: nil),
      best_place_context_fit: if(is_map(best), do: best.context_fit, else: nil),
      required_context_fit: 0.18
    }
  end

  defp abstain(reason),
    do: %{applied?: false, reason: reason, adjustments: [],
      semantic_accuracy: :not_measured}

  defp same_word_and_pos?(cell, index, word, pos, indexed? \\ true)
  defp same_word_and_pos?(cell, index, word, pos, indexed?)
       when is_map(cell) and is_binary(word) and word != "" do
    (not indexed? or field(cell, :token_index) == index) and
      normalize(field(cell, :norm) || field(cell, :lemma) ||
        field(cell, :word)) == word and
      normalize(field(cell, :pos)) == pos and
      is_binary(field(cell, :id))
  end

  defp same_word_and_pos?(_, _, _, _, _), do: false

  defp gloss_match?(text, re) when is_binary(text), do: Regex.match?(re, text)
  defp gloss_match?(_, _), do: false

  defp normalize(text) when is_binary(text),
    do: text |> String.trim() |> String.downcase()
  defp normalize(_), do: ""

  defp field(map, key) when is_map(map),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
end
