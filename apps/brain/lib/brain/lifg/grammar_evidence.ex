defmodule Brain.LIFG.GrammarEvidence do
  @moduledoc """
  Candidate-specific compatibility between Core sentence signatures and
  dictionary sense metadata. Pure: does not select winners or mutate input.

  `apply/4` is opt-in. A weight of zero returns scores unchanged.
  `:have_verb_to_infinitive` alone does not imply obligation; obligation
  evidence must also be present on the *specific dictionary sense*.
  """

  @type context :: %{
          required(:signatures) => [map()],
          required(:cells_by_id) => map(),
          optional(:resolved_roles) => map()
        }

  @spec prepare(map()) :: context()
  def prepare(%{} = si) do
    cells =
      si
      |> get(:active_cells, [])
      |> as_list()
      |> Enum.reduce(%{}, fn cell, acc ->
        case get(cell, :id, nil) do
          id when is_binary(id) -> Map.put(acc, id, cell)
          _ -> acc
        end
      end)

    role_competition = get(si, :role_competition, %{})

    roles =
      if is_map(role_competition) and
           get(role_competition, :source, nil) == :core_syntax_and_attested_dictionary_pos do
        get(role_competition, :roles, %{})
      else
        %{}
      end

    %{
      signatures: si |> get(:sentence_signatures, []) |> as_list(),
      cells_by_id: cells,
      resolved_roles: if(is_map(roles), do: roles, else: %{})
    }
  end

  def prepare(_), do: %{signatures: [], cells_by_id: %{}, resolved_roles: %{}}

  @doc """
  Apply bounded grammatical support to an existing candidate score map.
  The caller decides the scale: 0.0 disables; 1.0 uses the full deltas.
  Does not remove or create candidates.
  """
  @spec apply(map(), context(), non_neg_integer(), number()) :: map()
  def apply(scores, _context, _index, weight)
      when is_map(scores) and is_number(weight) and weight <= 0,
      do: scores

  def apply(scores, %{signatures: signatures, cells_by_id: cells}, index, weight)
      when is_map(scores) and is_integer(index) and index >= 0 and is_number(weight) do
    weight = min(weight * 1.0, 1.0)

    Map.new(scores, fn {id, base} ->
      support =
        signatures
        |> Enum.map(&support_for(&1, index, id, Map.get(cells, id)))
        |> Enum.max(fn -> 0.0 end)

      updated =
        if is_number(base) do
          min(1.0, max(0.0, base * 1.0 + support * weight))
        else
          base
        end

      {id, updated}
    end)
  end

  def apply(scores, _context, _index, _weight), do: scores

  @doc """
  Add bounded POS support only where independent grammatical evidence has
  adjudicated conflicting candidate parses. No candidate is removed, no sense
  gets lexical/semantic verification, and the original Stage1 alternatives
  remain available for auditing. This applies even at grammar_weight: 0,
  because it is stronger evidence than the general speculative signature
  support governed by that knob.
  """
  @spec apply_role_competition(map(), context(), non_neg_integer()) :: map()
  def apply_role_competition(scores, %{resolved_roles: roles}, index)
      when is_map(scores) and is_map(roles) and is_integer(index) and index >= 0 do
    case Map.get(roles, index) do
      %{status: :supported, pos: pos, basis: basis, evidence: evidence}
      when pos in ["noun", "adjective", "verb"] and
           basis in [:complete_determiner_adjective_noun,
                     :determined_adjective_before_unresolved_head,
                     :determined_subject_before_attested_past_predicate,
                     :determined_subject_before_attested_present_predicate,
                     :determined_subject_in_wh_aux_question,
                     :predicate_in_wh_aux_question,
                     :modal_question_predicate, :modal_declarative_predicate,
                     :imperative_predicate, :plural_present_predicate,
                     :finite_present_svo_predicate] and
           is_list(evidence) ->
        expected =
          case basis do
            :complete_determiner_adjective_noun ->
              [:bounded_determiner_surface, :dictionary_pos_witness,
               :complete_nominal_constituent]

            :determined_adjective_before_unresolved_head ->
              [:bounded_determiner_surface, :dictionary_pos_witness,
               :unresolved_code_head, :complete_nominal_surface]

            :determined_subject_before_attested_past_predicate ->
              [:bounded_determiner_surface, :dictionary_pos_witness,
               :attested_simple_past_predicate]

            :determined_subject_before_attested_present_predicate ->
              [:complete_finite_present_svo, :dictionary_pos_witness,
               :attested_finite_present_verb]

            :determined_subject_in_wh_aux_question ->
              [:wh_fronted, :auxiliary_inversion, :bounded_determiner_surface,
               :dictionary_pos_witness, :question_punctuation]

            :predicate_in_wh_aux_question ->
              [:wh_fronted, :auxiliary_inversion, :bounded_determiner_surface,
               :dictionary_pos_witness, :question_punctuation,
               :attested_verb_alternative]

            :modal_question_predicate ->
              [:auxiliary_subject_order, :dictionary_pos_witness,
               :bounded_predicate_slot]

            :modal_declarative_predicate ->
              [:subject_auxiliary_order, :dictionary_pos_witness,
               :bounded_predicate_slot]

            :imperative_predicate ->
              [:initial_predicate, :dictionary_pos_witness,
               :bounded_predicate_slot]

            :plural_present_predicate ->
              [:attested_plural_subject, :present_base_verb_witness,
               :dictionary_pos_witness, :bounded_predicate_slot]

            :finite_present_svo_predicate ->
              [:attested_finite_present_verb, :dictionary_pos_witness,
               :bounded_predicate_slot]
          end

        if Enum.all?(expected, &(&1 in evidence)) do
          Map.new(scores, fn
            {id, score} when is_binary(id) and is_number(score) ->
              boosted? = id_pos(id) == pos and not String.ends_with?(id, "|fallback")
              {id, if(boosted?, do: min(1.0, max(0.0, score + 0.30)), else: score)}

            entry ->
              entry
          end)
        else
          scores
        end

      _ ->
        scores
    end
  end

  def apply_role_competition(scores, _, _), do: scores

  @doc """
  Narrow, on-by-default support for an attested finite-present SVO hypothesis.
  Unlike the general opt-in grammar weights, this can only boost an existing
  exact verb sense carrying the full morphological witness. All other
  candidates and sentences retain their original scores.
  """
  @spec apply_finite_present(map(), context(), non_neg_integer()) :: map()
  def apply_finite_present(scores, %{signatures: signatures, cells_by_id: cells} = context, index)
      when is_map(scores) and is_list(signatures) and is_map(cells) and
             is_integer(index) and index >= 0 do
    # A supported Core predicate role already receives the shared 0.30 POS
    # adjustment; do not stack the legacy 0.32 on the same evidence.
    already_supported? =
      match?(%{status: :supported, basis: :finite_present_svo_predicate},
        get_in(context, [:resolved_roles, index]))

    observed? =
      Enum.any?(signatures, fn
        %{id: :finite_present_svo_candidate,
          roles: %{subject_head: subject, predicate: ^index, object_head: object},
          evidence: evidence}
        when is_integer(subject) and is_integer(object) and is_list(evidence) ->
          subject < index and index < object and
            Enum.all?(
              [:contiguous_surface, :attested_nominal_subject,
               :attested_nominal_object, :attested_finite_present_verb],
              &(&1 in evidence)
            )

        _ ->
          false
      end)

    if observed? and not already_supported? do
      Map.new(scores, fn {id, score} ->
        cell = Map.get(cells, id)
        updated =
          if is_binary(id) and not String.ends_with?(id, "|fallback") and
               id_pos(id) == "verb" and finite_present_sense?(cell) and
               is_number(score) do
            min(1.0, max(0.0, score * 1.0 + 0.32))
          else
            score
          end

        {id, updated}
      end)
    else
      scores
    end
  end

  def apply_finite_present(scores, _context, _index), do: scores

  @doc """
  Give existing dictionary noun candidates bounded support when a complete
  subject–copula–noun–relative-verb–determined-object construction is
  independently attested by Core syntax. This is a POS compatibility signal,
  not a selection of a specific word sense or an asserted proposition.

  In particular, "were lines that refute the claim" can prefer the common
  noun "lines" over an unrelated proper-name reading, while an isolated
  "Lines" or an incomplete/negated construction remains unaffected.
  """
  @spec apply_copular_relative(map(), context(), non_neg_integer()) :: map()
  def apply_copular_relative(
        scores,
        %{signatures: signatures, cells_by_id: cells},
        index
      )
      when is_map(scores) and is_list(signatures) and is_map(cells) and
             is_integer(index) and index >= 0 do
    supported? =
      Enum.any?(signatures, fn
        %{
          id: :copular_relative_classification,
          source: :core_syntax,
          span: {first, last},
          roles: %{
            subject_head: subject, copula: copula,
            complement_head: ^index, relative_marker: marker,
            relative_verb: verb, object_determiner: det, object_head: object
          },
          evidence: evidence
        } when is_list(evidence) ->
          first == subject and last == object and
            [copula, index, marker, verb, det, object] ==
              Enum.to_list((subject + 1)..(subject + 6)) and
            Enum.all?(
              [
                :contiguous_surface, :attested_subject_noun,
                :attested_copular_be, :attested_complement_noun,
                :explicit_relative_clause, :attested_relative_verb,
                :attested_object_noun
              ],
              &(&1 in evidence)
            )

        _ ->
          false
      end)

    if supported? do
      Map.new(scores, fn
        {id, score} when is_binary(id) and is_number(score) ->
          cell = Map.get(cells, id)

          if id_pos(id) == "noun" and
               not String.ends_with?(id, "|fallback") and
               is_map(cell) and get(cell, :pos, nil) == "noun" do
            {id, min(1.0, max(0.0, score + 0.36))}
          else
            {id, score}
          end

        entry ->
          entry
      end)
    else
      scores
    end
  end

  def apply_copular_relative(scores, _context, _index), do: scores

  @doc """
  Give a dictionary-attested verb POS bounded support in a complete
  regular-past D-N-V-D-N clause. This is pre-winner grammar evidence, not a
  selected dictionary sense, morphological tag or verified assertion.
  """
  @spec apply_bounded_past_transitive(map(), context(), non_neg_integer()) :: map()
  def apply_bounded_past_transitive(
        scores, %{signatures: signatures, cells_by_id: cells}, index
      )
      when is_map(scores) and is_list(signatures) and is_map(cells) and
             is_integer(index) and index >= 0 do
    witnessed? =
      Enum.any?(signatures, fn
        %{id: :bounded_regular_past_transitive, source: :core_syntax,
          span: {first, last},
          roles: %{subject_determiner: det, subject_head: subject,
                   predicate: ^index, object_determiner: object_det,
                   object_head: object},
          evidence: evidence} when is_list(evidence) ->
          first == det and last == object and
            [subject, index, object_det, object] ==
              Enum.to_list((det + 1)..(det + 4)) and
            Enum.all?(
              [:contiguous_surface, :attested_nominal_subject,
               :attested_dictionary_verb, :bounded_past_surface,
               :attested_nominal_object],
              &(&1 in evidence)
            )

        _ -> false
      end)

    if witnessed? do
      Map.new(scores, fn
        {id, score} when is_binary(id) and is_number(score) ->
          cell = Map.get(cells, id)

          if id_pos(id) == "verb" and not String.ends_with?(id, "|fallback") and
               is_map(cell) and get(cell, :pos, nil) == "verb" do
            {id, min(1.0, max(0.0, score + 0.40))}
          else
            {id, score}
          end

        entry -> entry
      end)
    else
      scores
    end
  end

  def apply_bounded_past_transitive(scores, _context, _index), do: scores

  # A weak construction hypothesis becomes useful only when an exact
  # sense supplies the matching morphological feature.
  defp support_for(%{id: :finite_past_candidate, roles: %{verb: role_index}}, index, _id, cell)
       when role_index == index do
    if simple_past?(cell), do: 0.28, else: 0.0
  end

  defp support_for(%{id: :perfect_predicate, roles: %{verb: role_index}}, index, _id, cell)
       when role_index == index do
    if past_participle?(cell), do: 0.24, else: 0.0
  end

  # This frame is broad (e.g. "have tried to leave"). It is NOT sufficient
  # by itself to assert an obligation meaning.
  defp support_for(%{id: :have_verb_to_infinitive, roles: %{verb: role_index}}, index, _id, cell)
       when role_index == index do
    if obligation_sense?(cell), do: 0.38, else: 0.0
  end

  defp support_for(
         %{id: :have_verb_to_infinitive, roles: %{complement: role_index}},
         index,
         id,
         _cell
       )
       when role_index == index do
    if id_pos(id) == "verb", do: 0.65, else: 0.0
  end

  defp support_for(
         %{id: :have_verb_to_infinitive, roles: %{infinitive_marker: role_index}},
         index,
         id,
         _cell
       )
       when role_index == index do
    if id_pos(id) == "particle", do: 0.65, else: 0.0
  end

  # A complete WH + auxiliary + subject + predicate + topic-preposition
  # question is stronger syntactic evidence than an overlapping two-token
  # noun-phrase hypothesis. In "what do you know about ...", a noun reading
  # for "know" may otherwise win because a local "you know" noun phrase
  # receives grammatical support while the complete question receives none.
  # Reward *existing* verb candidates only; this never chooses a dictionary
  # sense or invents a candidate.
  defp support_for(
         %{id: :wh_aux_topic_question,
           roles: %{wh: wh, auxiliary: aux, subject: subject,
                    predicate: predicate, preposition: prep},
           required_features: %{predicate: required},
           evidence: evidence},
         index,
         id,
         _cell
       )
       when predicate == index and is_binary(id) and is_list(evidence) and
              is_integer(wh) and is_integer(aux) and is_integer(subject) and
              is_integer(predicate) and is_integer(prep) do
    complete? =
      wh < aux and aux < subject and subject < predicate and
        predicate < prep and
        Enum.all?(
          [:wh_fronted, :auxiliary_inversion, :lexical_verb_available,
           :explicit_prepositional_topic, :question_punctuation],
          &(&1 in evidence)
        )

    if complete? and not String.ends_with?(id, "|fallback") and
         Enum.any?(List.wrap(required), &matches_pos?(&1, id_pos(id))) do
      0.38
    else
      0.0
    end
  end

  # Structural compatibility is a small *positive* signal for existing
  # lexical candidates. POS features come from Core.Syntax.Signatures; this
  # pass never manufactures IDs, removes alternatives, or replaces Stage1's
  # existing ClosedClass/PosBias/ContextFit scoring.
  @constituent_ids [:verb_particle_construction, :noun_phrase, :prepositional_phrase]

  defp support_for(
         %{id: construction, roles: roles, required_features: features},
         index,
         id,
         _cell
       )
       when construction in @constituent_ids and is_map(roles) and
              is_map(features) and is_binary(id) do
    candidate_pos = id_pos(id)

    if String.ends_with?(id, "|fallback") do
      0.0
    else
      roles
      |> Enum.flat_map(fn {role, role_index} ->
        required = Map.get(features, role, [])

        if role_index == index and
             Enum.any?(List.wrap(required), &matches_pos?(&1, candidate_pos)) do
          [role_support(construction, role)]
        else
          []
        end
      end)
      |> Enum.max(fn -> 0.0 end)
    end
  end

  defp support_for(_, _, _, _), do: 0.0

  # Stronger than isolated noun/adjective windows at the same token; the
  # separated construction requires a compatible verb and intervening object.
  defp role_support(:verb_particle_construction, :particle), do: 0.28
  defp role_support(:verb_particle_construction, :verb), do: 0.16
  defp role_support(:verb_particle_construction, :object), do: 0.12
  defp role_support(:verb_particle_construction, :determiner), do: 0.10
  defp role_support(:noun_phrase, :head), do: 0.18
  defp role_support(:noun_phrase, :determiner), do: 0.14
  defp role_support(:noun_phrase, role) when role in [:modifier, :modifier_1, :modifier_2], do: 0.12
  defp role_support(:prepositional_phrase, :preposition), do: 0.18
  defp role_support(:prepositional_phrase, role) when role in [:object, :head], do: 0.14
  defp role_support(:prepositional_phrase, :determiner), do: 0.10
  defp role_support(:prepositional_phrase, role) when role in [:modifier, :modifier_1, :modifier_2], do: 0.10
  defp role_support(_, _), do: 0.0

  defp matches_pos?(:pos_noun_or_pronoun, pos),
    do: pos in ["noun", "proper_noun", "pronoun"]

  defp matches_pos?(:pos_noun, pos), do: pos in ["noun", "proper_noun"]
  defp matches_pos?(:pos_verb, "verb"), do: true
  defp matches_pos?(:pos_particle, "particle"), do: true
  defp matches_pos?(:pos_adverb, "adverb"), do: true
  defp matches_pos?(:pos_determiner, "determiner"), do: true
  defp matches_pos?(:pos_preposition, "preposition"), do: true
  defp matches_pos?(:pos_adjective, "adjective"), do: true
  defp matches_pos?(_, _), do: false

  defp finite_present_sense?(cell) do
    evidence = tags(cell)

    Enum.all?(
      ["tag:form-of", "tag:indicative", "tag:present", "tag:singular", "tag:third-person"],
      &MapSet.member?(evidence, &1)
    )
  end

  defp simple_past?(cell) do
    tags = tags(cell)

    MapSet.member?(tags, "tag:past") and MapSet.member?(tags, "tag:form-of") and
      not MapSet.member?(tags, "tag:participle")
  end

  defp past_participle?(cell) do
    tags = tags(cell)
    MapSet.member?(tags, "tag:past") and MapSet.member?(tags, "tag:participle")
  end

  defp obligation_sense?(cell) do
    grammar = get(cell, :gram_function, []) |> as_list()
    definition = get(cell, :definition, "")

    Enum.any?(grammar, &(&1 in ["modal", "auxiliary"])) or
      (is_binary(definition) and
         Regex.match?(~r/\b(obligation|necessity|required to)\b/i, definition))
  end

  defp tags(cell), do: cell |> get(:semantic_atoms, []) |> as_list() |> MapSet.new()

  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_, pos, _] -> pos
      _ -> nil
    end
  end

  defp id_pos(_), do: nil

  defp get(%{} = map, key, default) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key)) || default
  end

  defp get(_, _, default), do: default
  defp as_list(list) when is_list(list), do: list
  defp as_list(_), do: []
end
