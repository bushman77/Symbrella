defmodule Brain.LIFG.RoleCompetition do
  @moduledoc """
  Evidence-gated competition between *incompatible* grammatical hypotheses.

  Unlike independent POS boosts, this read-only adjudicator uses a complete
  larger phrase or attested predicate alongside the original token surfaces.
  A result licenses support for an existing POS, never an exact dictionary
  sense or a factual assertion. Ambiguous or unsupported frames abstain.
  """

  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @max_signatures 128

  @spec resolve(map()) :: map()
  def resolve(%{} = si) do
    signatures = Map.get(si, :sentence_signatures, []) |> List.wrap() |> Enum.take(@max_signatures)
    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(&(Map.get(&1, :n, 1) == 1))
      |> Map.new(fn token -> {Map.get(token, :index), normalize(Map.get(token, :phrase))} end)

    cells =
      si
      |> Map.get(:active_cells, [])
      |> List.wrap()
      |> Enum.filter(&is_map/1)

    proposals =
      Enum.flat_map(signatures, fn
        %{id: :noun_phrase, source: :core_syntax, span: {first, last},
          roles: %{determiner: det_role, modifier: modifier, head: head_role},
          evidence: evidence}
        when is_integer(first) and is_integer(last) and is_integer(modifier) and
             det_role == first and head_role == last and
             modifier == first + 1 and last == first + 2 and is_list(evidence) ->
          if :nominal_constituent in evidence and :pos_pattern in evidence and
               MapSet.member?(@determiners, Map.get(tokens, first)) and
               witnessed?(cells, tokens, first, "determiner") and
               witnessed?(cells, tokens, modifier, "adjective") and
               witnessed?(cells, tokens, last, "noun") do
            [%{token_index: modifier, pos: "adjective",
               role: :modifier, frame: :noun_phrase,
               span: {first, last},
               basis: :complete_determiner_adjective_noun,
               evidence: [:bounded_determiner_surface, :dictionary_pos_witness,
                          :complete_nominal_constituent]}]
          else
            []
          end

        _ ->
          []
      end)

    past = Enum.filter(signatures, fn
      %{id: :finite_past_candidate, source: :core_syntax,
        roles: %{verb: i}, evidence: evidence}
      when is_integer(i) and is_list(evidence) ->
        :simple_past_form_available in evidence

      _ -> false
    end)

    subject_proposals =
      Enum.flat_map(signatures, fn
        %{id: :noun_phrase, source: :core_syntax,
          span: {determiner, head},
          roles: %{determiner: det_role, head: head_role},
          evidence: evidence}
        when is_integer(determiner) and is_integer(head) and
             det_role == determiner and head_role == head and
             head == determiner + 1 and is_list(evidence) ->
          predicate = head + 1
          attested_predicate? =
            Enum.any?(past, fn sig ->
              Map.get(sig.roles, :verb) == predicate and
                witnessed_past?(cells, tokens, predicate)
            end)

          if :nominal_constituent in evidence and
               MapSet.member?(@determiners, Map.get(tokens, determiner)) and
               witnessed?(cells, tokens, determiner, "determiner") and
               witnessed?(cells, tokens, head, "noun") and attested_predicate? do
            [%{token_index: head, pos: "noun", role: :subject_head,
               frame: :finite_past_candidate, span: {determiner, predicate},
               basis: :determined_subject_before_attested_past_predicate,
               evidence: [:bounded_determiner_surface, :dictionary_pos_witness,
                          :attested_simple_past_predicate]}]
          else
            []
          end

        _ ->
          []
      end)

    # A complete finite-present SVO signature carries independent morphological
    # evidence for the predicate. Support only its *existing* nominal subject
    # alternative, without inferring or verifying any dictionary meaning.
    present_subject_proposals =
      Enum.flat_map(signatures, fn
        %{id: :finite_present_svo_candidate, source: :core_syntax,
          span: {start, last},
          roles: %{subject_determiner: det, subject_head: subject,
                   predicate: predicate, object_head: object}, evidence: evidence}
        when is_integer(start) and is_integer(last) and is_integer(det) and
             is_integer(subject) and is_integer(predicate) and is_integer(object) and
             is_list(evidence) ->
          required = [:contiguous_surface, :attested_nominal_subject,
                      :attested_nominal_object, :attested_finite_present_verb]

          if start == det and subject == det + 1 and predicate == subject + 1 and
               predicate < object and object <= last and
               Enum.all?(required, &(&1 in evidence)) and
               MapSet.member?(@determiners, Map.get(tokens, det)) and
               witnessed?(cells, tokens, det, "determiner") and
               witnessed?(cells, tokens, subject, "noun") and
               witnessed?(cells, tokens, predicate, "verb") and
               witnessed?(cells, tokens, object, "noun") do
            [%{token_index: subject, pos: "noun", role: :subject_head,
               frame: :finite_present_svo_candidate, span: {start, last},
               basis: :determined_subject_before_attested_present_predicate,
               evidence: [:complete_finite_present_svo, :dictionary_pos_witness,
                          :attested_finite_present_verb]}]
          else
            []
          end

        _ -> []
      end)

    # "What did the artist draw?" has a complete auxiliary inversion and
    # attested noun/verb alternatives. Neither "artist" nor "draw" is hardcoded:
    # this supports POS only, not a specific word sense or a factual event.
    question_proposals =
      Enum.flat_map(signatures, fn
        %{id: :wh_aux_object_question, source: :core_syntax,
          span: {first, last},
          roles: %{wh: wh, auxiliary: aux, subject_determiner: det,
                   subject_head: subject, predicate: predicate},
          evidence: evidence}
        when is_integer(first) and is_integer(last) and is_integer(wh) and
             is_integer(aux) and is_integer(det) and is_integer(subject) and
             is_integer(predicate) and is_list(evidence) ->
          needed = [:wh_fronted, :auxiliary_inversion, :bounded_determiner_surface,
                    :attested_nominal_subject, :lexical_verb_available,
                    :question_punctuation, :contiguous_surface]

          if first == wh and aux == wh + 1 and det == aux + 1 and
               subject == det + 1 and predicate == subject + 1 and last == predicate and
               Map.get(tokens, wh) in ["what", "who"] and
               Map.get(tokens, aux) in ~w(do does did can could will would should may might) and
               MapSet.member?(@determiners, Map.get(tokens, det)) and
               Enum.all?(needed, &(&1 in evidence)) and
               witnessed?(cells, tokens, det, "determiner") and
               witnessed?(cells, tokens, subject, "noun") and
               witnessed?(cells, tokens, predicate, "verb") do
            basis_evidence = [:wh_fronted, :auxiliary_inversion,
                              :bounded_determiner_surface, :dictionary_pos_witness,
                              :question_punctuation]

            [%{token_index: subject, pos: "noun", role: :subject_head,
               frame: :wh_aux_object_question, span: {first, last},
               basis: :determined_subject_in_wh_aux_question,
               evidence: basis_evidence},
             %{token_index: predicate, pos: "verb", role: :predicate,
               frame: :wh_aux_object_question, span: {first, last},
               basis: :predicate_in_wh_aux_question,
               evidence: basis_evidence ++ [:attested_verb_alternative]}]
          else
            []
          end

        _ -> []
      end)

    # A local determiner + adjective + noun hypothesis can straddle the
    # *predicate* of a complete, independently witnessed clause. For example:
    #   The artist draws a picture.
    #   What did the artist draw?
    # "artist" has an adjective sense and "draw(s)" has a noun sense, so
    # "the artist draw(s)" also looks like a short noun phrase in isolation.
    # Give the complete clause precedence ONLY over that exact overlapping
    # three-token modifier proposal. Other proposals and contradictions among
    # complete clauses still compete normally and may remain unresolved.
    complete_subjects =
      present_subject_proposals ++
        Enum.filter(question_proposals, &(&1.role == :subject_head))

    {local_proposals, superseded_proposals} =
      Enum.split_with(proposals, fn proposal ->
        not superseded_local_modifier?(proposal, complete_subjects)
      end)

    decisions =
      (local_proposals ++ subject_proposals ++ present_subject_proposals ++ question_proposals)
      |> Enum.group_by(& &1.token_index)
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {index, alternatives} ->
        poses = alternatives |> Enum.map(& &1.pos) |> Enum.uniq()
        case poses do
          [pos] ->
            candidate = Enum.min_by(alternatives, fn a ->
              {a.basis, elem(a.span, 0), elem(a.span, 1)}
            end)

            Map.merge(candidate, %{pos: pos, status: :supported,
              proposals: length(alternatives)})

          _ ->
            %{token_index: index, status: :unresolved,
              reason: :incompatible_evidence_supported,
              competing_pos: Enum.sort(poses)}
        end
      end)

    roles =
      decisions
      |> Enum.filter(&(&1.status == :supported))
      |> Map.new(&{&1.token_index, &1})

    %{
      version: 1,
      source: :core_syntax_and_attested_dictionary_pos,
      roles: roles,
      decisions: decisions,
      superseded_proposals: superseded_proposals
    }
  end

  def resolve(_), do: %{version: 1, source: :invalid_input, roles: %{}, decisions: []}

  # A complete finite-present SVO or WH-object question is stronger evidence
  # than the overlapping three-token NP *only* when the NP's nominal head is
  # the independently attested predicate. Do not suppress adjective evidence
  # for ordinary noun phrases or for a different clause boundary.
  defp superseded_local_modifier?(
         %{role: :modifier, pos: "adjective",
           basis: :complete_determiner_adjective_noun,
           span: {local_det, local_head}}, complete_subjects
       ) do
    Enum.any?(complete_subjects, fn
      %{role: :subject_head, pos: "noun", token_index: subject,
        frame: :finite_present_svo_candidate, span: {first, last}} ->
        local_det == subject - 1 and local_head == subject + 1 and
          first == local_det and last > local_head

      %{role: :subject_head, pos: "noun", token_index: subject,
        frame: :wh_aux_object_question, span: {first, last}} ->
        local_det == subject - 1 and local_head == subject + 1 and
          first < local_det and last == local_head

      _ ->
        false
    end)
  end

  defp superseded_local_modifier?(_, _), do: false

  defp witnessed?(cells, tokens, index, pos) do
    word = Map.get(tokens, index)
    is_binary(word) and
      Enum.any?(cells, fn cell ->
        normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == word and
          normalize(Map.get(cell, :pos)) == pos
      end)
  end

  defp witnessed_past?(cells, tokens, index) do
    word = Map.get(tokens, index)

    is_binary(word) and
      Enum.any?(cells, fn cell ->
        tags = Map.get(cell, :semantic_atoms, []) |> List.wrap()
        normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == word and
          normalize(Map.get(cell, :pos)) == "verb" and
          Enum.all?(["tag:form-of", "tag:past"], &(&1 in tags)) and
          not ("tag:participle" in tags)
      end)
  end

  defp normalize(value) when is_atom(value), do: value |> Atom.to_string() |> normalize()
  defp normalize(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp normalize(_), do: nil
end
