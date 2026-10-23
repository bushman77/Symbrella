defmodule Core.Semantic.EmbeddedClauseEvidence do
  @moduledoc """
  Read-only inspection of grammatical candidates *inside* a question's
  unresolved complement. A perfect-predicate signature does not establish
  its subject, reference, truth, or an interpreted embedded proposition.
  """
  @max_candidates 4
  @max_surface 80

  @spec candidates(map(), map()) :: [map()]
  def candidates(%{} = si, %{
        kind: :wh_topic_question,
        status: :structurally_interpreted,
        proposition: %{
          type: :information_request,
          epistemic_status: :question_not_assertion,
          content: %{complement: %{argument: %{
            type: :unparsed_topic, span: {first, last}
          }}}
        }
      })
      when is_integer(first) and is_integer(last) and first <= last do
    surfaces = indexed_surfaces(Map.get(si, :tokens, []))
    winners =
      si
      |> get_in([:atl_slate, :winners])
      |> List.wrap()

    constituents =
      si
      |> get_in([:atl_slate, :constituents])
      |> List.wrap()

    si
    |> Map.get(:sentence_signatures, [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{id: :perfect_predicate, span: {a, b},
        roles: %{auxiliary: aux_index, verb: verb_index}} = signature
      when is_integer(a) and is_integer(b) and
             a == aux_index and b == verb_index and
             a >= first and b <= last and a < b ->
        with {:ok, aux} <- surface_at(surfaces, a),
             {:ok, verb} <- surface_at(surfaces, b) do
          subject_candidate =
            adjacent_nominal_candidate(constituents, surfaces, first, a) ||
              adjacent_adjectival_nominal_candidate(si, constituents, winners, surfaces, first, a)
          subject_diagnostic =
            subject_diagnostic(si, constituents, winners, surfaces, first, a, subject_candidate)
          relation_candidate =
            relation_candidate(subject_candidate, winners, a, b, aux, verb)

          [%{
            construction: :perfect_predicate,
            status: :syntactic_candidate_only,
            span: {a, b},
            auxiliary: %{surface: aux, token_index: a,
              candidate_sense_id: selected_id(winners, a)},
            predicate: %{surface: verb, token_index: b,
              candidate_sense_id: selected_id(winners, b)},
            grammatical_evidence: signature |> Map.get(:evidence, []) |> List.wrap()
              |> Enum.take(6),
            subject: :unresolved,
            subject_candidate: subject_candidate,
            subject_diagnostic: subject_diagnostic,
            relation_candidate: relation_candidate,
            subject_binding_status: :unresolved,
            embedded_clause_status: :not_interpreted,
            assertion_status: :not_asserted,
            verification: :external_unverified
          }]
        else
          _ -> []
        end

      _ ->
        []
    end)
    |> Enum.take(@max_candidates)
  end

  def candidates(_, _), do: []


  # Local adjacency plus a fully resolved ATL nominal supplies evidence for
  # a possible subject, not a verified subject-predicate attachment.
  # Longer NPs take precedence; disconnected, fallback, and lexical-MWE
  # witnesses never qualify.
  defp adjacent_nominal_candidate(constituents, surfaces, topic_start, auxiliary_index) do
    constituents
    |> Enum.filter(fn
      %{id: :noun_phrase, status: :resolved, span: {first, last},
        roles: %{head: %{token_index: head, sense_id: id}}}
      when is_integer(first) and is_integer(last) and
           first >= topic_start and last == auxiliary_index - 1 and
           head == last and is_binary(id) ->
        not String.ends_with?(id, "|fallback") and
          (String.contains?(id, "|noun|") or String.contains?(id, "|proper_noun|"))

      _ -> false
    end)
    |> Enum.sort_by(fn %{span: {first, _}} -> first end)
    |> Enum.find_value(fn %{span: {first, last}, roles: roles} ->
      head_role = Map.fetch!(roles, :head)
      if Map.get(head_role, :selection_scope) == :lexical_mwe do
        nil
      else
        case Enum.map(first..last, &surface_at(surfaces, &1)) do
          words when is_list(words) ->
            if Enum.all?(words, &match?({:ok, _}, &1)) do
              %{
                status: :adjacent_nominal_candidate_only,
                source: :atl_resolved_noun_phrase,
                surface: words |> Enum.map(fn {:ok, word} -> word end) |> Enum.join(" "),
                span: {first, last},
                head: %{token_index: last,
                  candidate_sense_id: head_role.sense_id},
                coreference_status: :unresolved,
                attachment_status: :unverified
              }
            else
              nil
            end
        end
      end
    end)
  end

  # An ATL-resolved determiner + adjective is only a possible nominalized
  # subject. Its head retains an adjective sense: do not reclassify as a noun
  # or assert who/what the phrase denotes. A separately witnessed perfect
  # predicate and exact adjacency are required by the caller.
  defp adjacent_adjectival_nominal_candidate(
         si, constituents, winners, surfaces, topic_start, auxiliary_index
       ) do
    start = auxiliary_index - 2
    finish = auxiliary_index - 1

    signature_witnessed? =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.any?(fn
        %{id: :nominalized_adjective_subject_candidate, span: {^start, ^finish},
          roles: %{determiner: ^start, adjective: ^finish}} -> true
        _ -> false
      end)

    if start < topic_start or not signature_witnessed? do
      nil
    else
      Enum.find_value(constituents, fn
        %{id: :nominalized_adjective_subject_candidate,
          status: :resolved,
          span: {^start, ^finish},
          roles: %{
            determiner: %{token_index: ^start, sense_id: det_id} = det,
            adjective: %{token_index: ^finish, sense_id: adjective_id} = adjective
          }}
        when is_binary(det_id) and is_binary(adjective_id) ->
          with true <- sense_pos(det_id) == "determiner",
               true <- sense_pos(adjective_id) == "adjective",
               false <- String.ends_with?(det_id, "|fallback"),
               false <- String.ends_with?(adjective_id, "|fallback"),
               true <- selected_id(winners, start) == det_id,
               true <- selected_id(winners, finish) == adjective_id,
               false <- Map.get(det, :selection_scope) == :lexical_mwe,
               false <- Map.get(adjective, :selection_scope) == :lexical_mwe,
               {:ok, det_surface} <- surface_at(surfaces, start),
               {:ok, adj_surface} <- surface_at(surfaces, finish) do
            %{
              status: :adjectival_nominalization_candidate_only,
              source: :atl_resolved_nominalization_hypothesis,
              surface: det_surface <> " " <> adj_surface,
              span: {start, finish},
              head: %{token_index: finish, candidate_sense_id: adjective_id,
                selected_pos: :adjective},
              coreference_status: :unresolved,
              attachment_status: :unverified,
              nominalization_status: :hypothesized_not_confirmed
            }
          else
            _ -> nil
          end

        _ -> nil
      end)
    end
  end

  # Only the adjacent subject candidate and a separately selected lexical
  # verb may yield a question-scoped structural relationship. This never
  # asserts that an event happened or verifies the candidate's referent.
  # The rest of the information-request complement (including contrasts)
  # remains unparsed. An unresolved lexical verb means abstention.
  defp relation_candidate(
         %{span: {start, finish}, surface: surface,
           status: status, source: subject_source,
           head: %{candidate_sense_id: head_id}} = subject,
         winners, auxiliary_index, predicate_index, auxiliary, verb
       )
       when status in [:adjacent_nominal_candidate_only,
                       :adjectival_nominalization_candidate_only] and
            is_integer(start) and is_integer(finish) and
            finish == auxiliary_index - 1 and
            predicate_index == auxiliary_index + 1 and
            is_binary(surface) and is_binary(head_id) do
    verb_id = selected_id(winners, predicate_index)

    if selected_id(winners, finish) == head_id and
         sense_pos(verb_id) == "verb" and
         not String.ends_with?(verb_id, "|fallback") and
         Map.get(subject, :attachment_status) == :unverified and
         Map.get(subject, :coreference_status) == :unresolved do
      %{
        type: :embedded_perfect_clause_candidate,
        status: :question_scoped_structural_relation_candidate,
        source: :atl_nominal_and_perfect_predicate_signature,
        subject_source: subject_source,
        surface: surface <> " " <> auxiliary <> " " <> verb,
        span: {start, predicate_index},
        subject: %{surface: surface, span: {start, finish},
          lexical_head_sense_id: head_id, identity: :unresolved},
        predicate: %{surface: verb, token_index: predicate_index,
          selected_sense_id: verb_id},
        auxiliary: %{surface: auxiliary, token_index: auxiliary_index},
        scope: :unparsed_information_request_complement,
        attachment_status: :unverified,
        assertion_status: :not_asserted,
        embedded_clause_status: :not_fully_interpreted,
        remainder_status: :not_interpreted,
        verification: :external_unverified
      }
    else
      nil
    end
  end

  defp relation_candidate(_, _, _, _, _, _), do: nil

  # Explain abstention from the already observed signatures, ATL reductions,
  # and selected lexical hypotheses. No new constituent or sense is produced.
  defp subject_diagnostic(si, constituents, winners, surfaces, topic_start, auxiliary_index, candidate) do
    preceding = auxiliary_index - 1
    window_start = max(topic_start, auxiliary_index - 4)

    indices =
      if preceding >= window_start, do: Enum.to_list(window_start..preceding), else: []

    window =
      Enum.flat_map(indices, fn index ->
        case surface_at(surfaces, index) do
          {:ok, surface} ->
            id = selected_id(winners, index)

            [%{
              index: index,
              surface: surface,
              selected_sense_id: id,
              selected_pos: sense_pos(id)
            }]

          _ ->
            []
        end
      end)

    signatures =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.flat_map(fn
        %{id: :noun_phrase, span: {start, ^preceding}}
        when is_integer(start) and start >= topic_start ->
          [%{span: {start, preceding}}]

        _ ->
          []
      end)
      |> Enum.take(6)

    atl =
      constituents
      |> Enum.flat_map(fn
        %{id: :noun_phrase, span: {start, ^preceding}} = item
        when is_integer(start) and start >= topic_start ->
          [%{
            span: {start, preceding},
            status: Map.get(item, :status, :unknown),
            unresolved_roles: item |> Map.get(:unresolved_roles, []) |> List.wrap()
              |> Enum.take(6)
          }]

        _ ->
          []
      end)
      |> Enum.take(6)

    nominalization_signatures =
      si
      |> Map.get(:sentence_signatures, [])
      |> List.wrap()
      |> Enum.flat_map(fn
        %{id: :nominalized_adjective_subject_candidate,
          span: {start, ^preceding}}
        when is_integer(start) and start >= topic_start ->
          [%{span: {start, preceding}}]

        _ -> []
      end)
      |> Enum.take(6)

    nominalization_atl =
      constituents
      |> Enum.flat_map(fn
        %{id: :nominalized_adjective_subject_candidate,
          span: {start, ^preceding}} = item
        when is_integer(start) and start >= topic_start ->
          [%{
            span: {start, preceding},
            status: Map.get(item, :status, :unknown),
            unresolved_roles: item |> Map.get(:unresolved_roles, [])
              |> List.wrap() |> Enum.take(6)
          }]

        _ -> []
      end)
      |> Enum.take(6)

    reason =
      cond do
        is_map(candidate) -> :adjacent_nominal_candidate_available
        nominalization_signatures != [] and nominalization_atl == [] ->
          :adjectival_nominalization_not_resolved_by_atl
        Enum.any?(nominalization_atl, &(&1.status != :resolved)) ->
          :atl_adjectival_nominalization_unresolved
        nominalization_atl != [] ->
          :adjectival_nominalization_failed_candidate_checks
        signatures == [] -> :no_adjacent_noun_phrase_signature
        atl == [] -> :noun_phrase_not_resolved_by_atl
        Enum.any?(atl, &(&1.status != :resolved)) -> :atl_nominal_unresolved
        true -> :resolved_nominal_failed_candidate_checks
      end

    %{
      status: reason,
      token_window: window,
      adjacent_noun_signatures: signatures,
      adjacent_atl_noun_phrases: atl,
      adjacent_nominalization_signatures: nominalization_signatures,
      adjacent_atl_nominalizations: nominalization_atl,
      selected_senses_are_hypotheses?: true,
      subject_binding_status: :unresolved
    }
  end

  defp sense_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_lemma, pos, _sense] -> pos
      _ -> nil
    end
  end

  defp sense_pos(_), do: nil

  defp indexed_surfaces(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {token, fallback}, acc ->
      if is_map(token) and Map.get(token, :n, 1) == 1 do
        index = Map.get(token, :index, fallback)
        surface = Map.get(token, :phrase) || Map.get(token, :norm)

        if is_integer(index) and is_binary(surface) do
          Map.put(acc, index, surface)
        else
          acc
        end
      else
        acc
      end
    end)
  end

  defp indexed_surfaces(_), do: %{}

  defp surface_at(surfaces, index) do
    case Map.get(surfaces, index) do
      word when is_binary(word) ->
        normalized =
          word
          |> String.trim()
          |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
          |> String.slice(0, @max_surface)

        if normalized == "", do: :error, else: {:ok, normalized}

      _ ->
        :error
    end
  end

  defp selected_id(winners, index) do
    case Enum.find(winners, fn winner ->
           is_map(winner) and Map.get(winner, :token_index) == index and
             is_binary(Map.get(winner, :id))
         end) do
      %{id: id} -> String.slice(id, 0, 128)
      _ -> nil
    end
  end
end
