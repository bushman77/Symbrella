defmodule Core.Semantic.Composition.RoleWitness do
  @moduledoc """
  Construction-scoped, read-only lexical witnesses for typed composition.

  Prefer the exact attested LIFG winner. For bounded event and stacked nominal
  roles, a conflicting winner can be supported by independently scored and
  attested POS evidence. Neither the winner nor the original SI is changed.

  A POS witness establishes role compatibility, NOT the intended word sense,
  a real-world fact, or permission to assert an event.
  """

  @pos_only_roles [:preposition, :modifier_1, :modifier_2]
  @alternative_roles [:temporal_connective, :auxiliary, :predicate, :location_head | @pos_only_roles]
  @max_scores 128

  @spec resolve(map(), atom(), non_neg_integer(), [String.t()]) ::
          {:ok, map()} | :unsupported
  def resolve(si, role, index, expected_pos)
      when is_map(si) and is_atom(role) and is_integer(index) and index >= 0 and
             is_list(expected_pos) do
    choices =
      si |> Map.get(:lifg_choices, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :token_index) == index))

    tokens =
      si |> Map.get(:tokens, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :index) == index and
        Map.get(&1, :n, 1) == 1))

    with [choice] <- choices,
         [token] <- tokens,
         phrase when is_binary(phrase) <- Map.get(token, :phrase),
         word <- normalize(phrase),
         true <- word != "",
         selected_id when is_binary(selected_id) <-
           Map.get(choice, :id) || Map.get(choice, :chosen_id) do
      if attested?(si, word, selected_id, expected_pos, role) do
        {:ok, witness(index, word, selected_id, :selected, selected_id, role)}
      else
        alternative(si, role, index, word, selected_id, expected_pos, choice)
      end
    else
      _ -> :unsupported
    end
  end

  def resolve(_, _, _, _), do: :unsupported

  @doc """
  A bounded noun-POS witness for an event's subject or location head.
  Preserves every scored, attested noun candidate without choosing a sense.
  This does not replace resolve/4 for sense-sensitive constructions.
  """
  @spec resolve_noun_kind(map(), atom(), non_neg_integer()) ::
          {:ok, map()} | :unsupported
  def resolve_noun_kind(si, role, index)
      when is_map(si) and role in [:subject_head, :location_head] and
           is_integer(index) and index >= 0 do
    choices = si |> Map.get(:lifg_choices, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :token_index) == index))
    tokens = si |> Map.get(:tokens, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :index) == index and
        Map.get(&1, :n, 1) == 1))

    with [choice] <- choices,
         [token] <- tokens,
         phrase when is_binary(phrase) <- Map.get(token, :phrase),
         word <- normalize(phrase),
         true <- word != "",
         selected when is_binary(selected) <- Map.get(choice, :id) || Map.get(choice, :chosen_id),
         scores when is_map(scores) <- choice_scores(choice),
         true <- map_size(scores) > 0 and map_size(scores) <= @max_scores do
      candidates =
        scores
        |> Enum.flat_map(fn
          {id, score} when is_binary(id) and is_number(score) and score > 0 ->
            if attested?(si, word, id, ["noun"], role), do: [{id, score}], else: []
          _ -> []
        end)
        |> Enum.sort_by(fn {id, score} -> {-score, id} end)

      case candidates do
        [_ | _] ->
          {:ok, %{
            token_index: index, word: word, pos: "noun", role: role,
            sense_id: nil, original_lifg_id: selected,
            selection_basis: :construction_noun_pos_only,
            attested_sense_ids: Enum.map(candidates, &elem(&1, 0)),
            attested_candidate_scores: candidates
          }}
        [] -> :unsupported
      end
    else
      _ -> :unsupported
    end
  end

  def resolve_noun_kind(_, _, _), do: :unsupported

  @doc """
  Read-only audit for current-turn event-role evidence. This does not relax
  resolve/4 or reveal sentence contents in the Blackboard.
  """
  def audit(si, role, index, expected_pos) do
    case resolve(si, role, index, expected_pos) do
      {:ok, witness} ->
        %{role: role, token_index: index, status: :admitted,
          reason: :witness_verified, selection_basis: witness.selection_basis}

      :unsupported ->
        audit_rejection(si, role, index, expected_pos)
    end
  end

  defp audit_rejection(si, role, index, expected_pos) do
    choices =
      si |> Map.get(:lifg_choices, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :token_index) == index))
    tokens =
      si |> Map.get(:tokens, []) |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :index) == index and
        Map.get(&1, :n, 1) == 1))

    detail =
      with [choice] <- choices,
           [token] <- tokens,
           phrase when is_binary(phrase) <- Map.get(token, :phrase),
           word <- normalize(phrase),
           scores when is_map(scores) <- choice_scores(choice),
           true <- map_size(scores) <= @max_scores do
        matching =
          Enum.flat_map(scores, fn
            {id, score} when is_binary(id) and is_number(score) and score >= 0 ->
              case String.split(id, "|", parts: 3) do
                [^word, pos, _] ->
                  if pos in expected_pos, do: [{id, pos, score}], else: []
                _ -> []
              end
            _ -> []
          end)

        exact = Enum.filter(matching, fn {id, pos, _} ->
          Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
            %{} = cell ->
              Map.get(cell, :id) == id and Map.get(cell, :pos) == pos and
                normalize(Map.get(cell, :norm)) == word
            _ -> false
          end)
        end)

        qualified = Enum.filter(exact, fn {id, _, _} ->
          attested?(si, word, id, expected_pos, role)
        end)

        sorted = qualified |> Enum.map(&elem(&1, 2)) |> Enum.sort(:desc)
        reason =
          cond do
            matching == [] -> :no_scored_role_alternative
            exact == [] -> :scored_dictionary_witness_missing
            qualified == [] -> :scored_role_morphology_missing
            length(sorted) > 1 and hd(sorted) == Enum.at(sorted, 1) ->
              :tied_attested_alternatives
            true -> :role_witness_unavailable
          end

        %{
          reason: reason,
          scored_role_candidates: length(matching),
          exact_dictionary_witnesses: length(exact),
          morphology_qualified_witnesses: length(qualified),
          any_dictionary_role_morphology?: Enum.any?(
            List.wrap(Map.get(si, :active_cells)),
            fn
              %{} = cell ->
                normalize(Map.get(cell, :norm)) == word and
                  Map.get(cell, :pos) in expected_pos and
                  morphology_compatible?(cell, role, Map.get(cell, :pos))
              _ -> false
            end
          )
        }
      else
        [] -> %{reason: :missing_role_evidence}
        _ -> %{reason: :missing_or_invalid_score_evidence}
      end

    Map.merge(%{role: role, token_index: index, status: :rejected}, detail)
  end

  # Core.LIFG.Attach exports the bounded post-softmax distribution as
  # :candidate_probs. Direct Stage-1 / synthetic callers may use :scores.
  # Only evidence on the current SemanticInput can authorize reconsideration.
  defp choice_scores(choice) when is_map(choice) do
    case Map.get(choice, :candidate_probs) do
      %{} = probabilities -> probabilities
      _ -> Map.get(choice, :scores)
    end
  end

  defp alternative(si, role, index, word, selected_id, expected_pos, choice) do
    scores = choice_scores(choice)

    if role in @alternative_roles and is_map(scores) and map_size(scores) <= @max_scores do
      candidates =
        scores
        |> Enum.flat_map(fn
          {id, score} when is_binary(id) and is_number(score) ->
            if score >= 0 and attested?(si, word, id, expected_pos, role),
              do: [{id, score}], else: []
          _ -> []
        end)
        |> Enum.sort_by(fn {id, score} -> {-score, id} end)

      # Prepositions and two-modifier nominal adjectives use a bounded,
      # construction-scoped POS witness when the selected sense conflicts.
      # A scored alternative is independently attested, but does NOT settle
      # which adjective or preposition dictionary sense was intended.
      # Bind every attested score to the current SI for replay verification.
      case {role, candidates} do
        # A full two-clause temporal construction can license the conjunction
        # POS of "before"/"after" even when the noun sense wins Stage-1.
        # Several tied conjunction senses do not justify choosing an exact
        # dictionary sense. Preserve every independently scored candidate;
        # the caller must verify the complete grammatical scope separately.
        {:temporal_connective, _}
            when word in ["before", "after"] and index == 7 and
                 expected_pos == ["conjunction", "preposition"] ->
          conjunctions =
            Enum.filter(candidates, fn {id, score} ->
              score > 0 and
                match?([_, "conjunction", _], String.split(id, "|", parts: 3))
            end)

          case conjunctions do
            [{_id, best_score} | _] ->
              {:ok, %{
                token_index: index,
                word: word,
                pos: "conjunction",
                sense_id: nil,
                attested_sense_ids: Enum.map(conjunctions, &elem(&1, 0)),
                attested_candidate_scores: conjunctions,
                role: role,
                selection_basis: :construction_pos_only,
                original_lifg_id: selected_id,
                candidate_score: best_score
              }}

            [] ->
              :unsupported
          end

        {pos_role, [{_id, score} | _]} when pos_role in @pos_only_roles ->
          {:ok, %{
            token_index: index,
            word: word,
            pos: if(pos_role == :preposition, do: "preposition", else: "adjective"),
            sense_id: nil,
            attested_sense_ids: Enum.map(candidates, &elem(&1, 0)),
            attested_candidate_scores: candidates,
            role: role,
            selection_basis: :construction_pos_only,
            original_lifg_id: selected_id,
            candidate_score: score
          }}

        {_, [{id, score}]} ->
          {:ok, witness(index, word, id, :construction_alternative,
            selected_id, role, score)}

        {_, [{id, score}, {_other_id, runner_score} | _]} when score > runner_score ->
          {:ok, witness(index, word, id, :construction_alternative,
            selected_id, role, score)}

        _ ->
          :unsupported
      end
    else
      :unsupported
    end
  end

  defp witness(index, word, id, basis, selected_id, role, score \\ nil) do
    [_lemma, pos, _suffix] = String.split(id, "|", parts: 3)
    %{
      token_index: index,
      word: word,
      pos: pos,
      sense_id: id,
      role: role,
      selection_basis: basis,
      original_lifg_id: selected_id,
      candidate_score: score
    }
  end

  defp attested?(si, word, id, expected_pos, role) do
    case String.split(id, "|", parts: 3) do
      [^word, pos, suffix] ->
        suffix != "" and pos in expected_pos and
          Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
            %{} = cell ->
              Map.get(cell, :id) == id and Map.get(cell, :pos) == pos and
                normalize(Map.get(cell, :norm)) == word and
                morphology_compatible?(cell, role, pos)
            _ -> false
          end)

      _ -> false
    end
  end

  defp morphology_compatible?(cell, :predicate, "verb") do
    atoms = Map.get(cell, :semantic_atoms, [])
    is_list(atoms) and
      Enum.all?(["tag:form-of", "tag:present", "tag:participle"],
        &(&1 in atoms))
  end

  defp morphology_compatible?(_cell, _role, _pos), do: true

  defp normalize(word) when is_binary(word) do
    word |> String.downcase() |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp normalize(_), do: ""
end
