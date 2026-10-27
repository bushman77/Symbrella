defmodule Core.Semantic.Composition.RoleWitness do
  @moduledoc """
  Construction-scoped, read-only lexical witnesses for V2 event composition.

  Prefer the exact attested LIFG winner. For bounded event roles only, a
  conflicting winner may be supplemented by a unique, scored, independently
  attested POS alternative. Neither the winner nor the original SI is changed.

  A POS witness establishes role compatibility, NOT the intended word sense,
  a real-world fact, or permission to assert an event.
  """

  @alternative_roles [:auxiliary, :predicate, :location_head]
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

  defp alternative(si, role, index, word, selected_id, expected_pos, choice) do
    scores = Map.get(choice, :scores)

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

      # A tied POS candidate does not justify selecting an arbitrary sense.
      case candidates do
        [{id, score} | rest] when rest == [] ->
          {:ok, witness(index, word, id, :construction_alternative,
            selected_id, role, score)}

        [{id, score}, {_other_id, runner_score} | _] when score > runner_score ->
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
      [^word, pos, suffix] when pos in expected_pos and suffix != "" ->
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
