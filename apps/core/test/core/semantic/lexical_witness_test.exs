defmodule Core.Semantic.LexicalWitnessTest do
  @moduledoc """
  P-040: each LexicalWitness policy must reproduce its source function
  exactly. The oracles below are verbatim copies of the private functions in
  SVOEvent, SentenceMeaning and ParticipantSpatial at fd241a5.
  """
  use ExUnit.Case, async: true

  alias Core.Semantic.LexicalWitness

  # ---- verbatim oracles ---------------------------------------------------

  # SVOEvent.single_choice/3
  defp svo_single_choice(si, index, pos) do
    selected =
      get(si, :lifg_choices)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :token_index) == index))

    case selected do
      [one] ->
        id = get(one, :chosen_id) || get(one, :id)

        if is_binary(id) and String.contains?(id, "|" <> pos <> "|"),
          do: {:ok, one},
          else: :error

      _ ->
        :error
    end
  end

  # SVOEvent.atl_match?/2
  defp svo_atl_match?(si, role_choices) do
    slate = get(si, :atl_slate)
    winners = if is_map(slate), do: get(slate, :winners), else: nil

    is_list(winners) and
      Enum.all?(role_choices, fn {index, choice} ->
        case Enum.filter(winners, &(get(&1, :token_index) == index)) do
          [winner] ->
            (get(winner, :id) || get(winner, :chosen_id)) ==
              (get(choice, :chosen_id) || get(choice, :id))

          _ ->
            false
        end
      end)
  end

  # SentenceMeaning.provisional_selected_witness/4
  defp sm_selected_witness(si, index, pos, surface) do
    selected =
      si
      |> Map.get(:lifg_choices, [])
      |> List.wrap()
      |> Enum.filter(fn
        %{token_index: ^index} -> true
        _ -> false
      end)

    case selected do
      [choice] ->
        id = Map.get(choice, :id) || Map.get(choice, :chosen_id)

        case id do
          id when is_binary(id) ->
            case String.split(id, "|", parts: 3) do
              [norm, ^pos, _sense] ->
                cells = List.wrap(Map.get(si, :active_cells))

                witnessed? =
                  Enum.any?(cells, fn
                    %{id: ^id, pos: ^pos, norm: cell_norm} when is_binary(cell_norm) ->
                      String.downcase(cell_norm) == String.downcase(surface)

                    _ ->
                      false
                  end)

                if String.downcase(norm) == String.downcase(surface) and witnessed?,
                  do: {:ok, id},
                  else: :error

              _ ->
                :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  # SentenceMeaning.object_roles_agree_with_winners?/2
  defp sm_roles_agree?(si, roles) do
    atl = Map.get(si, :atl_slate) || %{}
    winners = Map.get(atl, :winners)

    selections =
      case Map.get(si, :semantic_choices) do
        selected when is_list(selected) -> selected
        _ -> Map.get(si, :lifg_choices)
      end

    is_list(winners) and is_list(selections) and
      Enum.all?(roles, fn
        {_role, %{token_index: index, sense_id: sense_id}} ->
          Enum.count(winners, fn winner ->
            Map.get(winner, :token_index) == index and Map.get(winner, :id) == sense_id
          end) == 1 and
            Enum.count(selections, fn selected ->
              Map.get(selected, :token_index) == index and
                (Map.get(selected, :id) || Map.get(selected, :chosen_id)) == sense_id
            end) == 1

        _ ->
          false
      end)
  end

  # ParticipantSpatial.selected/3 + selected_or_attested/4 + attested_pos?/3
  defp ps_selected(si, index, pos) do
    selections =
      si |> get(:lifg_choices) |> List.wrap() |> Enum.filter(&(get(&1, :token_index) == index))

    winners =
      si
      |> get(:atl_slate)
      |> get(:winners)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :token_index) == index))

    with [selection] <- selections,
         [winner] <- winners,
         id when is_binary(id) <- get(selection, :chosen_id) || get(selection, :id),
         true <- String.contains?(id, "|" <> pos <> "|"),
         ^id <- get(winner, :id) || get(winner, :chosen_id) do
      {:ok, selection}
    else
      _ -> :error
    end
  end

  defp ps_selected_or_attested(si, index, pos, surface) do
    choices =
      si |> get(:lifg_choices) |> List.wrap() |> Enum.filter(&(get(&1, :token_index) == index))

    winners =
      si
      |> get(:atl_slate)
      |> get(:winners)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :token_index) == index))

    case {choices, winners} do
      {[], []} ->
        if ps_attested_pos?(si, surface, pos), do: {:ok, :attested_grammar_role}, else: :error

      {[_], [_]} ->
        ps_selected(si, index, pos)

      _ ->
        :error
    end
  end

  defp ps_attested_pos?(si, surface, pos) do
    word = clean(surface)

    si
    |> get(:active_cells)
    |> List.wrap()
    |> Enum.any?(fn cell ->
      id = get(cell, :id)
      norm = get(cell, :norm)
      cell_pos = get(cell, :pos)

      is_binary(id) and is_binary(norm) and
        clean(norm) == word and cell_pos == pos and
        String.starts_with?(id, word <> "|" <> pos <> "|")
    end)
  end

  defp clean(text) when is_binary(text),
    do: text |> String.trim() |> String.trim_trailing(".") |> String.downcase()

  defp get(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil

  # ---- fixture matrix -----------------------------------------------------

  defp cell(word, pos, n \\ 1), do: %{id: "#{word}|#{pos}|#{n}", norm: word, pos: pos}
  defp choice(i, id), do: %{token_index: i, chosen_id: id, id: id}

  defp base_si do
    %{
      lifg_choices: [
        choice(0, "the|determiner|1"),
        choice(1, "dog|noun|1"),
        choice(2, "chased|verb|1"),
        choice(4, "cat|noun|26")
      ],
      atl_slate: %{
        winners: [
          %{token_index: 0, id: "the|determiner|1"},
          %{token_index: 1, id: "dog|noun|1"},
          %{token_index: 2, id: "chased|verb|1"},
          %{token_index: 4, id: "cat|noun|26"}
        ]
      },
      active_cells: [
        cell("the", "determiner"),
        cell("dog", "noun"),
        cell("chased", "verb"),
        cell("cat", "noun", 26),
        cell("garden", "noun")
      ]
    }
  end

  # Each case: {label, si, index, pos, surface}
  defp cases do
    si = base_si()

    [
      {"exact noun", si, 1, "noun", "dog"},
      {"exact verb", si, 2, "verb", "chased"},
      {"surface case differs", si, 1, "noun", "Dog"},
      {"surface mismatch", si, 1, "noun", "cat"},
      {"wrong pos", si, 1, "verb", "dog"},
      {"no choice, attested", si, 3, "noun", "garden"},
      {"no choice, unattested", si, 3, "noun", "tree"},
      {"missing cell", %{si | active_cells: []}, 1, "noun", "dog"},
      {"two choices", %{si | lifg_choices: [choice(1, "dog|noun|1") | si.lifg_choices]}, 1,
       "noun", "dog"},
      {"chosen_id differs from id",
       %{si | lifg_choices: [%{token_index: 1, chosen_id: "dog|verb|2", id: "dog|noun|1"}]}, 1,
       "noun", "dog"},
      {"atl winner disagrees",
       put_in(si, [:atl_slate, :winners], [%{token_index: 1, id: "dog|noun|8"}]), 1, "noun",
       "dog"},
      {"string-keyed choice",
       %{si | lifg_choices: [%{"token_index" => 1, "chosen_id" => "dog|noun|1"}]}, 1, "noun",
       "dog"},
      {"semantic choices present (ignored by :lifg)",
       Map.put(si, :semantic_choices, [choice(1, "dog|verb|3")]), 1, "noun", "dog"},
      {"no lifg_choices key", Map.delete(si, :lifg_choices), 1, "noun", "dog"}
    ]
  end

  defp strip({:ok, %{} = w}, :id_pos_only), do: {:ok, w.choice}
  defp strip({:ok, %{} = w}, :selected_exact), do: {:ok, w.selected_id}

  defp strip({:ok, %{basis: :attested_grammar_role}}, :selected_or_attested),
    do: {:ok, :attested_grammar_role}

  defp strip({:ok, %{} = w}, :selected_or_attested), do: {:ok, w.choice}
  defp strip(:error, _), do: :error

  test ":id_pos_only reproduces SVOEvent.single_choice/3 on every case" do
    for {label, si, index, pos, surface} <- cases() do
      assert strip(LexicalWitness.witness(si, index, pos, surface, :id_pos_only), :id_pos_only) ==
               svo_single_choice(si, index, pos),
             label
    end
  end

  test ":selected_exact reproduces SentenceMeaning.provisional_selected_witness/4" do
    for {label, si, index, pos, surface} <- cases() do
      assert strip(
               LexicalWitness.witness(si, index, pos, surface, :selected_exact),
               :selected_exact
             ) == sm_selected_witness(si, index, pos, surface),
             label
    end
  end

  test ":selected_or_attested reproduces ParticipantSpatial.selected_or_attested/4" do
    for {label, si, index, pos, surface} <- cases() do
      assert strip(
               LexicalWitness.witness(si, index, pos, surface, :selected_or_attested),
               :selected_or_attested
             ) == ps_selected_or_attested(si, index, pos, surface),
             label
    end
  end

  test "the matrix actually separates the policies (they are not equivalent)" do
    outcomes =
      for {_label, si, index, pos, surface} <- cases() do
        for policy <- LexicalWitness.policies() do
          match?({:ok, _}, LexicalWitness.witness(si, index, pos, surface, policy))
        end
      end

    # At least one case where each pair of policies disagrees.
    assert Enum.any?(outcomes, fn [a, b, _] -> a != b end)
    assert Enum.any?(outcomes, fn [a, _, c] -> a != c end)
    assert Enum.any?(outcomes, fn [_, b, c] -> b != c end)
  end

  test ":winner_only reproduces SVOEvent.atl_match?/2" do
    for {label, si, index, _pos, _surface} <- cases() do
      ids =
        for i <- [0, index] do
          case Enum.find(List.wrap(get(si, :lifg_choices)), &(get(&1, :token_index) == i)) do
            nil -> {i, %{}}
            c -> {i, c}
          end
        end

      expected = svo_atl_match?(si, ids)
      sense_ids = Enum.map(ids, fn {i, c} -> {i, get(c, :chosen_id) || get(c, :id)} end)
      assert LexicalWitness.atl_agrees?(si, sense_ids, :winner_only) == expected, label
    end
  end

  test ":winner_and_selection reproduces SentenceMeaning.object_roles_agree_with_winners?/2" do
    for {label, si, index, _pos, _surface} <- cases(),
        sense_id <- ["dog|noun|1", "chased|verb|1", "dog|verb|3", "nope|noun|1"] do
      roles = %{actor: %{token_index: index, sense_id: sense_id}}

      assert LexicalWitness.atl_agrees?(si, [{index, sense_id}], :winner_and_selection) ==
               sm_roles_agree?(si, roles),
             "#{label} / #{sense_id}"
    end
  end

  test "witness records basis and choice source" do
    si = base_si()

    assert {:ok, %{basis: :selected_exact, choice_source: :lifg, selected_id: "dog|noun|1"}} =
             LexicalWitness.witness(si, 1, "noun", "dog", :selected_exact)

    assert {:ok, %{basis: :attested_grammar_role, selected_id: nil}} =
             LexicalWitness.witness(si, 3, "noun", "garden", :selected_or_attested)
  end

  test ":semantic_preferred reads MWE-reanchored choices when present" do
    si = Map.put(base_si(), :semantic_choices, [choice(1, "dog|verb|3")])

    assert {:ok, _} = LexicalWitness.witness(si, 1, "noun", "dog", :id_pos_only)

    assert :error =
             LexicalWitness.witness(si, 1, "noun", "dog", :id_pos_only,
               choice_source: :semantic_preferred
             )

    assert {:ok, %{selected_id: "dog|verb|3", choice_source: :semantic_preferred}} =
             LexicalWitness.witness(si, 1, "verb", "dog", :id_pos_only,
               choice_source: :semantic_preferred
             )
  end

  test ":semantic_preferred falls back to lifg_choices when semantic_choices is absent or nil" do
    for si <- [base_si(), Map.put(base_si(), :semantic_choices, nil)],
        policy <- [:id_pos_only, :selected_exact, :selected_or_attested] do
      assert LexicalWitness.witness(si, 1, "noun", "dog", policy) ==
               (case LexicalWitness.witness(si, 1, "noun", "dog", policy,
                       choice_source: :semantic_preferred
                     ) do
                  {:ok, w} -> {:ok, %{w | choice_source: :lifg}}
                  other -> other
                end),
             inspect(policy)
    end
  end

  test "malformed input never raises" do
    assert :error = LexicalWitness.witness(nil, 1, "noun", "dog", :selected_exact)
    assert :error = LexicalWitness.witness(%{}, -1, "noun", "dog", :selected_exact)
    assert :error = LexicalWitness.witness(%{}, 1, :noun, "dog", :selected_exact)
    assert :error = LexicalWitness.witness(base_si(), 1, "noun", nil, :selected_exact)
    assert :error = LexicalWitness.witness(base_si(), 1, "noun", "dog", :unknown_policy)

    assert :error =
             LexicalWitness.witness(base_si(), 1, "noun", "dog", :id_pos_only,
               choice_source: :other
             )

    refute LexicalWitness.atl_agrees?(nil, [{1, "dog|noun|1"}], :winner_only)
    refute LexicalWitness.atl_agrees?(%{lifg_choices: :bad}, [{1, "x"}], :winner_and_selection)
  end
end
