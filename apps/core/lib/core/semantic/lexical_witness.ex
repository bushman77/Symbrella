defmodule Core.Semantic.LexicalWitness do
  @moduledoc """
  P-040: one shared, policy-parameterized lexical role witness.

  Before P-040 the same claim ("the selected sense at token `i` has POS `p`")
  was checked by four composers with four different evidence standards
  (COLLAB §36.2 B1). This module reproduces three of those standards
  *exactly*, as named policies, so that existing behaviour can later be
  migrated one consumer at a time with byte-identical differential tests.

  Policies (each mirrors one existing private function verbatim):

    * `:id_pos_only` — `Core.Semantic.SVOEvent.single_choice/3`.
      Exactly one LIFG choice at the index; its `chosen_id || id` contains
      `"|pos|"`. No surface match, no dictionary-cell attestation.
    * `:selected_exact` — `Core.Semantic.SentenceMeaning.provisional_selected_witness/4`.
      Exactly one LIFG choice (atom keys); `id || chosen_id` splits into
      `[norm, pos, sense]`; `norm` equals the surface (case-insensitive) and
      a current-turn `active_cells` row with the same id, POS and norm exists.
    * `:selected_or_attested` — `Core.Semantic.ParticipantSpatial.selected_or_attested/4`.
      With no LIFG choice *and* no ATL winner at the index, a dictionary row
      attesting the POS for the surface suffices (`basis: :attested_grammar_role`,
      no sense id). With exactly one of each, the choice must contain
      `"|pos|"` and equal the ATL winner.

  `Composition.RoleWitness` is deliberately NOT reproduced here: its
  construction-scoped alternative logic stays with its constructions
  (COLLAB §34.6).

  A witness establishes role compatibility under a declared standard. It is
  never a verified word sense, a referent, or a fact. Pure; never raises on
  malformed input.
  """

  @type policy :: :id_pos_only | :selected_exact | :selected_or_attested
  @type choice_source :: :lifg | :semantic_preferred
  @type t :: %{
          token_index: non_neg_integer(),
          pos: String.t(),
          selected_id: String.t() | nil,
          basis: policy() | :attested_grammar_role,
          choice_source: choice_source(),
          choice: map() | nil
        }

  @policies [:id_pos_only, :selected_exact, :selected_or_attested]

  @doc "The supported policy names."
  @spec policies() :: [policy()]
  def policies, do: @policies

  @doc """
  Witness the role at `index` as `pos` under `policy`.

  Options: `choice_source: :lifg | :semantic_preferred` (default `:lifg`,
  which is what all three source functions read). `:semantic_preferred`
  reads `:semantic_choices` (MWE-reanchored) when it is a list, otherwise
  `:lifg_choices`.
  """
  @spec witness(map(), non_neg_integer(), String.t(), String.t() | nil, policy(), keyword()) ::
          {:ok, t()} | :error
  def witness(si, index, pos, surface, policy, opts \\ [])

  def witness(%{} = si, index, pos, surface, policy, opts)
      when is_integer(index) and index >= 0 and is_binary(pos) and policy in @policies and
             is_list(opts) do
    source = Keyword.get(opts, :choice_source, :lifg)

    if source in [:lifg, :semantic_preferred] do
      do_witness(policy, si, index, pos, surface, source)
    else
      :error
    end
  end

  def witness(_, _, _, _, _, _), do: :error

  # -- :id_pos_only (SVOEvent.single_choice/3) -------------------------------
  defp do_witness(:id_pos_only, si, index, pos, _surface, source) do
    selected =
      si
      |> choices(source, :loose)
      |> Enum.filter(&(get(&1, :token_index) == index))

    case selected do
      [one] ->
        id = get(one, :chosen_id) || get(one, :id)

        if is_binary(id) and String.contains?(id, "|" <> pos <> "|"),
          do: {:ok, result(index, pos, id, :id_pos_only, source, one)},
          else: :error

      _ ->
        :error
    end
  end

  # -- :selected_exact (SentenceMeaning.provisional_selected_witness/4) ------
  defp do_witness(:selected_exact, si, index, pos, surface, source)
       when is_binary(surface) do
    selected =
      si
      |> choices(source, :strict)
      |> Enum.filter(fn
        %{token_index: ^index} -> true
        _ -> false
      end)

    with [choice] <- selected,
         id when is_binary(id) <- Map.get(choice, :id) || Map.get(choice, :chosen_id),
         [norm, ^pos, _sense] <- String.split(id, "|", parts: 3),
         true <- String.downcase(norm) == String.downcase(surface),
         true <- exact_cell?(si, id, pos, surface) do
      {:ok, result(index, pos, id, :selected_exact, source, choice)}
    else
      _ -> :error
    end
  end

  defp do_witness(:selected_exact, _si, _index, _pos, _surface, _source), do: :error

  # -- :selected_or_attested (ParticipantSpatial.selected_or_attested/4) -----
  defp do_witness(:selected_or_attested, si, index, pos, surface, source)
       when is_binary(surface) do
    picks =
      si
      |> choices(source, :loose)
      |> Enum.filter(&(get(&1, :token_index) == index))

    winners = winners_at(si, index)

    case {picks, winners} do
      {[], []} ->
        if attested_pos?(si, surface, pos),
          do: {:ok, result(index, pos, nil, :attested_grammar_role, source, nil)},
          else: :error

      {[pick], [winner]} ->
        with id when is_binary(id) <- get(pick, :chosen_id) || get(pick, :id),
             true <- String.contains?(id, "|" <> pos <> "|"),
             ^id <- get(winner, :id) || get(winner, :chosen_id) do
          {:ok, result(index, pos, id, :selected_or_attested, source, pick)}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp do_witness(:selected_or_attested, _si, _index, _pos, _surface, _source), do: :error

  @doc """
  ATL agreement for `[{token_index, sense_id}]`, in one of two existing modes.

    * `:winner_only` — `SVOEvent.atl_match?/2`: exactly one ATL winner at each
      index and its `id || chosen_id` equals the sense id.
    * `:winner_and_selection` — `SentenceMeaning.object_roles_agree_with_winners?/2`:
      exactly one ATL winner with `id == sense_id` AND exactly one selection
      (`semantic_choices` if a list, else `lifg_choices`) with
      `id || chosen_id == sense_id`.

  ATL winners are built from LIFG choices, so agreement is consistency, not
  independent evidence (COLLAB §34.6).
  """
  @spec atl_agrees?(map(), [{non_neg_integer(), String.t()}], :winner_only | :winner_and_selection) ::
          boolean()
  def atl_agrees?(si, role_ids, mode \\ :winner_only)

  def atl_agrees?(%{} = si, role_ids, :winner_only) when is_list(role_ids) do
    slate = get(si, :atl_slate)
    winners = if is_map(slate), do: get(slate, :winners), else: nil

    is_list(winners) and
      Enum.all?(role_ids, fn
        {index, sense_id} ->
          case Enum.filter(winners, &(get(&1, :token_index) == index)) do
            [winner] -> (get(winner, :id) || get(winner, :chosen_id)) == sense_id
            _ -> false
          end

        _ ->
          false
      end)
  end

  def atl_agrees?(%{} = si, role_ids, :winner_and_selection) when is_list(role_ids) do
    atl = Map.get(si, :atl_slate) || %{}
    winners = if is_map(atl), do: Map.get(atl, :winners), else: nil

    selections =
      case Map.get(si, :semantic_choices) do
        selected when is_list(selected) -> selected
        _ -> Map.get(si, :lifg_choices)
      end

    is_list(winners) and is_list(selections) and
      Enum.all?(role_ids, fn
        {index, sense_id} ->
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

  def atl_agrees?(_, _, _), do: false

  # -- helpers ---------------------------------------------------------------

  # :loose reads atom or string keys (SVOEvent / ParticipantSpatial get/2);
  # :strict reads atom keys only (SentenceMeaning Map.get/3).
  # Same fallback as SentenceMeaning.object_roles_agree_with_winners?/2: an
  # absent or nil :semantic_choices falls back to :lifg_choices.
  defp choices(si, :semantic_preferred, mode) do
    case get(si, :semantic_choices) do
      list when is_list(list) -> list
      _ -> si |> fetch(:lifg_choices, mode) |> List.wrap()
    end
  end

  defp choices(si, :lifg, mode), do: si |> fetch(:lifg_choices, mode) |> List.wrap()

  defp fetch(si, key, :loose), do: get(si, key)
  defp fetch(si, key, :strict), do: Map.get(si, key, [])

  defp winners_at(si, index) do
    si
    |> get(:atl_slate)
    |> get(:winners)
    |> List.wrap()
    |> Enum.filter(&(get(&1, :token_index) == index))
  end

  defp exact_cell?(si, id, pos, surface) do
    si
    |> Map.get(:active_cells)
    |> List.wrap()
    |> Enum.any?(fn
      %{id: ^id, pos: ^pos, norm: cell_norm} when is_binary(cell_norm) ->
        String.downcase(cell_norm) == String.downcase(surface)

      _ ->
        false
    end)
  end

  defp attested_pos?(si, surface, pos) do
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

  defp result(index, pos, id, basis, source, choice) do
    %{
      token_index: index,
      pos: pos,
      selected_id: id,
      basis: basis,
      choice_source: source,
      choice: choice
    }
  end

  defp clean(text) when is_binary(text),
    do: text |> String.trim() |> String.trim_trailing(".") |> String.downcase()

  defp get(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil
end
