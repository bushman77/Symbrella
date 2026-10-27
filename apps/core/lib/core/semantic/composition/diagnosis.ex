defmodule Core.Semantic.Composition.Diagnosis do
  @moduledoc """
  Read-only, bounded explanation of lexical-role obstacles to event composition.

  Reports the selected LIFG POS and whether a compatible, separately attested
  POS alternative exists. It never replaces a lexical winner or authorizes
  a derivation. No dictionary definitions or user text enter telemetry.
  """

  alias Core.Semantic.Composition.RoleWitness

  @event_constructions [
    :progressive_directional_candidate,
    :progressive_locative_candidate
  ]

  @roles [
    {:subject_determiner, ["determiner"]},
    {:subject_head, ["noun"]},
    {:auxiliary, ["auxiliary", "verb"]},
    {:predicate, ["verb"]},
    {:preposition, ["preposition"]},
    {:location_determiner, ["determiner"]},
    {:location_head, ["noun"]}
  ]

  @doc "The exact V2 witness gate for every preferred event role, in deterministic order."
  @spec admission_audit(map()) :: [map()]
  def admission_audit(%{structural_competition:
        %{status: :preferred, preferred: %{construction: construction, roles: roles}}} = si)
      when construction in @event_constructions and is_map(roles) do
    Enum.map(@roles, fn {role, expected_pos} ->
      case Map.fetch(roles, role) do
        {:ok, index} when is_integer(index) and index >= 0 ->
          RoleWitness.audit(si, role, index, expected_pos)
        _ ->
          %{role: role, status: :rejected, reason: :missing_role}
      end
    end)
  end

  def admission_audit(_), do: []

  @spec blocked_roles(map()) :: [map()]
  def blocked_roles(%{structural_competition:
        %{status: :preferred, preferred: %{construction: construction, roles: roles}}} = si)
      when construction in @event_constructions and is_map(roles) do
    Enum.flat_map(@roles, fn {role, expected_pos} ->
      case Map.fetch(roles, role) do
        {:ok, index} when is_integer(index) and index >= 0 ->
          check_role(si, role, index, expected_pos)

        _ ->
          [%{role: role, token_index: nil, reason: :missing_role,
             selected_pos: nil, expected_pos: expected_pos,
             compatible_dictionary_pos?: false}]
      end
    end)
  end

  def blocked_roles(_), do: []

  defp check_role(si, role, index, expected) do
    choices =
      si
      |> Map.get(:lifg_choices, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :token_index) == index))

    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :index) == index and
        Map.get(&1, :n, 1) == 1))

    with [choice] <- choices,
         [token] <- tokens,
         word when is_binary(word) <- Map.get(token, :phrase),
         word <- normalize(word),
         id when is_binary(id) <- Map.get(choice, :id) || Map.get(choice, :chosen_id),
         {:ok, selected_pos} <- id_pos(id) do
      alternatives? = compatible_dictionary_pos?(si, word, expected)

      reason =
        cond do
          selected_pos not in expected -> :selected_pos_conflict
          not exact_witness?(si, word, id, selected_pos) ->
            :selected_sense_not_attested
          true -> :ok
        end

      if reason == :ok do
        []
      else
        [%{
          role: role, token_index: index, reason: reason,
          selected_pos: selected_pos, expected_pos: expected,
          compatible_dictionary_pos?: alternatives?
        }]
      end
    else
      _ ->
        [%{
          role: role, token_index: index, reason: :missing_role_evidence,
          selected_pos: nil, expected_pos: expected,
          compatible_dictionary_pos?: false
        }]
    end
  end

  defp compatible_dictionary_pos?(si, word, expected) do
    Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
      %{} = cell ->
        normalize(Map.get(cell, :norm)) == word and
          Map.get(cell, :pos) in expected and is_binary(Map.get(cell, :id))
      _ -> false
    end)
  end

  defp exact_witness?(si, word, id, pos) do
    Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
      %{} = cell ->
        Map.get(cell, :id) == id and Map.get(cell, :pos) == pos and
          normalize(Map.get(cell, :norm)) == word and
          String.starts_with?(id, word <> "|" <> pos <> "|")
      _ -> false
    end)
  end

  defp id_pos(id) do
    case String.split(id, "|", parts: 3) do
      [_word, pos, _sense] when pos != "" -> {:ok, pos}
      _ -> :error
    end
  end

  defp normalize(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp normalize(_), do: ""
end
