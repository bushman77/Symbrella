defmodule Brain.LIFG.GrammarEvidence do
  @moduledoc """
  Candidate-specific compatibility between Core sentence signatures and
  dictionary sense metadata. Pure: does not select winners or mutate input.

  `apply/4` is opt-in. A weight of zero returns scores unchanged.
  `:have_verb_to_infinitive` alone does not imply obligation; obligation
  evidence must also be present on the *specific dictionary sense*.
  """

  @type context :: %{signatures: [map()], cells_by_id: map()}

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

    %{
      signatures: si |> get(:sentence_signatures, []) |> as_list(),
      cells_by_id: cells
    }
  end

  def prepare(_), do: %{signatures: [], cells_by_id: %{}}

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

  defp role_support(:verb_particle_construction, :particle), do: 0.18
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
  defp matches_pos?(:pos_determiner, "determiner"), do: true
  defp matches_pos?(:pos_preposition, "preposition"), do: true
  defp matches_pos?(:pos_adjective, "adjective"), do: true
  defp matches_pos?(_, _), do: false

  defp support_for(_, _, _, _), do: 0.0

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
