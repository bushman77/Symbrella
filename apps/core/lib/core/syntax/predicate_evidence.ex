defmodule Core.Syntax.PredicateEvidence do
  @moduledoc """
  Read-only, pre-LIFG evidence for an existing verb POS in a predicate slot.

  This is deliberately not an event extractor or word-sense adjudicator.
  A grammatical surface pattern AND an exact-surface dictionary verb witness
  are required. The caller must resolve conflicts with other role proposals.
  No candidate IDs, lexical meanings, or factual assertions are generated.
  """

  @modals MapSet.new(~w(can could may might must shall should will would do does did))
  @pronouns MapSet.new(~w(i you we they he she it))
  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @wh MapSet.new(~w(what when where why how which who))
  @modifiers MapSet.new(~w(please not))
  @required_present [:contiguous_surface, :attested_nominal_subject,
                     :attested_nominal_object, :attested_finite_present_verb]
  @max_tokens 64
  @max_proposals 32

  @spec detect(map()) :: [map()]
  def detect(%{} = si) do
    nodes = base_nodes(Map.get(si, :tokens, []))
    cells = List.wrap(Map.get(si, :active_cells, [])) |> Enum.filter(&is_map/1)

    if length(nodes) in 2..@max_tokens and
         contiguous?(nodes) and length(cells) <= 4096 do
      tokens = Map.new(nodes, &{&1.index, &1.word})
      max_idx = length(nodes) - 1

      (modal_questions(tokens, cells, max_idx) ++
         modal_statements(tokens, cells, max_idx) ++
         imperatives(tokens, cells, max_idx) ++
         plural_present(tokens, cells, max_idx) ++
         attested_present(si, tokens, cells, max_idx))
      |> Enum.uniq_by(&{&1.token_index, &1.basis, &1.span})
      |> Enum.take(@max_proposals)
    else
      []
    end
  end

  def detect(_), do: []

  defp modal_questions(tokens, cells, last) do
    starts =
      cond do
        MapSet.member?(@modals, Map.get(tokens, 0)) -> [0]
        MapSet.member?(@wh, Map.get(tokens, 0)) and
            MapSet.member?(@modals, Map.get(tokens, 1)) -> [1]
        true -> []
      end

    Enum.flat_map(starts, fn aux ->
      subject = aux + 1
      candidates =
        [{subject + 1, pronoun?(tokens, cells, subject)},
         {subject + 2, determined_noun?(tokens, cells, subject)}]

      Enum.flat_map(candidates, fn {start, subject_ok?} ->
        predicate = skip_modifiers(tokens, start, last)

        if subject_ok? and predicate <= last and verb?(tokens, cells, predicate) do
          [proposal(predicate, :modal_question_predicate, {0, last},
             [:auxiliary_subject_order, :dictionary_pos_witness, :bounded_predicate_slot])]
        else
          []
        end
      end)
    end)
  end

  defp modal_statements(tokens, cells, last) do
    Enum.flat_map(0..last, fn aux ->
      if MapSet.member?(@modals, Map.get(tokens, aux)) do
        subject_ok? =
          (aux >= 1 and pronoun?(tokens, cells, aux - 1)) or
            (aux >= 2 and determined_noun?(tokens, cells, aux - 2))
        predicate = skip_modifiers(tokens, aux + 1, last)

        if subject_ok? and predicate <= last and verb?(tokens, cells, predicate) do
          [proposal(predicate, :modal_declarative_predicate, {0, last},
             [:subject_auxiliary_order, :dictionary_pos_witness, :bounded_predicate_slot])]
        else
          []
        end
      else
        []
      end
    end)
  end

  defp imperatives(tokens, cells, last) do
    start = if Map.get(tokens, 0) == "please", do: 1, else: 0
    after_verb = Map.get(tokens, start + 1)

    if start < last and
         not MapSet.member?(@modals, Map.get(tokens, start)) and
         verb?(tokens, cells, start) and
         (MapSet.member?(@determiners, after_verb) or
            after_verb in ~w(me him her us them it)) do
      [proposal(start, :imperative_predicate, {0, last},
         [:initial_predicate, :dictionary_pos_witness, :bounded_predicate_slot])]
    else
      []
    end
  end

  # Explicit plural morphology prevents a surface-only guess that a base
  # form is finite present. This grants POS support, NOT episodic SVO admission.
  defp plural_present(tokens, cells, last) do
    with true <- last in [3, 4],
         true <- MapSet.member?(@determiners, Map.get(tokens, 0)),
         true <- witnessed?(tokens, cells, 0, "determiner"),
         true <- plural_noun?(tokens, cells, 1),
         true <- present_base_verb?(tokens, cells, 2),
         true <- (last == 3 and witnessed?(tokens, cells, 3, "noun")) or
           (last == 4 and MapSet.member?(@determiners, Map.get(tokens, 3)) and
             witnessed?(tokens, cells, 3, "determiner") and
             witnessed?(tokens, cells, 4, "noun")) do
      [proposal(2, :plural_present_predicate, {0, last},
         [:attested_plural_subject, :present_base_verb_witness,
          :dictionary_pos_witness, :bounded_predicate_slot])]
    else
      _ -> []
    end
  end

  defp attested_present(si, tokens, cells, last) do
    si
    |> Map.get(:sentence_signatures, [])
    |> List.wrap()
    |> Enum.flat_map(fn
      %{id: :finite_present_svo_candidate, source: :core_syntax,
        span: {0, ^last},
        roles: %{subject_head: subject, predicate: predicate, object_head: object},
        evidence: evidence}
      when is_integer(subject) and is_integer(predicate) and is_integer(object) and
           is_list(evidence) ->
        if subject < predicate and predicate < object and
             Enum.all?(@required_present, &(&1 in evidence)) and
             verb?(tokens, cells, predicate) do
          [proposal(predicate, :finite_present_svo_predicate, {0, last},
             [:attested_finite_present_verb, :dictionary_pos_witness,
              :bounded_predicate_slot])]
        else
          []
        end

      _ -> []
    end)
  end

  defp proposal(index, basis, span, evidence) do
    %{token_index: index, pos: "verb", role: :predicate,
      frame: :predicate_clause, span: span, basis: basis, evidence: evidence}
  end

  defp skip_modifiers(tokens, start, last) do
    Enum.reduce(1..2, start, fn _, idx ->
      if idx <= last and MapSet.member?(@modifiers, Map.get(tokens, idx)),
        do: idx + 1, else: idx
    end)
  end

  defp pronoun?(tokens, cells, i) do
    MapSet.member?(@pronouns, Map.get(tokens, i)) and
      witnessed?(tokens, cells, i, "pronoun")
  end

  defp determined_noun?(tokens, cells, det) do
    MapSet.member?(@determiners, Map.get(tokens, det)) and
      witnessed?(tokens, cells, det, "determiner") and
      witnessed?(tokens, cells, det + 1, "noun")
  end

  defp verb?(tokens, cells, i), do: witnessed?(tokens, cells, i, "verb")

  defp plural_noun?(tokens, cells, index) do
    word = Map.get(tokens, index)
    Enum.any?(cells, fn cell ->
      tags = List.wrap(Map.get(cell, :semantic_atoms, []))
      cell_matches?(cell, word, "noun") and
        "tag:plural" in tags and "tag:form-of" in tags
    end)
  end

  defp present_base_verb?(tokens, cells, index) do
    word = Map.get(tokens, index)
    Enum.any?(cells, fn cell ->
      tags = List.wrap(Map.get(cell, :semantic_atoms, []))
      cell_matches?(cell, word, "verb") and
        not Enum.any?(["tag:past", "tag:participle"], &(&1 in tags)) and
        not ("tag:form-of" in tags)
    end)
  end

  defp witnessed?(tokens, cells, index, pos) do
    word = Map.get(tokens, index)
    is_binary(word) and
      Enum.any?(cells, &cell_matches?(&1, word, pos))
  end

  defp cell_matches?(cell, word, pos) when is_binary(word) do
    normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == word and
      normalize(Map.get(cell, :pos)) == pos and
      case Map.get(cell, :id) do
        id when is_binary(id) ->
          String.starts_with?(id, word <> "|" <> pos <> "|")
        _ -> false
      end
  end

  defp cell_matches?(_, _, _), do: false

  defp base_nodes(tokens) when is_list(tokens) do
    tokens
    |> Enum.filter(fn token ->
      is_map(token) and Map.get(token, :n, 1) == 1 and
        Map.get(token, :kind) not in [:punct, "punct"]
    end)
    |> Enum.map(fn token ->
      %{index: Map.get(token, :index), word: normalize(Map.get(token, :phrase))}
    end)
    |> Enum.sort_by(& &1.index)
  end

  defp base_nodes(_), do: []

  defp contiguous?(nodes) do
    Enum.map(nodes, & &1.index) == Enum.to_list(0..(length(nodes) - 1)) and
      Enum.all?(nodes, &(is_binary(&1.word) and &1.word != ""))
  end

  defp normalize(word) when is_binary(word) do
    word
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end

  defp normalize(_), do: nil
end
