defmodule Brain.LIFG.StructuralCompetition do
  @moduledoc """
  Read-only, sentence-level competition between independently observed
  grammatical constructions.

  Core.Syntax supplies the sentence signatures and their lexical/morphological
  witnesses. LIFG supplies the selected lexical alternatives. A structurally
  complete reading may remain preferable even when a local lexical winner
  disagrees with one of its grammatical roles.

  This module does not reassign dictionary senses, assert facts, admit items to
  working memory, or modify the supplied SemanticInput.
  """

  @max_signatures 64
  @max_candidates 12
  @max_tokens 48
  @min_preference 0.68
  @min_margin 0.12

  @constituent_ids MapSet.new([
    :noun_phrase,
    :prepositional_phrase,
    :possessive_noun_phrase,
    :verb_particle_construction
  ])

  @type outcome :: map()

  @spec resolve(map()) :: outcome()
  def resolve(%{} = si) do
    indices = base_indices(Map.get(si, :tokens, []))
    last = List.last(indices)

    candidates =
      if is_integer(last) and last < @max_tokens and
           indices == Enum.to_list(0..last) do
        signatures = si |> Map.get(:sentence_signatures, []) |> List.wrap()
        choices = si |> Map.get(:lifg_choices, []) |> List.wrap()

        signatures
        |> Enum.take(@max_signatures)
        |> Enum.filter(&complete_clause?(&1, last))
        |> Enum.uniq_by(&{&1.id, &1.span, &1.roles})
        |> Enum.take(@max_candidates)
        |> Enum.map(&assess(&1, signatures, choices))
        |> Enum.sort_by(fn c -> {-c.compatibility, Atom.to_string(c.construction)} end)
      else
        []
      end

    {status, preferred, margin} = adjudicate(candidates)

    %{
      version: 1,
      source: :sentence_signatures_and_lifg,
      status: status,
      preferred: preferred,
      candidates: candidates,
      discrimination_margin: margin,
      factual_authority?: false
    }
  end

  def resolve(_), do: %{
    version: 1,
    source: :invalid_input,
    status: :no_complete_candidate,
    preferred: nil,
    candidates: [],
    discrimination_margin: nil,
    factual_authority?: false
  }

  defp base_indices(tokens) when is_list(tokens) do
    tokens
    |> Enum.filter(fn
      %{index: index, phrase: phrase} = token
      when is_integer(index) and index >= 0 and is_binary(phrase) ->
        Map.get(token, :n, 1) == 1 and
          Map.get(token, :kind) not in [:punct, "punct"]
      _ -> false
    end)
    |> Enum.map(& &1.index)
    |> Enum.sort()
  end

  defp base_indices(_), do: []

  defp complete_clause?(
         %{id: id, source: :core_syntax, span: {0, span_last},
           roles: roles, evidence: evidence}, last
       )
       when is_atom(id) and is_map(roles) and is_list(evidence) do
    span_last == last and not MapSet.member?(@constituent_ids, id) and
      evidence != [] and map_size(roles) >= 2 and
      Enum.all?(Map.values(roles), &(is_integer(&1) and &1 >= 0 and &1 <= last)) and
      Enum.any?([:predicate, :verb], &Map.has_key?(roles, &1)) and
      Enum.any?([:subject, :subject_head, :wh, :object_head, :lexical_target],
        &Map.has_key?(roles, &1))
  end

  defp complete_clause?(_, _), do: false

  defp assess(signature, signatures, choices) do
    comparisons =
      signature
      |> Map.get(:required_features, %{})
      |> Enum.flat_map(fn {role, features} ->
        index = Map.get(signature.roles, role)
        permitted = expected_pos(features)

        if is_integer(index) and permitted != [] do
          selected =
            Enum.find(choices, fn choice ->
              is_map(choice) and Map.get(choice, :token_index) == index
            end)

          actual =
            case selected do
              %{} = choice ->
                choice
                |> Map.get(:id, Map.get(choice, :chosen_id))
                |> id_pos()
              _ -> nil
            end

          [%{role: role, token_index: index, expected_pos: permitted,
             selected_pos: actual,
             status: cond do
               is_nil(actual) -> :unresolved
               actual in permitted -> :compatible
               true -> :conflict
             end}]
        else
          []
        end
      end)
      |> Enum.sort_by(& &1.token_index)

    witnessed = Enum.count(comparisons, &(&1.status == :compatible))
    conflicts = Enum.count(comparisons, &(&1.status == :conflict))
    checked = length(comparisons)
    lexical_fit = if checked == 0, do: 0.0, else: witnessed / checked

    constituents =
      signatures
      |> Enum.filter(fn
        %{id: id, span: {first, last}} ->
          MapSet.member?(@constituent_ids, id) and
            first >= elem(signature.span, 0) and last <= elem(signature.span, 1)
        _ -> false
      end)
      |> Enum.map(&%{construction: &1.id, span: &1.span})
      |> Enum.uniq()
      |> Enum.take(12)

    %{
      construction: signature.id,
      span: signature.span,
      roles: signature.roles,
      evidence: signature.evidence,
      constituents: constituents,
      lexical_role_evidence: comparisons,
      matched_roles: witnessed,
      conflicting_roles: conflicts,
      unresolved_roles: checked - witnessed - conflicts,
      compatibility: Float.round(0.72 + 0.28 * lexical_fit, 4),
      factual_authority?: false
    }
  end

  defp adjudicate([]), do: {:no_complete_candidate, nil, nil}

  defp adjudicate([first | rest]) do
    margin =
      case rest do
        [second | _] -> Float.round(first.compatibility - second.compatibility, 4)
        [] -> nil
      end

    enough_margin? = is_nil(margin) or margin >= @min_margin

    # Require multiple independent lexical-role confirmations, and avoid
    # preferring a reading contradicted by more roles than support it.
    if first.matched_roles >= 2 and
         first.matched_roles >= first.conflicting_roles and
         first.compatibility >= @min_preference and enough_margin? do
      {:preferred, first, margin}
    else
      {:unresolved, nil, margin}
    end
  end

  defp expected_pos(features) when is_list(features) do
    features
    |> Enum.flat_map(fn
      :pos_noun -> ["noun"]
      :pos_common_noun -> ["noun"]
      :pos_noun_or_pronoun -> ["noun", "pronoun"]
      :pos_pronoun -> ["pronoun"]
      :pos_verb -> ["verb"]
      :pos_auxiliary -> ["auxiliary", "verb"]
      :pos_determiner -> ["determiner"]
      :pos_preposition -> ["preposition"]
      :pos_adjective -> ["adjective"]
      :present_participle -> ["verb"]
      :attested_present_participle -> ["verb"]
      _ -> []
    end)
    |> Enum.uniq()
  end

  defp expected_pos(_), do: []

  defp id_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_lemma, pos, _sense] -> pos
      _ -> nil
    end
  end

  defp id_pos(_), do: nil
end
