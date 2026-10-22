defmodule Core.Social.ReportedProposition do
  @moduledoc """
  A deliberately bounded, read-only declarative-composition pilot for Colony.

  Recognizes one counted-link construction with a copular predicate and
  a restrictive relative clause ("N of my checker's M links were lines that
  refute the claim"). It requires a matching surface, retrieved dictionary
  POS for the key roles, attested noun-phrase signatures, and matching ATL
  lexical winners. Merely observing a verb in the title is not understanding.

  The result is always attributed to the external reporter, not verified,
  persisted, taught to the brain or submitted to live neuron processes.
  Unsupported or ambiguous inputs explicitly abstain.
  """

  @max_chars 420
  @pattern ~r/\A(?:[^\n:]{1,120}:\s*)?(?<count>\d{1,3})\s+of\s+(?<possessive>(?:my|the)\s+checker['’]s)\s+(?<total>\d{1,3})\s+(?<subject>links)\s+(?<copula>were)\s+(?<classification>lines)\s+(?<relative>that)\s+(?<predicate>refute|support)\s+(?<article>the)\s+(?<object>claim)(?=$|[.!?](?:\s|$))/iu

  @doc """
  Return a bounded, attributed proposition or an explicit abstention.
  The caller must supply an SI after LTM, Syntax, LIFG and pure ATL reduction.
  """
  @spec interpret(map()) :: map()
  def interpret(%{source: :colony_external, sentence: sentence} = si)
      when is_binary(sentence) do
    text = String.slice(sentence, 0, @max_chars)

    with captures when is_map(captures) <- Regex.named_captures(@pattern, text),
         {:ok, count, total} <- counts(captures),
         {:ok, positions} <- roles(si, captures, count, total),
         :ok <- dictionary_roles(si, positions),
         :ok <- phrase_signatures(si, positions),
         :ok <- selected_roles(si, positions) do
      relation =
        case String.downcase(captures["predicate"]) do
          "refute" -> :reported_refutation
          "support" -> :reported_support
        end

      %{
        status: :attributed_structural_interpretation,
        kind: :reported_declarative_proposition,
        source: :colony_external,
        trust: :untrusted_external,
        verification: :author_claim_not_independently_verified,
        relation: :copular_classification,
        embedded_relation: relation,
        subject: :reported_checker_links,
        complement: :lines,
        object: :the_claim,
        reported_count: count,
        reported_total: total,
        possessor:
          if(String.starts_with?(String.downcase(captures["possessive"]), "my "),
            do: :speaker_reported_not_verified,
            else: :external_checker_owner_not_verified),
        grammar: [:counted_nominal_subject, :copular_were, :restrictive_relative_clause],
        evidence: [:exact_bounded_surface, :dictionary_pos, :noun_phrase_signatures,
                   :lifg_atl_lexical_agreement],
        # Indices are non-semantic evidence pointers, not raw text or BrainCells.
        role_indices: Map.take(positions, [:checker, :links, :were, :lines, :relative_verb, :claim])
      }
    else
      # Extend to bounded declarative families only when the legacy counted
      # construction does not match. A *matched* but unsupported headline
      # must still abstain instead of falling through to a looser parser.
      nil -> Core.Social.DeclarativeProposition.interpret(si)
      {:error, :lifg_atl_role_disagreement, role_audit} ->
        Map.put(abstain(:lifg_atl_role_disagreement), :role_audit, role_audit)

      {:error, reason} -> abstain(reason)
      :error -> abstain(:insufficient_role_evidence)
      _ -> abstain(:insufficient_role_evidence)
    end
  end

  def interpret(_), do: abstain(:untrusted_or_invalid_input)

  defp abstain(reason),
    do: %{status: :not_interpreted, reason: reason, verification: :external_unverified}

  defp counts(%{"count" => count, "total" => total}) do
    with {n, ""} <- Integer.parse(count),
         {t, ""} <- Integer.parse(total),
         true <- n > 0 and n <= t and t <= 999 do
      {:ok, n, t}
    else
      _ -> {:error, :invalid_reported_counts}
    end
  end

  defp roles(si, captures, count, total) do
    # The canonical tokenizer may separate checker + apostrophe + s, while
    # keeping non-contiguous primitive token indices. Locate by ordered word
    # surfaces rather than assuming adjacent token indices.
    possessive = captures["possessive"] |> String.downcase()
    determiner = if String.starts_with?(possessive, "my "), do: "my", else: "the"
    predicate = captures["predicate"] |> String.downcase()

    targets = [
      {:count, Integer.to_string(count)}, {:of, "of"}, {:determiner, determiner},
      {:checker, "checker"}, {:total, Integer.to_string(total)}, {:links, "links"},
      {:were, "were"}, {:lines, "lines"}, {:that, "that"},
      {:relative_verb, predicate}, {:the, "the"}, {:claim, "claim"}
    ]

    tokens =
      si
      |> Map.get(:tokens, [])
      |> Enum.filter(fn token ->
        is_map(token) and Map.get(token, :kind, :word) not in [:punct, "punct"] and
          Map.get(token, :n, 1) == 1
      end)

    Enum.reduce_while(targets, {:ok, %{}, tokens}, fn {role, surface}, {:ok, found, remaining} ->
      case Enum.split_while(remaining, fn token ->
             normalize(Map.get(token, :phrase)) != surface
           end) do
        {_skipped, [token | after_token]} ->
          case Map.get(token, :index) do
            index when is_integer(index) and index >= 0 ->
              {:cont, {:ok, Map.put(found, role, index), after_token}}

            _ ->
              {:halt, {:error, :missing_token_indices}}
          end

        _ ->
          {:halt, {:error, :token_surface_mismatch}}
      end
    end)
    |> case do
      {:ok, indices, _remaining} -> {:ok, indices}
      {:error, reason} -> {:error, reason}
    end
  end

  defp dictionary_roles(si, indices) do
    required = [
      {:checker, "noun"}, {:links, "noun"}, {:were, "verb"},
      {:lines, "noun"}, {:relative_verb, "verb"}, {:claim, "noun"}
    ]

    cells = Map.get(si, :active_cells, [])

    if Enum.all?(required, fn {role, pos} ->
         Enum.any?(cells, fn cell ->
           is_map(cell) and Map.get(cell, :token_index) == Map.fetch!(indices, role) and
             Map.get(cell, :pos) == pos and
             is_binary(Map.get(cell, :id))
         end)
       end) do
      :ok
    else
      {:error, :missing_attested_dictionary_roles}
    end
  end

  defp phrase_signatures(si, indices) do
    signatures = Map.get(si, :sentence_signatures, [])
    valid_head = fn determiner, head ->
      Enum.any?(signatures, fn frame ->
        Map.get(frame, :id) == :noun_phrase and
          get_in(frame, [:roles, :determiner]) == Map.fetch!(indices, determiner) and
          get_in(frame, [:roles, :head]) == Map.fetch!(indices, head)
      end)
    end

    if valid_head.(:determiner, :checker) and valid_head.(:the, :claim) do
      :ok
    else
      {:error, :missing_nominal_signature_agreement}
    end
  end

  defp selected_roles(si, indices) do
    slate = Map.get(si, :atl_slate, %{})
    winners = Map.get(slate, :winners, [])
    cells = Map.get(si, :active_cells, [])
    required = [
      {:checker, "noun"}, {:links, "noun"}, {:were, "verb_or_auxiliary"},
      {:lines, "noun"}, {:relative_verb, "verb"}, {:claim, "noun"}
    ]

    # A copular be-form is a grammatical auxiliary even when the dictionary
    # also has a "verb" entry. The built-in "were|auxiliary|0" sense is not a
    # retrieved BrainCell; accept it only with *independent* surface, Core
    # syntax and dictionary verb witnesses. Other roles still require exact
    # agreement between the selected ID and a retrieved dictionary row.
    audit =
      Enum.map(required, fn {role, expected} ->
        index = Map.fetch!(indices, role)

        selected =
          Enum.filter(winners, fn winner ->
            Map.get(winner, :token_index) == index
          end)

        allowed_pos =
          if expected == "verb_or_auxiliary", do: ["verb", "auxiliary"], else: [expected]

        attested? = fn id ->
          pos = selected_pos(id)
          Enum.any?(cells, fn cell ->
            is_map(cell) and Map.get(cell, :token_index) == index and
              Map.get(cell, :id) == id and Map.get(cell, :pos) == pos
          end)
        end

        matched =
          Enum.filter(selected, fn winner ->
            id = Map.get(winner, :id)

            selected_pos(id) in allowed_pos and
              (attested?.(id) or
                 (role == :were and copular_auxiliary_witness?(si, indices, id)))
          end)

        %{
          role: role,
          token_index: index,
          expected_pos: expected,
          selected_pos:
            selected
            |> Enum.map(&selected_pos(Map.get(&1, :id)))
            |> Enum.reject(&is_nil/1)
            |> Enum.uniq()
            |> Enum.take(3),
          selected_senses:
            selected
            |> Enum.map(&Map.get(&1, :id))
            |> Enum.filter(&is_binary/1)
            |> Enum.map(&String.slice(&1, 0, 96))
            |> Enum.take(3),
          dictionary_pos_available?: Enum.any?(cells, fn cell ->
            is_map(cell) and Map.get(cell, :token_index) == index and
              Map.get(cell, :pos) in allowed_pos
          end),
          selected_count: length(selected),
          agreed?: length(matched) == 1 and length(selected) == 1
        }
      end)

    if Enum.all?(audit, & &1.agreed?) do
      :ok
    else
      {:error, :lifg_atl_role_disagreement, audit}
    end
  end

  # The closed-class candidate is not claimed to be a retrieved dictionary
  # sense. Its grammatical be-function is supported by a complete Core frame,
  # the exact token surface, and an independently attested lexical be-form.
  defp copular_auxiliary_witness?(si, indices, "were|auxiliary|0") do
    index = Map.fetch!(indices, :were)

    surface? =
      Enum.any?(Map.get(si, :tokens, []), fn token ->
        is_map(token) and Map.get(token, :index) == index and
          normalize(Map.get(token, :phrase)) == "were"
      end)

    dictionary? =
      Enum.any?(Map.get(si, :active_cells, []), fn cell ->
        is_map(cell) and Map.get(cell, :token_index) == index and
          Map.get(cell, :pos) == "verb" and
          normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == "were" and
          is_binary(Map.get(cell, :id))
      end)

    grammar? =
      Enum.any?(Map.get(si, :sentence_signatures, []), fn signature ->
        Map.get(signature, :id) == :copular_relative_classification and
          Map.get(signature, :source) == :core_syntax and
          get_in(signature, [:roles, :copula]) == index and
          get_in(signature, [:roles, :complement_head]) ==
            Map.fetch!(indices, :lines)
      end)

    surface? and dictionary? and grammar?
  end

  defp copular_auxiliary_witness?(_, _, _), do: false

  defp selected_pos(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [_lemma, pos, suffix] when suffix != "fallback" -> pos
      _ -> nil
    end
  end

  defp selected_pos(_), do: nil

  defp normalize(value) when is_binary(value),
    do: value |> String.trim() |> String.downcase()

  defp normalize(_), do: ""
end
