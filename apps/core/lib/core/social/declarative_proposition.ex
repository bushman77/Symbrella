defmodule Core.Social.DeclarativeProposition do
  @moduledoc """
  Bounded, source-attributed declarative-clause pilot for untrusted social text.

  This is not an open-ended English parser. It composes a small family of
  complete subject/predicate/object constructions from canonical token
  positions, attested Core syntax and exact LTM-to-ATL sense selections.
  It does not create dictionary senses, verify external claims, write memory,
  invoke live neurons, or generate text with an LLM.
  """

  @determiners ~w(a an the this that these those my your its their)
  @numbers %{
    "one" => 1, "two" => 2, "three" => 3, "four" => 4, "five" => 5,
    "six" => 6, "seven" => 7, "eight" => 8, "nine" => 9, "ten" => 10,
    "eleven" => 11, "twelve" => 12, "thirteen" => 13
  }

  @spec interpret(map()) :: map()
  def interpret(%{source: :colony_external, tokens: tokens, sentence: sentence} = si)
      when is_list(tokens) and is_binary(sentence) do
    nodes =
      tokens
      |> Enum.filter(fn t ->
        is_map(t) and Map.get(t, :n, 1) == 1 and
          Map.get(t, :kind, :word) not in [:punct, "punct"] and
          is_integer(Map.get(t, :index)) and is_binary(Map.get(t, :phrase))
      end)
      |> Enum.map(fn t ->
        %{index: t.index, word: normalize(t.phrase)}
      end)

    # Refuse titles, multiple sentences and text beyond the read-only budget.
    # Colon headings need a different parser; the prior counted-link pilot
    # remains responsible for its own exact title construction.
    if length(nodes) in 4..8 and String.length(sentence) <= 420 and
         not String.contains?(sentence, [":", ";", "\n"]) and
         simple_sentence?(sentence) and surface_tokens_agree?(sentence, nodes) do
      case parse(nodes) do
        {:ok, clause} ->
          with :ok <- attested_roles(si, clause.roles),
               :ok <- complete_grammar(si, clause),
               :ok <- selected_roles(si, clause.roles) do
            %{
              status: :attributed_structural_interpretation,
              kind: :reported_declarative_proposition,
              source: :colony_external,
              trust: :untrusted_external,
              verification: :author_claim_not_independently_verified,
              construction: clause.construction,
              subject: clause.subject,
              predicate: clause.predicate,
              object: clause.object,
              relation: clause.relation,
              polarity: clause.polarity,
              reported_count: clause.count,
              reported_total: nil,
              grammar: clause.grammar,
              evidence: [:bounded_original_surface, :attested_dictionary_roles,
                         :lifg_atl_lexical_agreement, :core_syntax_support],
              role_indices: Map.new(clause.roles, fn {role, index, _pos} -> {role, index} end)
            }
          else
            {:error, reason} -> abstain(reason)
          end

        {:error, reason} -> abstain(reason)
      end
    else
      abstain(:unsupported_surface)
    end
  end

  def interpret(_), do: abstain(:invalid_or_untrusted_input)

  defp simple_sentence?(sentence) do
    # A terminal full stop is fine; embedded sentences or quoted instructions
    # are not within this first compositional family.
    body = sentence |> String.trim() |> String.trim_trailing(".")
    not String.contains?(body, [".", "?", "!", <<96>>])
  end

  # All semantic roles must refer to the actual source surface, rather
  # than caller-supplied token identities that happen to pass POS checks.
  defp surface_tokens_agree?(sentence, nodes) do
    from_surface =
      sentence
      |> then(&Regex.scan(~r/[\p{L}\p{N}]+/u, &1))
      |> Enum.map(fn [word] -> String.downcase(word) end)

    from_tokens = Enum.map(nodes, & &1.word)

    from_surface == from_tokens and
      Enum.all?(from_tokens, &Regex.match?(~r/\A[\p{L}\p{N}]+\z/u, &1))
  end

  defp parse(nodes) do
    words = Enum.map(nodes, & &1.word)

    case words do
      [det, subject, verb, number, object] when det in @determiners ->
        with {:ok, count} <- number(number),
             :ok <- supported_verb(verb) do
          clause(
            :counted_object_action, subject, verb, object,
            :reported_action, :positive, count,
            [noun_role(:subject, nodes, 1), verb_role(nodes, 2),
             noun_role(:object, nodes, 4)],
            [{0, 1}],
            [:determined_subject, :past_action, :counted_object]
          )
        end

      [number, subject, verb, det, _modifier, object] when det in @determiners ->
        with {:ok, count} <- number(number),
             :ok <- supported_verb(verb),
             {:ok, relation} <- evidence_relation(verb) do
          clause(
            :counted_transitive, subject, verb, object,
            relation, :positive, count,
            [noun_role(:subject, nodes, 1), verb_role(nodes, 2),
             {:modifier, Enum.at(nodes, 4).index, "adjective"},
             noun_role(:object, nodes, 5)],
            [{3, 5}],
            [:counted_subject, :past_action, :modified_object]
          )
        end

      [number, subject, verb, det, object] when det in @determiners ->
        with {:ok, count} <- number(number),
             :ok <- supported_verb(verb),
             {:ok, relation} <- evidence_relation(verb) do
          clause(
            :counted_transitive, subject, verb, object,
            relation, :positive, count,
            [noun_role(:subject, nodes, 1), verb_role(nodes, 2),
             noun_role(:object, nodes, 4)],
            [{3, 4}],
            [:counted_subject, :past_action, :determined_object]
          )
        end

      [det, subject, "did", "not", verb, object_det, object]
      when det in @determiners and object_det in @determiners ->
        with :ok <- supported_verb(verb) do
          clause(
            :explicit_negated_transitive, subject, verb, object,
            :reported_action, :negative, nil,
            [noun_role(:subject, nodes, 1),
             verb_role(nodes, 4), noun_role(:object, nodes, 6),
             {:auxiliary, Enum.at(nodes, 2).index, "verb"},
             {:negator, Enum.at(nodes, 3).index, "adverb"}],
            [{0, 1}, {5, 6}],
            [:determined_subject, :did_not_negation, :base_predicate, :determined_object]
          )
        end

      _ ->
        {:error, :unsupported_surface}
    end
  end

  defp clause(construction, subject, predicate, object, relation,
              polarity, count, roles, noun_phrases, grammar) do
    {:ok, %{
      construction: construction, subject: subject, predicate: predicate,
      object: object, relation: relation, polarity: polarity, count: count,
      roles: roles, noun_phrases: noun_phrases, grammar: grammar
    }}
  end

  defp noun_role(role, nodes, at), do: {role, Enum.at(nodes, at).index, "noun"}
  defp verb_role(nodes, at), do: {:predicate, Enum.at(nodes, at).index, "verb"}

  defp number(word) do
    case Map.get(@numbers, word) do
      n when is_integer(n) -> {:ok, n}
      _ ->
        case Integer.parse(word) do
          {n, ""} when n in 1..999 -> {:ok, n}
          _ -> {:error, :unsupported_reported_number}
        end
    end
  end

  defp supported_verb(word) when is_binary(word) do
    # Generic action verbs can be represented by their source surface, but
    # this first family only admits explicit, previously scoped predicates.
    if word in ~w(produced contradicted refuted supported verify),
      do: :ok, else: {:error, :unsupported_predicate}
  end

  defp evidence_relation(verb) when verb in ~w(contradicted refuted),
    do: {:ok, :reported_refutation}
  defp evidence_relation("supported"), do: {:ok, :reported_support}
  defp evidence_relation(_), do: {:error, :unsupported_evidence_relation}

  defp attested_roles(si, roles) do
    cells = Map.get(si, :active_cells, [])

    if Enum.all?(roles, fn {role, index, pos} ->
         Enum.any?(cells, fn cell ->
           if is_map(cell) do
             available_pos = Map.get(cell, :pos)
             valid_pos? =
               available_pos == pos or
                 (role == :auxiliary and available_pos in ["verb", "auxiliary"])

             Map.get(cell, :token_index) == index and
               valid_pos? and is_binary(Map.get(cell, :id))
           else
             false
           end
         end)
       end) do
      :ok
    else
      {:error, :missing_attested_dictionary_roles}
    end
  end

  defp selected_roles(si, roles) do
    winners = get_in(si, [:atl_slate, :winners]) || []
    cells = Map.get(si, :active_cells, [])

    if Enum.all?(roles, fn {role, index, pos} ->
         selected = Enum.filter(winners, &(Map.get(&1, :token_index) == index))

         length(selected) == 1 and
           (Enum.any?(cells, fn cell ->
              id = Map.get(hd(selected), :id)
              is_binary(id) and Map.get(cell, :id) == id and
                Map.get(cell, :token_index) == index and
                Map.get(cell, :pos) == pos
            end) or
              (role == :auxiliary and
                 did_auxiliary_witness?(si, index, Map.get(hd(selected), :id))))
       end) do
      :ok
    else
      {:error, :lifg_atl_role_disagreement}
    end
  end

  # "did" can be a closed-class auxiliary selected by Stage1, rather than
  # a literal dictionary verb sense. Accept precisely this grammatical
  # function with independently attested did/not morphology and a negative
  # surface; no new BrainCell or certainty about the external claim is made.
  defp did_auxiliary_witness?(si, index, "did|auxiliary|0") do
    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :active_cells, [])

    surface? =
      Enum.any?(tokens, fn token ->
        Map.get(token, :index) == index and
          normalize(Map.get(token, :phrase)) == "did"
      end)

    next_not? =
      Enum.any?(tokens, fn token ->
        Map.get(token, :index) == index + 1 and
          normalize(Map.get(token, :phrase)) == "not"
      end)

    dictionary? =
      Enum.any?(cells, fn cell ->
        Map.get(cell, :token_index) == index and
          Map.get(cell, :pos) == "verb" and
          normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == "did" and
          is_binary(Map.get(cell, :id))
      end)

    surface? and next_not? and dictionary?
  end

  defp did_auxiliary_witness?(_, _, _), do: false

  defp complete_grammar(si, clause) do
    signatures = Map.get(si, :sentence_signatures, [])
    tokens = Map.get(si, :tokens, [])
    indices = Map.new(clause.roles, fn {role, index, _pos} -> {role, index} end)

    nouns_supported? =
      Enum.all?(clause.noun_phrases, fn {det_position, head_position} ->
        det_index = Enum.at(tokens, det_position) && Map.get(Enum.at(tokens, det_position), :index)
        head_index = Enum.at(tokens, head_position) && Map.get(Enum.at(tokens, head_position), :index)

        Enum.any?(signatures, fn sig ->
          Map.get(sig, :id) == :noun_phrase and
            Map.get(sig, :source) == :core_syntax and
            get_in(sig, [:roles, :determiner]) == det_index and
            get_in(sig, [:roles, :head]) == head_index
        end)
      end)

    # Only an independently attested simple-past candidate supports the
    # affirmative constructions. Negation instead requires an exact "did not"
    # sequence plus selected lexical witnesses for both markers.
    predicate_supported? =
      if clause.construction == :explicit_negated_transitive do
        true
      else
        Enum.any?(signatures, fn sig ->
          Map.get(sig, :id) == :finite_past_candidate and
            Map.get(sig, :source) == :core_syntax and
            get_in(sig, [:roles, :verb]) == Map.get(indices, :predicate)
        end)
      end

    if nouns_supported? and predicate_supported?,
      do: :ok, else: {:error, :insufficient_grammatical_evidence}
  end

  defp normalize(value) when is_binary(value),
    do: value |> String.trim() |> String.downcase()
  defp normalize(_), do: ""

  defp abstain(reason),
    do: %{status: :not_interpreted, reason: reason, verification: :external_unverified}
end
