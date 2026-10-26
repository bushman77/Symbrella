defmodule Core.Semantic.SVOEvent do
  @moduledoc """
  Evidence-gated subject-verb-object event projection. Requires the full
  finite-present or independently attested finite-past clause, exact LIFG
  role senses, and agreeing ATL winners.

  This stores the user's attributed assertion; it does not verify truth.
  """
  alias Core.Semantic.{EpisodicEvents, VerbLemma}

  @required_present [:contiguous_surface, :attested_nominal_subject,
                      :attested_nominal_object, :attested_finite_present_verb]
  @required_past [:contiguous_surface, :attested_nominal_subject,
                   :attested_nominal_object, :attested_simple_past_verb]

  @spec project(map(), String.t()) :: [map()]
  def project(si, sentence) when is_map(si) and is_binary(sentence) do
    with true <- admissible_text?(sentence),
         [sig] <- full_clause_signatures(si, sentence),
         %{subject_head: subject, predicate: predicate, object_head: object} = roles <- sig.roles,
         {first, last} <- sig.span,
         true <- is_integer(subject) and is_integer(predicate) and is_integer(object),
         true <- first <= subject and subject < predicate and predicate < object and object <= last,
         {:ok, tokens} <- clause_tokens(si, first, last, sentence),
         {:ok, entity} <- nominal(tokens, roles, :subject_determiner, subject),
         {:ok, value} <- nominal(tokens, roles, :object_determiner, object),
         {:ok, sub_choice} <- single_choice(si, subject, "noun"),
         {:ok, verb_choice} <- single_choice(si, predicate, "verb"),
         {:ok, obj_choice} <- single_choice(si, object, "noun"),
         true <- atl_match?(si, [{subject, sub_choice}, {predicate, verb_choice}, {object, obj_choice}]),
         {:ok, lemma} <- canonical_verb(si, verb_choice, sig.id),
         verb_surface when is_binary(verb_surface) <- Map.get(tokens, predicate),
         verb_surface <- clean(verb_surface),
         true <- Regex.match?(~r/\A\p{L}+\z/u, verb_surface),
         {:ok, event} <- EpisodicEvents.normalize(%{entity: entity,
           relation: "verb_" <> lemma, value: value}) do
      # Frame roles are grounded in exact surface tokens, compatible LIFG
      # selections, and ATL agreement. Preserve the original verb surface
      # separately from its attested canonical relation for reverse queries.
      tense = if sig.id == :finite_past_svo_candidate, do: "past", else: "present"
      source_frame = if tense == "past", do: :finite_past_svo, else: :finite_present_svo

      participants = %{
        "actor" => entity,
        "predicate" => lemma,
        "surface_predicate" => verb_surface,
        "patient" => value,
        "tense" => tense
      }

      # Fixed 0.8 denotes an admission grade, not a probability of truth.
      [Map.merge(event, %{source_frame: source_frame, frame_confidence: 0.8,
                           participants: participants})]
    else
      _ -> []
    end
  end

  def project(_, _), do: []

  @doc """
  Return the unique *complete-sentence* finite SVO signature.

  This uses the same source, evidence and exact-surface constraints as the
  projection boundary. Smaller overlapping SVO hypotheses (such as
  "the artist draws a") must not suppress a validated full clause.
  A signature alone is not evidence of an event: callers must still revalidate
  the selected lexical roles and episode independently.
  """
  @spec complete_signature(map()) :: map() | nil
  def complete_signature(%{} = si) do
    case get(si, :sentence) do
      sentence when is_binary(sentence) ->
        if admissible_text?(sentence) do
          case full_clause_signatures(si, sentence) do
            [signature] -> signature
            _ -> nil
          end
        end

      _ -> nil
    end
  end

  def complete_signature(_), do: nil

  # Overlapping lexical POS hypotheses may create partial SVO candidates.
  # Only full-sentence interpretations compete for episode admission.
  # Multiple complete analyses remain ambiguous and are rejected.
  defp full_clause_signatures(si, sentence) do
    si
    |> signatures()
    |> Enum.filter(fn %{span: {first, last}} ->
      match?({:ok, _}, clause_tokens(si, first, last, sentence))
    end)
  end

  defp canonical_verb(si, choice, :finite_past_svo_candidate),
    do: VerbLemma.canonical_past(si, choice)
  defp canonical_verb(si, choice, :finite_present_svo_candidate),
    do: VerbLemma.canonical(si, choice)
  defp canonical_verb(_, _, _), do: :error

  defp signatures(si) do
    get(si, :sentence_signatures)
    |> List.wrap()
    |> Enum.filter(fn
      %{id: id, source: :core_syntax,
        span: {0, last}, roles: roles, evidence: evidence}
        when id in [:finite_present_svo_candidate, :finite_past_svo_candidate] and
             is_integer(last) and is_map(roles) and is_list(evidence) ->
          required =
            if id == :finite_past_svo_candidate, do: @required_past,
              else: @required_present

          Enum.all?(required, &(&1 in evidence)) and
            not Map.has_key?(roles, :negation)
      _ -> false
    end)
  end

  defp admissible_text?(text) do
    t = String.trim(text)
    t != "" and byte_size(t) <= 256 and
      not String.contains?(t, ["?", "\"", "“", "”", ";", ":", "\n"]) and
      not Regex.match?(~r/\b(?:not|never|no|maybe|perhaps|possibly|probably|might|could|would|should|if|unless|apparently|supposedly|reportedly)\b/iu, t) and
      not Regex.match?(~r/^(?:i think|i guess|imagine|suppose|according to|remember that)\b/iu, t)
  end

  # Exact full-clause surface alignment prevents subordinate or quoted spans
  # from becoming independent episodic claims.
  defp clause_tokens(si, first, last, sentence) do
    tokens =
      get(si, :tokens)
      |> List.wrap()
      |> Enum.with_index()
      |> Enum.flat_map(fn {token, position} ->
        index = get(token, :index) || position
        word = get(token, :phrase)
        n = get(token, :n)
        kind = get(token, :kind)
        if is_integer(index) and is_binary(word) and word != "" and
             (is_nil(n) or n == 1) and kind not in [:punct, "punct"] do
          [{index, word}]
        else
          []
        end
      end)
      |> Enum.sort_by(&elem(&1, 0))
    ids = Enum.map(tokens, &elem(&1, 0))
    surface = tokens |> Enum.map(&elem(&1, 1)) |> Enum.join(" ")

    if first == 0 and last == length(tokens) - 1 and
         ids == Enum.to_list(first..last) and clean(surface) == clean(sentence),
      do: {:ok, Map.new(tokens)}, else: :error
  end

  defp nominal(tokens, roles, determiner, head) do
    head_word = Map.get(tokens, head)
    determiner_word = Map.get(tokens, Map.get(roles, determiner))
    noun =
      if is_binary(determiner_word) and is_binary(head_word),
        do: determiner_word <> " " <> head_word, else: head_word

    if is_binary(noun) and Regex.match?(~r/\A[\p{L}\p{N}][\p{L}\p{N} '-]*\.?\z/u, noun),
      do: {:ok, noun |> String.trim_trailing(".") |> String.downcase()},
      else: :error
  end

  defp single_choice(si, index, pos) do
    selected =
      get(si, :lifg_choices)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :token_index) == index))
    case selected do
      [one] ->
        id = get(one, :chosen_id) || get(one, :id)
        if is_binary(id) and String.contains?(id, "|" <> pos <> "|"),
          do: {:ok, one}, else: :error
      _ -> :error
    end
  end

  defp atl_match?(si, role_choices) do
    slate = get(si, :atl_slate)
    winners = if is_map(slate), do: get(slate, :winners), else: nil
    is_list(winners) and
      Enum.all?(role_choices, fn {index, choice} ->
        case Enum.filter(winners, &(get(&1, :token_index) == index)) do
          [winner] -> (get(winner, :id) || get(winner, :chosen_id)) ==
                        (get(choice, :chosen_id) || get(choice, :id))
          _ -> false
        end
      end)
  end

  defp clean(text), do: text |> String.trim() |> String.trim_trailing(".") |> String.downcase()
  defp get(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil
end
