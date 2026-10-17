defmodule Core.Semantic.SVOEvent do
  @moduledoc """
  Evidence-gated subject-verb-object event projection. Requires the full
  finite-present clause, exact LIFG role senses, and agreeing ATL winners.

  This stores the user's attributed assertion; it does not verify truth.
  """
  alias Core.Semantic.EpisodicEvents

  @required [:contiguous_surface, :attested_nominal_subject,
             :attested_nominal_object, :attested_finite_present_verb]

  @spec project(map(), String.t()) :: [map()]
  def project(si, sentence) when is_map(si) and is_binary(sentence) do
    with true <- admissible_text?(sentence),
         [sig] <- signatures(si),
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
         lemma when is_binary(lemma) <- lemma(verb_choice),
         true <- Regex.match?(~r/\A[a-z][a-z0-9_]{0,47}\z/, lemma),
         {:ok, event} <- EpisodicEvents.normalize(%{entity: entity,
           relation: "verb_" <> lemma, value: value}) do
      # Fixed 0.8 denotes an admission grade, not a probability of truth.
      [Map.merge(event, %{source_frame: :finite_present_svo, frame_confidence: 0.8})]
    else
      _ -> []
    end
  end

  def project(_, _), do: []

  defp signatures(si) do
    get(si, :sentence_signatures)
    |> List.wrap()
    |> Enum.filter(fn
      %{id: :finite_present_svo_candidate, source: :core_syntax,
        span: {0, last}, roles: roles, evidence: evidence}
        when is_integer(last) and is_map(roles) and is_list(evidence) ->
          Enum.all?(@required, &(&1 in evidence)) and
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

  defp lemma(choice) do
    case get(choice, :lemma) do
      text when is_binary(text) and text != "" -> String.downcase(text)
      _ ->
        (get(choice, :chosen_id) || get(choice, :id) || "")
        |> String.split("|") |> hd() |> String.downcase()
    end
  end

  defp clean(text), do: text |> String.trim() |> String.trim_trailing(".") |> String.downcase()
  defp get(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil
end
