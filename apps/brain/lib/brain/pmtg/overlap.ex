defmodule Brain.PMTG.Overlap do
  @moduledoc """
  Avoid redundant PMTG retrieval for unigrams contained by a selected,
  verified multiword token. Uses token spans, never substring matching.

  This is a *query planner* optimization. It does not discard LIFG choices
  or change word-sense selections. Overlapping MWEs are kept for resolution,
  and explicitly disputed unigram choices remain eligible for consultation.
  """

  @spec prune([map()], [map()]) :: {[map()], [non_neg_integer()]}
  def prune(needy, tokens) when is_list(needy) and is_list(tokens) do
    by_index =
      tokens
      |> Enum.filter(&is_map/1)
      |> Map.new(fn token -> {index(token), token} end)

    covering =
      for choice <- needy,
          is_map(choice),
          token = Map.get(by_index, index(choice)),
          is_map(token),
          multiword?(token),
          valid_winner?(choice, token),
          span = span(token),
          not is_nil(span),
          do: {index(choice), span}

    Enum.reduce(needy, {[], []}, fn choice, {keep, dropped} ->
      token = if is_map(choice), do: Map.get(by_index, index(choice)), else: nil
      single_span = if is_map(token) and not multiword?(token), do: span(token)

      covered? =
        is_tuple(single_span) and not explicit_dispute?(choice) and
          Enum.any?(covering, fn {mwe_index, outer} ->
            mwe_index != index(choice) and strictly_contains?(outer, single_span)
          end)

      if covered? do
        {keep, [index(choice) | dropped]}
      else
        {[choice | keep], dropped}
      end
    end)
    |> then(fn {keep, dropped} -> {Enum.reverse(keep), Enum.reverse(dropped)} end)
  end

  def prune(needy, _tokens), do: {needy, []}

  defp multiword?(token) do
    get(token, :mw) == true or (is_integer(get(token, :n)) and get(token, :n) > 1)
  end

  defp valid_winner?(choice, token) do
    id = get(choice, :chosen_id) || get(choice, :id)
    phrase = get(token, :norm) || get(token, :phrase)

    with true <- is_binary(id),
         [lemma, _pos, _sense] <- String.split(id, "|", parts: 3),
         true <- is_binary(phrase) do
      normalized = normalize(phrase)

      normalized != "" and normalized == normalize(lemma) and
        not String.ends_with?(id, "|fallback") and
        not explicit_dispute?(choice)
    else
      _ -> false
    end
  end

  # Disputed lexical candidates still get their own lookup; this includes
  # an explicit veto, not ordinary low softmax probabilities from a large slate.
  defp explicit_dispute?(%{} = choice) do
    get(choice, :veto?) == true or
      get(choice, :requires_separate_evidence?) == true or
      get(choice, :semantic_conflict?) == true
  end

  defp explicit_dispute?(_), do: false

  defp strictly_contains?({a, b}, {c, d}),
    do: a <= c and d <= b and (a < c or d < b)

  defp span(token) do
    case get(token, :span) do
      {a, b} when is_integer(a) and is_integer(b) and a >= 0 and b > a -> {a, b}
      [a, b] when is_integer(a) and is_integer(b) and a >= 0 and b > a -> {a, b}
      _ -> nil
    end
  end

  defp index(token) do
    get(token, :token_index) || get(token, :index)
  end

  defp get(map, key) when is_map(map),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key)))

  defp normalize(text) do
    text
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end
end
