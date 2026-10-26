defmodule Brain.LIFG.DefinitionRoleCompatibility do
  @moduledoc """
  Evidence-gated, production dictionary-definition compatibility for LIFG.

  This is not a sense selector. An observable grammatical construction in the
  *current* sentence must agree with a dictionary description before this
  module grants a small bounded positive bonus. No guessed semantic roles,
  database writes, external calls, or overrides of the candidate slate.
  """

  @bonus 0.18
  @venue_heads MapSet.new(~w(
    institution office facility building venue location premises
    shop store restaurant school hospital station library warehouse
  ))
  @determiners MapSet.new(~w(a an the this that my your our))
  @max_modifiers 1

  @spec evaluate(String.t() | nil, String.t() | nil, map()) :: map()
  def evaluate(sentence, target, candidate)
      when is_binary(sentence) and is_binary(target) and is_map(candidate) do
    definition = get(candidate, :definition)
    words = tokenize(sentence)
    target_words = tokenize(target)

    cond do
      not is_binary(definition) or String.trim(definition) == "" ->
        abstain(:missing_definition)

      direct_salutation?(sentence, target_words) and salutation_definition?(definition) ->
        %{applied?: true, bonus: @bonus, reason: :direct_salutation_definition,
          evidence: [:standalone_salutation, :dictionary_salutation_gloss]}

      target_occurrences(words, target_words) != 1 ->
        abstain(:nonunique_or_missing_target)

      locative?(words, target_words) and
          venue_definition?(definition) and
          lexical_support?(words, target_words, definition) ->
        %{applied?: true, bonus: @bonus, reason: :locative_venue_definition,
          evidence: [:locative_preposition, :dictionary_venue_head, :independent_gloss_term]}

      true ->
        abstain(:insufficient_role_evidence)
    end
  end

  def evaluate(_sentence, _target, _candidate), do: abstain(:invalid_input)

  defp abstain(reason), do: %{applied?: false, bonus: 0.0, reason: reason}

  defp direct_salutation?(sentence, [target])
       when target in ["hello", "hi", "hey"] do
    # Require one bare vocative and a declarative/exclamatory greeting; "Hello?"
    # is not enough evidence to distinguish ordinary salutation from a call
    # for attention or a disconnected telephone.
    Regex.match?(
      ~r/\A\s*(?:hello|hi|hey)\s*,?\s+[\p{L}][\p{L}'-]{0,47}\s*[.!]\s*\z/iu,
      sentence
    )
  end

  defp direct_salutation?(_, _), do: false

  defp salutation_definition?(definition) do
    text = String.downcase(definition)
    Regex.match?(~r/\bsalutation\b/u, text) and
      not Regex.match?(~r/\b(?:telephone|sarcastic|sarcastically)\b/u, text)
  end

  defp locative?(words, target_words) do
    indices = matching_indices(words, target_words)

    case indices do
      [index] ->
        prev =
          words
          |> Enum.take(index)
          |> Enum.take(-(@max_modifiers + 2))

        case Enum.reverse(prev) do
          ["at" | _] -> true
          [determiner, "at" | _] -> MapSet.member?(@determiners, determiner)
          [modifier, determiner, "at" | _] ->
            MapSet.member?(@determiners, determiner) and
              Regex.match?(~r/\A\p{L}+\z/u, modifier)
          _ -> false
        end

      _ -> false
    end
  end

  defp venue_definition?(definition) do
    cleaned =
      definition
      |> String.downcase()
      |> String.replace(~r/\A(?:\s*\([^)]{1,80}\))+/u, "")
      |> String.trim()

    head_words =
      cleaned
      |> tokenize()
      |> Enum.drop_while(&(&1 in ["a", "an", "the"]))
      |> Enum.take(3)

    Enum.any?(head_words, &MapSet.member?(@venue_heads, &1))
  end

  # The locative marker by itself says merely "a place". It only earns
  # semantic weight when another contextual term independently occurs in the
  # same candidate definition, preventing automatic location-type preference.
  defp lexical_support?(words, target_words, definition) do
    context = MapSet.new(words) |> MapSet.difference(MapSet.new(target_words))
    gloss = MapSet.new(tokenize(definition))
    common = MapSet.intersection(context, gloss)

    Enum.any?(common, fn term ->
      String.length(term) >= 4 and
        term not in ~w(that this with from there which where when into what about)
    end)
  end

  defp target_occurrences(words, targets), do: length(matching_indices(words, targets))

  defp matching_indices(words, targets) do
    if targets == [] do
      []
    else
      words
      |> Enum.with_index()
      |> Enum.flat_map(fn {_word, index} ->
        if Enum.slice(words, index, length(targets)) == targets, do: [index], else: []
      end)
    end
  end

  defp tokenize(text) when is_binary(text) do
    Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, String.downcase(text))
    |> Enum.map(&hd/1)
  end

  defp tokenize(_), do: []

  defp get(map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
end
