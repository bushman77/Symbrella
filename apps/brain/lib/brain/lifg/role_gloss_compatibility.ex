defmodule Brain.LIFG.RoleGlossCompatibility do
  @moduledoc """
  Candidate-local, read-only compatibility between an observed grammatical
  object/source relation and ONE dictionary-sense definition.

  This is a deliberately small paraphrase grammar, not embedding similarity:
  * object and source HEADS must appear literally, in that order;
  * the relation between them must be "from" or "out of";
  * an optional determiner and up to two modifiers may precede the source;
  * a tightly bounded enumerated object list can pair with an enumerated
    source list inside the SAME gloss clause;
  * punctuation or conjunctions cannot bridge a missing relation.

  "remove a sword from its sheath" and "pull a sword out of its sheath"
  therefore share a structural reading, without declaring "remove", "pull"
  or "withdraw" interchangeable in the dictionary.

  No confidence, sense identity or lifecycle data is modified or inferred.
  """

  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @boundaries MapSet.new(~w(and but because while if although whereas))
  @max_modifiers 2
  @word_or_punctuation ~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*|[\p{P}\p{S}]/u
  @enumerated_clause ~r/\(([^()]{1,100})\)[ \t]+(from|out[ \t]+of)[ \t]+([^.;:!?—–\-\n]{1,100})/u
  @max_alternative_words 4
  @max_alternatives 6

  @type relation :: %{required(:object) => String.t(),
                       required(:source) => String.t(),
                       required(:preposition) => String.t()}

  @spec match(String.t() | nil, relation()) :: {:ok, map()} | :no_match
  def match(definition, %{object: object, source: source, preposition: marker})
      when is_binary(definition) and is_binary(object) and
             is_binary(source) and marker in ["from", "out of"] do
    obj = normalize(object)
    src = normalize(source)

    if word?(obj) and word?(src) do
      tokens = tokenize(definition)

      direct =
        tokens
        |> Enum.with_index()
        |> Enum.find_value(:no_match, fn {word, index} ->
          if word == obj do
            # Both markers express the same bounded SOURCE relation.
            # The gloss may say "water out of a well" even if the sentence
            # says "water from a well" (and vice versa).
            find_relation(tokens, index, src)
          else
            false
          end
        end)

      case direct do
        {:ok, _} = result -> result
        _ -> enumerated_relation(definition, obj, src)
      end
    else
      :no_match
    end
  end

  def match(_, _), do: :no_match

  defp find_relation(tokens, object_index, source) do
    suffix = Enum.drop(tokens, object_index + 1)

    case suffix do
      # Wiktextract often sets the object in parentheses: "(water) from a well".
      # Permit exactly one closing parenthesis, NOT arbitrary punctuation.
      [")", "from" | remaining] ->
        source_match(remaining, source, "from", object_index, object_index + 3)

      [")", "out", "of" | remaining] ->
        source_match(remaining, source, "out of", object_index, object_index + 4)

      ["from" | remaining] ->
        source_match(remaining, source, "from", object_index, object_index + 2)

      ["out", "of" | remaining] ->
        source_match(remaining, source, "out of", object_index, object_index + 3)

      _ ->
        false
    end
  end

  defp source_match(remaining, source, marker, object_index, start_index) do
    {words, offset} =
      case remaining do
        [determiner | tail] ->
          if MapSet.member?(@determiners, determiner),
            do: {tail, 1}, else: {remaining, 0}

        [] ->
          {[], 0}
      end

    words
    |> Enum.take(@max_modifiers + 1)
    |> Enum.with_index()
    |> Enum.find_value(false, fn {word, relative_index} ->
      if word == source and
           Enum.all?(Enum.take(words, relative_index), &modifier?/1) do
        {:ok, %{
          pattern: marker,
          definition_token_span:
            {object_index, start_index + offset + relative_index},
          modifier_count: relative_index
        }}
      else
        false
      end
    end)
  end

  # Bounded Wiktionary-style lists, e.g.:
  #   "pull out (a firearm, sword, or other weapon) from a holster, sheath, etc."
  # Both matched heads must appear in the linked object and source lists.
  # The regular expression cannot cross a clause-ending period, semicolon,
  # colon, exclamation mark, question mark, em dash or newline.
  defp enumerated_relation(definition, object, source) do
    @enumerated_clause
    |> Regex.scan(String.downcase(definition))
    |> Enum.find_value(:no_match, fn [_, object_text, marker, source_text] ->
      object_alternatives = alternatives(object_text)
      source_alternatives = alternatives(source_text)

      if String.contains?(object_text, ",") and
           length(object_alternatives) >= 2 and
           length(object_alternatives) <= @max_alternatives and
           length(source_alternatives) >= 1 and
           length(source_alternatives) <= @max_alternatives and
           Enum.any?(object_alternatives, &(alternative_head?(&1, object))) and
           Enum.any?(source_alternatives, &(alternative_head?(&1, source))) do
        {:ok,
         %{
           pattern: :enumerated_object_source,
           gloss_marker: String.replace(marker, ~r/\s+/u, " "),
           object_alternatives: length(object_alternatives),
           source_alternatives: length(source_alternatives),
           object_head: object,
           source_head: source
         }}
      else
        false
      end
    end)
  end

  # Conjunctions coordinate alternative heads ONLY inside the bounded
  # parenthetical or source-list capture; they are not skipped in open text.
  defp alternatives(text) do
    text
    |> String.replace(~r/\s+\b(?:or|and)\b\s+/u, ",")
    |> String.split(",", trim: true)
    |> Enum.map(&String.trim/1)
    |> Enum.map(&String.replace(&1, ~r/^(?:or|and)\s+/u, ""))
    |> Enum.reject(&(&1 == ""))
  end

  defp alternative_head?(alternative, expected) do
    tokens = Regex.scan(~r/[\p{L}\p{N}]+(?:['’][\p{L}\p{N}]+)*/u, alternative)
             |> Enum.map(&hd/1)

    length(tokens) in 1..@max_alternative_words and
      List.last(tokens) == expected
  end

  defp modifier?(word), do: word?(word) and
                              not MapSet.member?(@boundaries, word) and
                              not MapSet.member?(@determiners, word)

  defp word?(token) when is_binary(token), do: Regex.match?(~r/^[\p{L}\p{N}]+$/u, token)
  defp word?(_), do: false

  defp tokenize(text) do
    Regex.scan(@word_or_punctuation, String.downcase(text))
    |> Enum.map(&hd/1)
  end

  defp normalize(text), do: text |> String.trim() |> String.downcase()
end
