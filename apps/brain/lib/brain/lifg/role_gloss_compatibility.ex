defmodule Brain.LIFG.RoleGlossCompatibility do
  @moduledoc """
  Candidate-local, read-only compatibility between an observed grammatical
  object/source relation and ONE dictionary-sense definition.

  This is a deliberately small paraphrase grammar, not embedding similarity:
  * object and source HEADS must appear literally, in that order;
  * the relation between them must be "from" or "out of";
  * an optional determiner and up to two modifiers may precede the source;
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
