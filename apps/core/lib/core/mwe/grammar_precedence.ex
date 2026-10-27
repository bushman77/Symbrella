defmodule Core.MWE.GrammarPrecedence do
  @moduledoc """
  Prevent a lexical verb MWE from swallowing the object and particle of a
  larger, independently observed verb-particle construction.

  This only affects candidate *promotion*. Dictionary entries, all POS
  alternatives, and sentence-signature hypotheses remain untouched.

  Example: "Turn the lights off." may contain a dictionary verb entry
  "lights off" (= third-person form of "light off"). Its two-word span is
  nonetheless the noun object + particle of a separate verb "turn".
  By contrast, "He lights off the rocket" may genuinely use that verb MWE.
  """

  @spec incompatible_verb_mwe?(map(), {integer(), integer()}, list() | term()) :: boolean()
  def incompatible_verb_mwe?(si, {first, last}, lexical_pos)
      when is_map(si) and is_integer(first) and is_integer(last) and
             first >= 0 and last >= first do
    lexical_verb_only?(lexical_pos) and
      Enum.any?(List.wrap(Map.get(si, :sentence_signatures)), fn
        %{
          id: :verb_particle_construction,
          roles: %{verb: verb, object: object, particle: particle}
        }
        when is_integer(verb) and is_integer(object) and is_integer(particle) ->
          # A separate preceding verb claims the noun object and trailing
          # particle. A verb-only MWE beginning at that object cannot also
          # serve as the predicate in the very same span.
          verb < first and first <= object and object <= last and
            first <= particle and particle <= last and object < particle

        _ ->
          false
      end)
  end

  def incompatible_verb_mwe?(_si, _span, _pos), do: false

  @doc """
  Do not promote lexical readings that contradict a clear question structure.

  This does not invent new senses: the dictionary entry remains in the raw
  lexical candidates, while the derived semantic view keeps the constituent
  words available for compositional interpretation.
  """
  @spec incompatible_question_mwe?(map(), String.t(), list() | term()) :: boolean()
  def incompatible_question_mwe?(si, phrase, pos) when is_map(si) and is_binary(phrase) do
    sentence = Map.get(si, :sentence) || ""
    phrase = String.downcase(String.trim(phrase))
    poses = List.wrap(pos) |> Enum.map(&normalize_pos/1)

    cond do
      phrase == "what is it" and "interjection" in poses ->
        # When a lexical verb follows "what is it", the words form a
        # compositional question ("What is it used for?" / "What is it
        # sitting on?"). Promoting the shorter dictionary phrase would
        # swallow the interrogative subject and leave the predicate detached.
        Regex.match?(~r/^\s*what\s+is\s+it\s+\p{L}+(?:\s|\?|$)/iu, sentence)

      phrase == "is it" and "interjection" in poses ->
        Regex.match?(~r/^\s*is\s+it\s+.+\?/iu, sentence)

      phrase == "close to" and "adverb" in poses ->
        Regex.match?(~r/^\s*(?:is|was)\s+it\s+close\s+to\s+.+\?/iu, sentence)

      true ->
        false
    end
  end

  def incompatible_question_mwe?(_, _, _), do: false

  defp lexical_verb_only?(poses) do
    case List.wrap(poses) do
      [] -> false
      list -> Enum.all?(list, &(normalize_pos(&1) == "verb"))
    end
  end

  defp normalize_pos(pos) when is_atom(pos), do: Atom.to_string(pos)
  defp normalize_pos(pos) when is_binary(pos), do: String.downcase(pos)
  defp normalize_pos(_), do: ""
end
