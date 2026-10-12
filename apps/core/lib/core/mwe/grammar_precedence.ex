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
