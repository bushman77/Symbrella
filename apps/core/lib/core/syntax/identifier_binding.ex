defmodule Core.Syntax.IdentifierBinding do
  @moduledoc """
  Surface-verified attachment of a code-shaped name to a named container.

  Only an independently observed object followed immediately by the attested
  'in a module called Identifier' construction is accepted. Both the artifact
  and identifier retain their original spelling. This is grammatical evidence
  for a user request, NOT proof that the module exists or is valid Elixir.
  """

  @max_tokens 64

  @spec detect(map(), [map()], map()) :: [map()]
  def detect(%{} = si, frames, %{unresolved_tokens: identifiers})
      when is_list(frames) and is_list(identifiers) do
    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :n, 1) == 1))

    surfaces = Map.new(tokens, fn token ->
      {Map.get(token, :index), strip_punct(Map.get(token, :phrase))}
    end)
    cells = List.wrap(Map.get(si, :active_cells, []))
    declared = Map.new(identifiers, &{&1.token_index, &1})

    if map_size(surfaces) > @max_tokens do
      []
    else
      Enum.flat_map(frames, fn
        %{status: :candidate, source: :original_syntax_and_dictionary_pos,
          object_head: object, object_surface: artifact,
          meaning_verified?: false} when is_integer(object) and is_binary(artifact) ->
          prep = object + 1
          det = object + 2
          owner = object + 3
          called = object + 4
          name_index = object + 5
          name = Map.get(declared, name_index)

          valid? =
            normalize(Map.get(surfaces, prep)) == "in" and
              normalize(Map.get(surfaces, det)) == "a" and
              normalize(Map.get(surfaces, owner)) == "module" and
              normalize(Map.get(surfaces, called)) == "called" and
              witnessed?(cells, "in", "preposition") and
              witnessed?(cells, "a", "determiner") and
              witnessed?(cells, "module", "noun") and
              (witnessed?(cells, "called", "verb") or
                 witnessed?(cells, "called", "adjective")) and
              is_map(name) and name.kind == :declared_identifier and
              name.lexically_resolved? == false and
              name.surface == Map.get(surfaces, name_index)

          if valid? do
            [%{
              status: :candidate, frame: :requested_artifact_named_module,
              source: :original_syntax_and_dictionary_pos,
              requested_object_head: object,
              requested_object_surface: artifact,
              container_head: owner,
              container_surface: Map.get(surfaces, owner),
              container_name_index: name_index,
              container_name_surface: name.surface,
              span: {prep, name_index},
              evidence: [:adjacent_prepositional_attachment,
                         :attested_module_head, :attested_naming_marker,
                         :unresolved_identifier_surface],
              semantic_type_verified?: false,
              existence_verified?: false, meaning_verified?: false,
              applied_to_lifg?: false
            }]
          else
            []
          end

        _ -> []
      end)
    end
  end

  def detect(_, _, _), do: []

  defp witnessed?(cells, word, pos) do
    Enum.any?(cells, fn
      %{id: id} = cell when is_binary(id) ->
        normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == word and
          normalize(Map.get(cell, :pos)) == pos and
          String.starts_with?(id, word <> "|" <> pos <> "|")
      _ -> false
    end)
  end

  defp strip_punct(s) when is_binary(s),
    do: String.replace(s, ~r/[?!.;,]+\z/u, "")
  defp strip_punct(_), do: nil

  defp normalize(s) when is_binary(s), do: String.downcase(s)
  defp normalize(_), do: nil
end
