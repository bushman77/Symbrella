defmodule Core.Semantic.VerbLemma do
  @moduledoc """
  Canonicalize a selected finite-present verb only when the exact dictionary
  sense explicitly identifies its base form.

  LIFG's \`:lemma\` currently names the selected surface BrainCell (for
  example, "draws"), not necessarily the dictionary base verb ("draw").
  The full-form relation therefore requires both matching Wiktextract
  morphology tags and an explicit form-of statement in that sense's gloss.

  Without that proof, retain the selected surface form rather than guessing
  with an English suffix rule.
  """

  @finite_present_tags [
    "tag:form-of",
    "tag:indicative",
    "tag:present",
    "tag:singular",
    "tag:third-person"
  ]

  @base_pattern ~r/\A[a-z][a-z0-9_]{0,47}\z/

  @form_of_glosses [
    ~r/\A(?:\([^)]*\)\s*)?(?:an?\s+)?inflected form of (?<base>[a-z][a-z0-9_]{0,47})\.?\z/iu,
    ~r/\A(?:\([^)]*\)\s*)?third-person singular (?:simple )?present (?:indicative )?(?:form )?of (?<base>[a-z][a-z0-9_]{0,47})\.?\z/iu
  ]

  @spec canonical(map(), map()) :: {:ok, String.t()} | :error
  def canonical(si, choice) when is_map(si) and is_map(choice) do
    id = get(choice, :chosen_id) || get(choice, :id)

    case id do
      value when is_binary(value) ->
        case String.split(value, "|", parts: 3) do
          [surface, "verb", _sense] ->
            surface = String.downcase(surface)

            if Regex.match?(@base_pattern, surface) do
              {:ok, attested_base(si, value) || surface}
            else
              :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  def canonical(_, _), do: :error

  defp attested_base(si, id) do
    bases =
      si
      |> get(:active_cells)
      |> List.wrap()
      |> Enum.filter(&(get(&1, :id) == id))
      |> Enum.map(&base_from_cell/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    # Never choose arbitrarily between conflicting dictionary evidence.
    case bases do
      [base] -> base
      _ -> nil
    end
  end

  defp base_from_cell(cell) do
    tags = get(cell, :semantic_atoms)
    definition = get(cell, :definition)

    if is_list(tags) and Enum.all?(@finite_present_tags, &(&1 in tags)) and
         is_binary(definition) do
      gloss =
        definition
        |> String.split(" — ")
        |> List.last()
        |> String.trim()

      Enum.find_value(@form_of_glosses, fn regex ->
        case Regex.named_captures(regex, gloss) do
          %{"base" => base} ->
            base = String.downcase(base)
            if Regex.match?(@base_pattern, base), do: base, else: nil

          _ ->
            nil
        end
      end)
    end
  end

  defp get(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp get(_, _), do: nil
end
