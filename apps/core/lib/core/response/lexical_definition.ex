defmodule Core.Response.LexicalDefinition do
  @moduledoc """
  LLM-free definition requests grounded in the LTM BrainCell rows already
  attached to this exact user turn. Parses a complete definition question to
  obtain the lexical target; never substitutes the classifier keyword
  ("meaning", "definition") or a working-memory attention hint.

  When LIFG leaves senses tied, present bounded dictionary alternatives rather
  than treating a stable winner ID as semantic certainty.
  """

  @patterns [
    ~r/\A\s*what\s+is\s+(?:the\s+)?(?:meaning|definition)\s+of\s+(?<term>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[?.!]?\s*\z/iu,
    ~r/\A\s*what\s+does\s+(?<term>[\p{L}][\p{L}\p{M}'-]{0,39})\s+mean\s*[?.!]?\s*\z/iu,
    ~r/\A\s*define\s+(?<term>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[?.!]?\s*\z/iu
  ]

  @max_definitions 3
  @max_definition_chars 180

  @spec requested_term(String.t()) :: String.t() | nil
  def requested_term(text) when is_binary(text) and byte_size(text) <= 260 do
    Enum.find_value(@patterns, fn pattern ->
      case Regex.named_captures(pattern, text) do
        %{"term" => term} -> String.downcase(term)
        _ -> nil
      end
    end)
  end

  def requested_term(_), do: nil

  @spec generate(String.t(), map(), map(), map()) ::
          {:ok, String.t(), map()} | :not_applicable
  def generate(text, %{risk_bucket: :low}, %{action: action}, si)
      when is_binary(text) and is_map(si) and
           action in [:answer, :offer_options, :act_first] do
    with term when is_binary(term) <- requested_term(text),
         ^text <- Map.get(si, :sentence),
         true <- Map.get(si, :source) in [:user, :prod, :test] do
      definitions =
        si
        |> Map.get(:active_cells, [])
        |> valid_definitions(term)

      {reply, status} = reply(term, definitions)

      {:ok, reply, %{
        status: status,
        provenance: :current_turn_ltm_definitions,
        requested_term: term,
        candidate_definitions: length(definitions),
        returned_definitions: min(length(definitions), @max_definitions),
        selected_sense_verified?: length(definitions) == 1,
        lexicon_rows_used?: definitions != [],
        semantic_verified?: false,
        factual_claims?: false,
        llm_called?: false
      }}
    else
      _ -> :not_applicable
    end
  end

  def generate(_, _, _, _), do: :not_applicable

  defp valid_definitions(rows, term) when is_list(rows) do
    rows
    |> Enum.filter(&is_map/1)
    |> Enum.flat_map(fn row ->
      id = field(row, :id)
      norm = field(row, :norm)
      pos = field(row, :pos)
      gloss = field(row, :definition)
      source = field(row, :source)

      cond do
        source not in [:ltm, "ltm"] -> []
        not is_binary(id) or not String.starts_with?(id, term <> "|") -> []
        not is_binary(norm) or String.downcase(norm) != term -> []
        not is_binary(pos) or pos == "" -> []
        not is_binary(gloss) -> []
        true ->
          case clean_gloss(gloss) do
            nil -> []
            definition -> [%{id: id, pos: String.downcase(pos), definition: definition}]
          end
      end
    end)
    |> Enum.sort_by(fn %{id: id, pos: pos} ->
      {pos_rank(pos), sense_number(id), id}
    end)
    |> Enum.uniq_by(fn %{definition: definition} -> String.downcase(definition) end)
  end

  defp valid_definitions(_, _), do: []

  defp clean_gloss(gloss) do
    sanitized =
      gloss
      |> String.replace(~r/[[:cntrl:]]/u, " ")
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

    if String.valid?(sanitized) and String.length(sanitized) in 6..1200 do
      if String.length(sanitized) > @max_definition_chars,
        do: String.slice(sanitized, 0, @max_definition_chars) <> "…",
        else: sanitized
    end
  end

  defp pos_rank("noun"), do: 0
  defp pos_rank("verb"), do: 1
  defp pos_rank("adjective"), do: 2
  defp pos_rank("adverb"), do: 3
  defp pos_rank(_), do: 4

  defp sense_number(id) do
    case List.last(String.split(id, "|")) |> Integer.parse() do
      {number, ""} -> number
      _ -> 999_999
    end
  end

  defp reply(term, []) do
    {"I understand you're asking what \"#{term}\" means, but I don't have a usable dictionary definition in this turn.",
     :lexical_definition_missing}
  end

  defp reply(term, [%{pos: pos, definition: definition}]) do
    {"\"#{term}\" (#{pos}): #{definition}", :lexical_definition}
  end

  defp reply(term, definitions) do
    lines =
      definitions
      |> Enum.take(@max_definitions)
      |> Enum.map_join("\n", fn %{pos: pos, definition: definition} ->
        "- (#{pos}) #{definition}"
      end)

    suffix =
      if length(definitions) > @max_definitions,
        do: "\nOther dictionary senses are also available.",
        else: ""

    {"\"#{term}\" has multiple recorded meanings:\n" <> lines <> suffix,
     :lexical_definition_ambiguous}
  end

  defp field(map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
end
