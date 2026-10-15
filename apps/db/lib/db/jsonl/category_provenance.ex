defmodule Db.JSONL.CategoryProvenance do
  @moduledoc """
  Read-only triage for Wiktextract category provenance, *not* a whitelist
  for BrainCell semantic atoms.

  Wiktextract's source = "w" means that a category came from Wiktionary;
  it does NOT prove the category is meaningful lexical evidence. Category
  kind = "other" includes both useful names such as "English surnames" and
  maintenance categories such as "Pages with 2 entries".

  Treat :other_review and :likely_maintenance as separate inspection queues.
  Do not suppress either from the retained raw JSONL; this module never writes.
  """

  @type classification ::
          :place | :topic | :other_review | :likely_maintenance |
          :disambiguation_source | :non_w_source | :untyped |
          :missing_name | :legacy_string

  @spec classify(term()) :: classification()
  def classify(%{"name" => name} = map) when is_binary(name) do
    name = String.trim(name)
    kind = Map.get(map, "kind")
    source = Map.get(map, "source")

    cond do
      name == "" -> :missing_name
      is_binary(source) and String.contains?(source, "disamb") ->
        :disambiguation_source
      source != "w" -> :non_w_source
      kind == "place" -> :place
      kind == "topic" -> :topic
      likely_maintenance?(name) -> :likely_maintenance
      kind == "other" -> :other_review
      true -> :untyped
    end
  end

  def classify(value) when is_binary(value), do: :legacy_string
  def classify(_), do: :missing_name

  @doc "Conservative name patterns for an audit queue, not evidence suppression."
  @spec likely_maintenance?(String.t()) :: boolean()
  def likely_maintenance?(name) when is_binary(name) do
    low = String.downcase(String.trim(name))

    Enum.any?(
      [
        "pages with ", "entries with ", "english entries with ",
        "english links with ", "terms with redundant ",
        "english terms with redundant ", "english terms with collocations"
      ],
      &String.starts_with?(low, &1)
    ) or String.contains?(low, "translation boxes") or
      String.contains?(low, "incorrect language header") or
      Regex.match?(~r/^terms with .+ translations$/, low) or
      # Wiktionary prefixes many maintenance labels with a language name,
      # e.g. "Mandarin terms with redundant transliterations". The old
      # "terms with redundant ..." prefix only matched unprefixed labels.
      Regex.match?(
        ~r/(?:^| )terms with (?:non-)?redundant (?:manual )?(?:transliterations|script codes)$/,
        low
      )
  end

  def likely_maintenance?(_), do: false
end
