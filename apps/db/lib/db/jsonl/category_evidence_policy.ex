defmodule Db.JSONL.CategoryEvidencePolicy do
  @moduledoc """
  Conservative, versioned semantic-atom policy for Wiktextract categories.

  Wiktionary source="w" is provenance, not semantic certification. A category
  named "English links with redundant wikilinks" is source="w" too.

  This policy promotes only explicit classes:
    * place or topic categories with source="w" -> cat:en:...
    * reviewed "other" domain names          -> cat:en:...
    * reviewed proper-name classes           -> cat:en:...
    * reviewed regional labels               -> usage:region:...
    * reviewed grammatical labels            -> usage:grammar:...

  Unknown kinds, legacy category strings, maintenance/disambiguation metadata
  and unreviewed "other" labels are withheld, not deleted from source JSONL.
  LIFG's current ContextFit deliberately ignores usage:* atoms; they are
  retained separately for a future usage-aware scorer.

  Note: This affects NEWLY populated BrainCell structs only. Existing database
  rows are not rewritten or stripped by this module.
  """

  alias Db.JSONL.{CategoryProvenance, Util}

  @version 1

  # Explicit human-reviewed semantic domains. Do not use substring matching
  # or promote every Wiktionary category of kind="other".
  @domains MapSet.new([
    "algebra", "anatomy", "astronomy", "baseball", "biology", "botany",
    "category theory", "chemistry", "chess", "computing", "cricket",
    "engineering", "finance", "geography", "geology", "grammar",
    "history", "hydrology", "law", "linguistics", "mathematics",
    "medicine", "military", "music", "nautical", "physics",
    "software", "sports"
  ])

  @regions MapSet.new([
    "american english", "australian english", "british english",
    "canadian english", "english english", "indian english",
    "irish english", "new zealand english", "scottish english",
    "south african english", "us english"
  ])

  # Keep the namespace used by the Cash pilot for backward compatibility.
  @name_classes MapSet.new([
    "english surnames", "english given names", "english male given names",
    "english female given names"
  ])

  @grammar_labels MapSet.new(["english pluralia tantum"])

  @type decision ::
          {:emit, :semantic_domain | :place | :topic |
                  :name_class | :regional_usage | :grammatical_usage, String.t()} |
          {:skip, :disambiguation_source | :maintenance | :unreviewed_other |
                   :unknown_kind | :wrong_source | :legacy_string | :missing_name}

  @spec version() :: pos_integer()
  def version, do: @version

  @spec decide(term()) :: decision()
  def decide(%{"name" => name} = category) when is_binary(name) do
    normalized = Util.normalize_atom(name)
    kind = Map.get(category, "kind")
    source = Map.get(category, "source")

    cond do
      normalized == "" ->
        {:skip, :missing_name}

      is_binary(source) and String.contains?(source, "disamb") ->
        {:skip, :disambiguation_source}

      source != "w" ->
        {:skip, :wrong_source}

      CategoryProvenance.likely_maintenance?(name) ->
        {:skip, :maintenance}

      kind == "place" ->
        {:emit, :place, "cat:en:" <> normalized}

      kind == "topic" ->
        {:emit, :topic, "cat:en:" <> normalized}

      kind == "other" and MapSet.member?(@domains, normalized) ->
        {:emit, :semantic_domain, "cat:en:" <> normalized}

      kind == "other" and MapSet.member?(@regions, normalized) ->
        {:emit, :regional_usage, "usage:region:" <> normalized}

      kind == "other" and MapSet.member?(@name_classes, normalized) ->
        {:emit, :name_class, "cat:en:" <> normalized}

      kind == "other" and MapSet.member?(@grammar_labels, normalized) ->
        {:emit, :grammatical_usage, "usage:grammar:" <> normalized}

      kind == "other" ->
        {:skip, :unreviewed_other}

      true ->
        {:skip, :unknown_kind}
    end
  end

  def decide(%{"name" => _}), do: {:skip, :missing_name}
  def decide(value) when is_binary(value), do: {:skip, :legacy_string}
  def decide(_), do: {:skip, :missing_name}

  @doc "Return only vetted atoms, preserving first-seen order per sense."
  @spec atoms(term()) :: [String.t()]
  def atoms(values) do
    values
    |> List.wrap()
    |> Enum.flat_map(fn value ->
      case decide(value) do
        {:emit, _class, atom} -> [atom]
        {:skip, _reason} -> []
      end
    end)
    |> Enum.uniq()
  end
end
