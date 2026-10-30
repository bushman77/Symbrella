defmodule Core.Pipeline.LexicalBridge do
  @moduledoc """
  Read-only, explicitly requested dictionary-sense bridge diagnostic.

  Compare senses for two lexical terms using shared *definition terms*, or
  two explicitly supplied intermediary lemmas. Each result retains every
  attested sense ID and the exact lexical witness. Overlap proposes a
  hypothesis; it does NOT establish a word sense, a real-world relationship,
  or a LIFG winner.

  Does not modify SemanticInput, dictionary rows, neural state or LIFG.
  Intermediate terms are never discovered or asserted automatically.
  """

  alias Core.Pipeline.Input
  alias Core.Pipeline.LTM.Source

  @max_senses_per_term 192
  @max_chains 32
  @noise_words ~w(a an and are as at be been being by can did do does for from
    had has have in into is it its of on or that the their them there these
    this those to was were what when where which who with would you your
    one ones any all each either some such other others something anything
    someone another more less than much many one two three after before
    used use using having has had being often usually sometimes also
    especially including etc like called known kind kinds type types)

  @type report :: map()

  @doc """
  Inspect a direct shared-gloss bridge, or an explicitly supplied two-lemma
  chain (`via: ["boat", "watercourse"]`). A default checked DB lookup reads
  full sense slates, never an eight-sense subset.

  A `:lookup` callback of arity 1 may return `{:ok, rows}` for tests.
  No source text or credentials are logged.
  """
  @spec audit(String.t(), String.t(), keyword()) :: {:ok, report()} | {:error, atom()}
  def audit(source, target, opts \\ [])

  def audit(source, target, opts)
      when is_binary(source) and is_binary(target) and is_list(opts) do
    via = Keyword.get(opts, :via, [])
    terms = [source | List.wrap(via)] ++ [target]

    cond do
      not is_list(via) or length(via) not in [0, 2] ->
        {:error, :invalid_intermediates}

      not Enum.all?(terms, &valid_term?/1) ->
        {:error, :invalid_term}

      length(Enum.uniq(Enum.map(terms, &String.downcase/1))) != length(terms) ->
        {:error, :duplicate_terms}

      true ->
        terms = Enum.map(terms, &String.downcase/1)
        lookup = Keyword.get(opts, :lookup, &dictionary_lookup/1)

        with {:ok, slates} <- lookup_slates(terms, lookup) do
          [source | _] = terms
          target = List.last(terms)
          candidates =
            case Enum.slice(terms, 1, length(terms) - 2) do
              [] -> direct_bridges(slates[source], slates[target])
              [left, right] ->
                two_lemma_bridges(slates[source], slates[left], slates[right],
                  slates[target], left, right)
            end

          examined = Enum.map(candidates, &annotate_evidence(&1, slates, source, target))

          {:ok, %{
            mode: :observation_only,
            source: source,
            target: target,
            via: Enum.slice(terms, 1, length(terms) - 2),
            senses_by_term: Map.new(slates, fn {term, rows} -> {term, length(rows)} end),
            status: if(candidates == [], do: :unresolved, else: :candidate_bridges),
            candidate_count: length(examined),
            evidence_quality_counts: Enum.frequencies_by(examined, & &1.evidence_quality),
            category_evidence_counts: Enum.frequencies_by(examined, & &1.category_evidence.status),
            candidates: Enum.take(examined, @max_chains),
            truncated?: length(examined) > @max_chains,
            verified_intended_sense?: false,
            changes_lifg?: false
          }}
        end
    end
  end

  def audit(_, _, _), do: {:error, :invalid_input}

  defp direct_bridges(sources, targets) do
    for source <- sources,
        target <- targets,
        term <- shared_terms(source, target) do
      %{
        kind: :shared_definition_term,
        source_sense_id: source.id,
        target_sense_id: target.id,
        witness: term,
        via: [],
        status: :candidate_only
      }
    end
    |> Enum.sort_by(&{&1.source_sense_id, &1.target_sense_id, &1.witness})
  end

  defp two_lemma_bridges(sources, firsts, seconds, targets, left, right) do
    for source <- sources,
        MapSet.member?(definition_terms(source), left),
        first <- firsts,
        second <- seconds,
        term <- shared_terms(first, second),
        target <- targets,
        MapSet.member?(definition_terms(target), right) do
      %{
        kind: :two_intermediate_gloss_bridge,
        source_sense_id: source.id,
        target_sense_id: target.id,
        via: [
          %{lemma: left, sense_id: first.id},
          %{lemma: right, sense_id: second.id}
        ],
        witnesses: %{
          source_definition_mentions: left,
          intermediate_definitions_share: term,
          target_definition_mentions: right
        },
        status: :candidate_only
      }
    end
    |> Enum.sort_by(fn c ->
      {c.source_sense_id, c.target_sense_id, Enum.map(c.via, & &1.sense_id),
       c.witnesses.intermediate_definitions_share}
    end)
  end

  # Quality here denotes the *location* of the lexical witness, not the
  # correctness or semantic strength of a relationship. Body-overlap can be
  # accidental ("natural", "size"), and qualifier-overlap ("slang") cannot
  # establish any semantic compatibility. Full candidate lists are retained.
  defp annotate_evidence(%{kind: :shared_definition_term} = candidate, slates, source, target) do
    first = sense(slates, source, candidate.source_sense_id)
    second = sense(slates, target, candidate.target_sense_id)
    from = witness_location(first, candidate.witness)
    to = witness_location(second, candidate.witness)

    candidate
    |> Map.put(:evidence_quality, quality([from, to], :direct))
    |> Map.put(:category_evidence, category_evidence(first, second))
    |> Map.put(:evidence_audit, %{
      source_region: from,
      target_region: to,
      source_senses_with_witness: term_sense_frequency(slates[source], candidate.witness),
      target_senses_with_witness: term_sense_frequency(slates[target], candidate.witness),
      comparison_scope: :retrieved_senses_only,
      semantic_relevance: :not_measured
    })
  end

  defp annotate_evidence(%{kind: :two_intermediate_gloss_bridge} = candidate,
       slates, source, target) do
    [left, right] = candidate.via
    witnesses = candidate.witnesses

    source_location =
      witness_location(sense(slates, source, candidate.source_sense_id),
        witnesses.source_definition_mentions)

    first_location =
      witness_location(sense(slates, left.lemma, left.sense_id),
        witnesses.intermediate_definitions_share)

    second_location =
      witness_location(sense(slates, right.lemma, right.sense_id),
        witnesses.intermediate_definitions_share)

    target_location =
      witness_location(sense(slates, target, candidate.target_sense_id),
        witnesses.target_definition_mentions)

    candidate
    |> Map.put(:evidence_quality,
      quality([source_location, first_location, second_location, target_location], :multi_hop))
    |> Map.put(:category_evidence, category_evidence(
      sense(slates, source, candidate.source_sense_id),
      sense(slates, target, candidate.target_sense_id)
    ))
    |> Map.put(:evidence_audit, %{
      source_lemma_region: source_location,
      first_intermediate_region: first_location,
      second_intermediate_region: second_location,
      target_lemma_region: target_location,
      comparison_scope: :retrieved_senses_only,
      semantic_relevance: :not_measured
    })
  end

  # Category annotations are sparse. Only shared cat:en:* values can
  # provide a positive domain witness; missing or disjoint annotations are
  # always inconclusive, never negative compatibility evidence.
  # Etymology, POS and usage tags are intentionally excluded.
  defp category_evidence(source, target) do
    source_categories = dictionary_categories(source)
    target_categories = dictionary_categories(target)

    shared =
      MapSet.intersection(
        MapSet.new(source_categories), MapSet.new(target_categories)
      )
      |> MapSet.to_list()
      |> Enum.sort()

    status =
      cond do
        shared != [] -> :shared_category
        source_categories == [] and target_categories == [] -> :no_category_metadata
        source_categories == [] or target_categories == [] -> :one_sided_category
        true -> :disjoint_category_labels_unverified
      end

    %{
      status: status,
      source_categories: source_categories,
      target_categories: target_categories,
      shared_categories: shared,
      semantic_compatibility_proven?: false,
      semantic_incompatibility_proven?: false
    }
  end

  defp dictionary_categories(%{semantic_atoms: atoms}) when is_list(atoms) do
    atoms
    |> Enum.filter(&(is_binary(&1) and String.starts_with?(&1, "cat:en:")))
    |> Enum.map(&String.replace_prefix(&1, "cat:en:", ""))
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp dictionary_categories(_), do: []

  defp quality(regions, kind) do
    cond do
      :leading_qualifier in regions -> :includes_leading_qualifier
      :not_attested in regions -> :missing_witness
      kind == :multi_hop -> :multi_hop_gloss_overlap_unverified
      true -> :definition_body_overlap_unverified
    end
  end

  defp sense(slates, lemma, id) do
    slates
    |> Map.fetch!(lemma)
    |> Enum.find(&(&1.id == id))
  end

  defp term_sense_frequency(rows, word) do
    Enum.count(rows, &MapSet.member?(definition_terms(&1), word))
  end

  defp witness_location(%{definition: definition}, word)
       when is_binary(definition) and is_binary(word) do
    {leading, body} =
      case Regex.run(~r/\A\s*(?:\([^)]*\)\s*)+/u, definition) do
        [prefix] -> {prefix, String.replace_prefix(definition, prefix, "")}
        _ -> {"", definition}
      end

    cond do
      MapSet.member?(words(body), word) -> :definition_body
      MapSet.member?(words(leading), word) -> :leading_qualifier
      true -> :not_attested
    end
  end

  defp witness_location(_, _), do: :not_attested

  defp words(text) when is_binary(text) do
    Regex.scan(~r/[\p{L}\p{N}]+/u, text)
    |> Enum.map(fn [term] -> String.downcase(term) end)
    |> MapSet.new()
  end

  defp shared_terms(a, b) do
    a
    |> definition_terms()
    |> MapSet.intersection(definition_terms(b))
    |> MapSet.to_list()
    |> Enum.sort()
  end

  defp definition_terms(%{definition: value}) when is_binary(value) do
    noise = MapSet.new(@noise_words)

    Regex.scan(~r/[\p{L}\p{N}]+/u, value)
    |> Enum.map(fn [term] -> String.downcase(term) end)
    |> Enum.filter(&(String.length(&1) >= 4 and not MapSet.member?(noise, &1)))
    |> MapSet.new()
  end

  defp definition_terms(_), do: MapSet.new()

  defp lookup_slates(terms, lookup) when is_function(lookup, 1) do
    Enum.reduce_while(terms, {:ok, %{}}, fn term, {:ok, result} ->
      outcome =
        try do
          lookup.(term)
        rescue
          _ -> {:error, :lookup_failed}
        catch
          _, _ -> {:error, :lookup_failed}
        end

      case outcome do
        {:ok, %{rows: rows}} when is_list(rows) ->
          accept_rows(term, rows, result)

        {:ok, rows} when is_list(rows) ->
          accept_rows(term, rows, result)

        {:error, reason} when is_atom(reason) ->
          {:halt, {:error, reason}}

        _ ->
          {:halt, {:error, :invalid_lookup_result}}
      end
    end)
  end

  defp lookup_slates(_, _), do: {:error, :invalid_lookup}

  defp accept_rows(_term, rows, _result) when length(rows) > @max_senses_per_term,
    do: {:halt, {:error, :sense_limit_exceeded}}

  defp accept_rows(term, rows, result) do
    if Enum.all?(rows, fn
         %{id: id, norm: ^term, definition: _}
         when is_binary(id) and byte_size(id) > 0 -> true
         _ -> false
       end) do
      {:cont, {:ok, Map.put(result, term, Enum.uniq_by(rows, & &1.id))}}
    else
      {:halt, {:error, :invalid_dictionary_evidence}}
    end
  end

  defp dictionary_lookup(term) do
    term
    |> Input.prepare()
    |> Source.checked_ltm(limit_per_norm: :all)
  end

  defp valid_term?(value) when is_binary(value) do
    byte_size(value) <= 64 and
      Regex.match?(~r/\A[\p{L}]+(?:[-'’][\p{L}]+)*\z/u, value)
  end

  defp valid_term?(_), do: false
end
