defmodule Core.Semantic.PredicateLemmaEvidence do
  @moduledoc """
  Read-only dictionary grounding for a grammatical predicate's lemma.

  The caller must supply an already attested token-local verb witness. A
  form-of dictionary definition must explicitly name the lemma; the surface
  spelling alone never authorizes lemmatization. The lookup retains every
  attested verb sense within the safety bound, including competing valencies.

  Grammatical argument positions and dictionary valency tags are reported
  separately. Neither proves what the verb means in this sentence.
  """

  alias Core.Pipeline.Input
  alias Core.Pipeline.LTM.Source

  @max_form_senses 32
  @max_lemma_senses 128
  @form_of ~r/\b(simple past and past participle|simple past|past participle|present participle and gerund|present participle|third-person singular simple present)\s+of\s+([\p{L}]+(?:[-'’][\p{L}]+)*)\b/ui
  @valency_tags ["tag:transitive", "tag:ditransitive", "tag:intransitive"]

  @spec audit(map(), [map()], keyword()) :: map()
  def audit(predicate, cells, opts \\ [])

  def audit(%{token_index: index, word: word, attested_verb_ids: ids},
            cells, opts)
      when is_integer(index) and index >= 0 and is_binary(word) and
           is_list(ids) and is_list(cells) and is_list(opts) do
    matching =
      Enum.filter(cells, fn
        %{token_index: ^index, pos: "verb", norm: norm, id: id} = cell ->
          is_binary(id) and is_binary(norm) and normalize(norm) == word and
            id in ids and String.starts_with?(id, word <> "|verb|") and
            is_binary(Map.get(cell, :definition)) and
            is_list(Map.get(cell, :semantic_atoms))

        _ -> false
      end)

    if length(matching) > @max_form_senses do
      base(:form_sense_limit_exceeded)
    else
      forms =
        matching
        |> Enum.flat_map(fn cell ->
          if "tag:form-of" in cell.semantic_atoms do
            case Regex.run(@form_of, cell.definition) do
              [evidence, inflection, lemma] ->
                lemma = normalize(lemma)

                if valid_lemma?(lemma) do
                  [%{
                    inflected_sense_id: cell.id,
                    inflected_word: word,
                    lemma: lemma,
                    inflection: String.downcase(inflection),
                    supporting_text: evidence,
                    source_definition: cell.definition,
                    source: :attested_form_of_dictionary_definition
                  }]
                else
                  []
                end

              _ -> []
            end
          else
            []
          end
        end)
        |> Enum.uniq_by(&{&1.inflected_sense_id, &1.lemma})
        |> Enum.sort_by(&{&1.lemma, &1.inflected_sense_id})

      lemmas = forms |> Enum.map(& &1.lemma) |> Enum.uniq()

      case lemmas do
        [] ->
          base(:no_attested_form_of)

        [lemma] ->
          ground(lemma, forms, opts)

        _ ->
          base(:multiple_attested_lemmas)
          |> Map.put(:forms, forms)
          |> Map.put(:lemmas, lemmas)
      end
    end
  end

  def audit(_, _, _), do: base(:invalid_input)

  defp ground(lemma, forms, opts) do
    lookup = Keyword.get(opts, :lookup, &dictionary_lookup/1)

    case safe_lookup(lookup, lemma) do
      {:ok, rows} ->
        if length(rows) > @max_lemma_senses do
          base(:lemma_sense_limit_exceeded)
          |> Map.put(:forms, forms)
          |> Map.put(:lemma, lemma)
        else
          senses =
            rows
            |> Enum.filter(fn
              %{id: id, norm: norm, pos: "verb", definition: definition}
                  when is_binary(id) and is_binary(norm) and
                       is_binary(definition) ->
                normalize(norm) == lemma and
                  String.starts_with?(id, lemma <> "|verb|")

              _ -> false
            end)
            |> Enum.uniq_by(& &1.id)
            |> Enum.sort_by(& &1.id)
            |> Enum.map(fn cell ->
              atoms = Map.get(cell, :semantic_atoms)
              tags = if is_list(atoms), do: atoms, else: []
              %{
                sense_id: cell.id,
                definition: cell.definition,
                valency_tags: Enum.filter(@valency_tags, &(&1 in tags)),
                valency_relation_to_explicit_object: :not_determined,
                semantic_relation_verified?: false
              }
            end)

          %{
            status: if(senses == [], do: :lemma_senses_unavailable, else: :lemma_senses_attested),
            lemma: lemma,
            forms: forms,
            lemma_sense_count: length(senses),
            lemma_senses: senses,
            argument_structure: :explicit_subject_predicate_object,
            selected_sense_id: nil,
            semantic_relation_verified?: false,
            modifies_lifg?: false
          }
        end

      {:error, reason} ->
        base(:lookup_error)
        |> Map.put(:reason, reason)
        |> Map.put(:lemma, lemma)
        |> Map.put(:forms, forms)
    end
  end

  defp safe_lookup(lookup, lemma) when is_function(lookup, 1) do
    result =
      try do
        lookup.(lemma)
      rescue
        _ -> {:error, :lookup_failed}
      catch
        _, _ -> {:error, :lookup_failed}
      end

    case result do
      {:ok, %{rows: rows}} when is_list(rows) -> {:ok, rows}
      {:ok, rows} when is_list(rows) -> {:ok, rows}
      {:error, reason} when is_atom(reason) -> {:error, reason}
      _ -> {:error, :invalid_lookup_result}
    end
  end
  defp safe_lookup(_, _), do: {:error, :invalid_lookup}

  defp dictionary_lookup(lemma) do
    lemma |> Input.prepare() |> Source.checked_ltm(limit_per_norm: :all)
  end

  defp valid_lemma?(word),
    do: byte_size(word) <= 64 and
      Regex.match?(~r/\A[\p{L}]+(?:[-'’][\p{L}]+)*\z/u, word)

  defp normalize(word) when is_binary(word),
    do: word |> String.downcase() |> String.trim()
  defp normalize(_), do: ""

  defp base(status) do
    %{
      status: status, forms: [], lemma_senses: [], lemma_sense_count: 0,
      selected_sense_id: nil, semantic_relation_verified?: false,
      modifies_lifg?: false
    }
  end
end
