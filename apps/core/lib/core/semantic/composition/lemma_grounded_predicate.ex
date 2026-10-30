defmodule Core.Semantic.Composition.LemmaGroundedPredicate do
  @moduledoc """
  V1 observational bridge from an attested, scored inflected verb to its
  canonical dictionary lemma senses.

  Discovery reads only the current SI's one-token LIFG score slate and
  token-local dictionary cells. It follows a strictly evidenced 'form-of'
  definition and *never* changes a selected POS or sense.

  Grounding accepts separately retrieved lemma dictionary rows (normally
  Db.brain_cells_for_norm/1, supplied by the caller) and emits independently
  attributable, tentative predicate features. Features come from the lemma's
  own verb-sense definition, NOT from the inflection's form-of statement.

  A dictionary entry may express several distinct senses. All attested lemma
  senses are retained, and no feature is interpreted as the intended reading,
  a physical event, or a proved semantic compatibility.
  """

  @max_forms 32
  @max_lemmas 4
  @max_lemma_senses 128

  # Require recognizable morphological headwords. The form-of tag and POS
  # are checked independently; the regex itself is not sufficient evidence.
  @form_of ~r/\b(present participle and gerund|present participle|simple past and past participle|simple past|past participle|third-person singular simple present)\s+of\s+([\p{L}]+(?:[-'’][\p{L}]+)*)\b/ui

  # Closed, deliberately conservative hypothesis rules. Never infer a feature
  # from the lemma string ("sit") or the inflected token ("sitting") alone.
  # Unrecognized definitions remain in the output with no derived features.
  @semantic_rules [
    %{
      feature: :seated_posture,
      family: :physical_posture,
      pattern: ~r/\b(?:upper body is upright|body in an upright position|be seated|is seated|sitting position|seated position|occupy a seat|rest(?:ing)? on (?:the|one's) buttocks)\b/ui
    },
    %{
      feature: :physical_motion,
      family: :movement,
      pattern: ~r/\b(?:move from one place to another|move from one location to another|travel from one place to another|change location by moving)\b/ui
    },
    %{
      feature: :physical_force,
      family: :physical_interaction,
      pattern: ~r/\b(?:apply force to|exert force on|push against|press against|strike with force)\b/ui
    },
    %{
      feature: :physical_contact,
      family: :physical_interaction,
      pattern: ~r/\b(?:make physical contact with|rub against|scratch the surface of)\b/ui
    },
    %{
      feature: :topic_discussion,
      family: :communication,
      pattern: ~r/\b(?:discuss a subject|discuss a topic|talk about a subject|speak about a topic|converse about)\b/ui
    }
  ]

  @doc """
  Identify scored, attested inflected verb senses and their dictionary-derived
  lemmas at a *known* token index. Does not retrieve lemma entries.
  """
  @spec discover(map(), non_neg_integer()) :: map()
  def discover(si, index) when is_map(si) and is_integer(index) and index >= 0 do
    tokens =
      si
      |> get(:tokens)
      |> List.wrap()
      |> Enum.filter(fn
        %{} = token -> get(token, :index) == index and get(token, :n) == 1
        _ -> false
      end)

    choices =
      si
      |> get(:lifg_choices)
      |> List.wrap()
      |> Enum.filter(fn
        %{} = choice -> get(choice, :token_index) == index
        _ -> false
      end)

    with [token] <- tokens,
         word when is_binary(word) <- get(token, :phrase),
         word <- normalize(word),
         true <- word != "",
         [choice] <- choices,
         selected_id when is_binary(selected_id) <- get(choice, :id),
         %{} = scores <- get(choice, :candidate_probs),
         true <- map_size(scores) > 0 and map_size(scores) <= 128 do

      attested =
        si
        |> get(:active_cells)
        |> List.wrap()
        |> Enum.filter(fn
          %{} = cell ->
            get(cell, :token_index) == index and
              normalize(get(cell, :norm)) == word and
              get(cell, :pos) == "verb" and
              valid_id?(get(cell, :id), word, "verb") and
              score_for(scores, get(cell, :id)) != nil
          _ ->
            false
        end)
        |> Enum.uniq_by(&get(&1, :id))

      if length(attested) > @max_forms do
        base(:too_many_attested_form_senses, word, index, selected_id)
      else
        forms =
          Enum.flat_map(attested, fn cell ->
            definition = get(cell, :definition)
            atoms = get(cell, :semantic_atoms)

            if is_binary(definition) and is_list(atoms) and
                 "tag:form-of" in atoms do
              case Regex.run(@form_of, definition) do
                [span, form, lemma] ->
                  lemma = normalize(lemma)

                  if valid_lemma?(lemma) do
                    [%{
                      inflected_sense_id: get(cell, :id),
                      surface: word,
                      lemma: lemma,
                      inflection: String.downcase(form),
                      evidence_text: span,
                      semantic_atoms: Enum.filter(atoms, &is_binary/1),
                      score: score_for(scores, get(cell, :id)),
                      source: :attested_scored_form_of_dictionary_sense,
                      morphological_only?: true
                    }]
                  else
                    []
                  end

                _ ->
                  []
              end
            else
              []
            end
          end)
          |> Enum.sort_by(&{&1.lemma, &1.inflected_sense_id})

        lemmas = forms |> Enum.map(& &1.lemma) |> Enum.uniq()

        status =
          cond do
            forms == [] -> :no_attested_form_of_verb
            length(lemmas) > @max_lemmas -> :too_many_distinct_lemmas
            true -> :forms_attested
          end

        %{
          status: status,
          token_index: index,
          surface: word,
          selected_lifg_id: selected_id,
          forms: if(status == :forms_attested, do: forms, else: []),
          requested_lemmas: if(status == :forms_attested, do: lemmas, else: []),
          selected_lifg_unchanged?: true,
          semantic_compatibility_proven?: false
        }
      end
    else
      _ -> base(:missing_or_unusable_token_scores, nil, index, nil)
    end
  end

  def discover(_, _), do: %{status: :invalid_input, requested_lemmas: [],
                            forms: [], semantic_compatibility_proven?: false}

  @doc """
  Join a discovery record against independently fetched *verb lemma* rows.
  Rejects unrelated lemma cells. A valid form-of witness is not a sense winner.

  All matching lemma senses are evaluated separately, including those whose
  definitions do not match an experimental feature cue.
  """
  @spec ground(map(), [map()]) :: map()
  def ground(%{status: :forms_attested, requested_lemmas: lemmas} = discovery, rows)
      when is_list(lemmas) and is_list(rows) do
    candidates =
      rows
      |> Enum.filter(fn
        %{} = row ->
          lemma = normalize(get(row, :norm))

          lemma in lemmas and get(row, :pos) == "verb" and
            valid_id?(get(row, :id), lemma, "verb")
        _ -> false
      end)
      |> Enum.uniq_by(&get(&1, :id))
      |> Enum.sort_by(&get(&1, :id))

    if length(candidates) > @max_lemma_senses do
      Map.merge(discovery, %{
        status: :too_many_lemma_senses,
        lemma_senses: [],
        feature_hypotheses: []
      })
    else
      senses =
        Enum.map(candidates, fn cell ->
          definition = get(cell, :definition)
          lemma = normalize(get(cell, :norm))

          features =
            if is_binary(definition) do
              Enum.flat_map(@semantic_rules, fn rule ->
                case Regex.run(rule.pattern, definition) do
                  [match | _] ->
                    [%{
                      feature: rule.feature,
                      family: rule.family,
                      source_lemma_sense_id: get(cell, :id),
                      source_lemma: lemma,
                      supporting_text: match,
                      source: :lemma_dictionary_definition,
                      epistemic_status: :candidate_only
                    }]
                  _ ->
                    []
                end
              end)
            else
              []
            end

          %{
            lemma_sense_id: get(cell, :id),
            lemma: lemma,
            definition: definition,
            features: features,
            classified?: features != []
          }
        end)

      all_features = Enum.flat_map(senses, & &1.features)

      status =
        cond do
          senses == [] -> :lemma_senses_unavailable
          all_features == [] -> :lemma_senses_unclassified
          true -> :predicate_feature_candidates
        end

      Map.merge(discovery, %{
        status: status,
        lemma_senses: senses,
        feature_hypotheses: all_features,
        semantic_compatibility_proven?: false,
        selected_lifg_unchanged?: true
      })
    end
  end

  def ground(%{} = discovery, _) do
    Map.merge(discovery, %{lemma_senses: [], feature_hypotheses: [],
                           semantic_compatibility_proven?: false})
  end

  def ground(_, _), do: %{status: :invalid_input, lemma_senses: [],
                         feature_hypotheses: [],
                         semantic_compatibility_proven?: false}

  defp base(status, word, index, selected_id) do
    %{
      status: status, token_index: index, surface: word,
      selected_lifg_id: selected_id, requested_lemmas: [],
      forms: [], semantic_compatibility_proven?: false,
      selected_lifg_unchanged?: true
    }
  end

  defp score_for(scores, id) when is_map(scores) and is_binary(id) do
    case Map.get(scores, id) do
      v when is_number(v) and v >= 0 -> v
      _ -> nil
    end
  end

  defp valid_id?(id, lemma, "verb")
       when is_binary(id) and is_binary(lemma) do
    case String.split(id, "|", parts: 3) do
      [^lemma, "verb", sense] when sense != "" -> true
      _ -> false
    end
  end

  defp valid_id?(_, _, _), do: false

  defp valid_lemma?(lemma) when is_binary(lemma),
    do: String.length(lemma) >= 2 and String.length(lemma) <= 48 and
        Regex.match?(~r/\A[\p{L}]+(?:[-'’][\p{L}]+)*\z/u, lemma)
  defp valid_lemma?(_), do: false

  defp normalize(s) when is_binary(s), do: String.downcase(String.trim(s))
  defp normalize(_), do: ""

  defp get(map, key) when is_map(map),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
  defp get(_, _), do: nil
end
