defmodule SymbrellaWeb.HomeLive.HTML.EvidenceSections do
  @moduledoc """
  Explain modal sections rendered strictly from Core.SemanticInput-derived
  evidence. Labels retain runtime field names where precision matters.
  Neither dictionary verification nor device state is inferred from LLM text.
  """

  @spec build(map() | term()) :: [map()]
  def build(evidence) when is_map(evidence) do
    []
    |> runtime_section(field(evidence, :runtime))
    |> sentence_competition_section(
      field(evidence, :sentence_competition),
      field(evidence, :question_signatures)
    )
    |> verified_section(field(evidence, :verified_phrasal_meanings))
    |> semantic_section(field(evidence, :semantic_winners))
    |> comprehension_section(field(evidence, :comprehension))
    |> semantic_sense_shadow_section(field(evidence, :semantic_sense_shadow))
    |> semantic_cohort_section(field(evidence, :semantic_cohort))
    |> grammar_section(
      field(evidence, :verb_particle_signatures),
      field(evidence, :atl_verb_particles)
    )
    |> hypotheses_section(field(evidence, :unverified_phrasal_hypotheses))
  end

  def build(_), do: []

  defp runtime_section(sections, runtime) when is_map(runtime) and map_size(runtime) > 0 do
    items =
      []
      |> value("intent", field(runtime, :intent))
      |> value("confidence", field(runtime, :confidence))
      |> value("conversation_act", field(runtime, :conversation_act))
      |> value("context_intent", field(runtime, :context_intent))
      |> value("selected_action", field(runtime, :selected_action))

    append(sections, :runtime_decision, "Core runtime decision", nil,
      "Values from the final SemanticInput, not inferred from the assistant's reply.",
      items
    )
  end

  defp runtime_section(sections, _), do: sections

  defp sentence_competition_section(sections, assessment, signatures) do
    signatures = list(signatures)
    comparisons = if(is_map(assessment), do: list(field(assessment, :comparisons)), else: [])

    if comparisons == [] and signatures == [] do
      sections
    else
      items =
        Enum.map(signatures, fn frame ->
          %{
            label: "Sentence frame #{show(field(frame, :span))}",
            body:
              []
              |> value("id", field(frame, :id))
              |> value("roles", field(frame, :roles))
              |> value("evidence", field(frame, :evidence))
              |> body()
          }
        end) ++
          Enum.map(comparisons, fn comparison ->
            %{
              label: field(comparison, :phrase) || "Lexical vs compositional reading",
              body:
                []
                |> value("winner", field(comparison, :winner))
                |> value("reason", field(comparison, :reason))
                |> value("span", field(comparison, :span))
                |> value("frame_id", field(comparison, :frame_id))
                |> value("crossed_roles", field(comparison, :crossed_roles))
                |> value("role_boundary_conflict?", field(comparison, :role_boundary_conflict?))
                |> value("dictionary_sense_ids", field(comparison, :dictionary_sense_ids))
                |> value("lexical_context_fit", field(comparison, :lexical_context_fit))
                |> value("compositional_compatibility", field(comparison, :compositional_compatibility))
                |> value("lexical_compatibility", field(comparison, :lexical_compatibility))
                |> value("discrimination_margin", field(comparison, :discrimination_margin))
                |> body()
            }
          end)

      append(sections, :sentence_competition, "Sentence-level competition",
        Integer.to_string(length(comparisons)),
        "Compositional grammar competes with dictionary MWEs before LIFG. Compatibility scores are bounded heuristics, not probabilities or LIFG scores.",
        items)
    end
  end

  defp verified_section(sections, meanings) when is_list(meanings) and meanings != [] do
    items =
      Enum.map(meanings, fn m ->
        label = field(m, :id) || "Verified dictionary sense"

        body =
          []
          |> value("phrase", field(m, :phrase))
          |> value("definition", field(m, :definition))
          |> value("source", field(m, :source))
          |> value("context_score", field(m, :context_score))
          |> value("constituent_indices", field(m, :constituent_indices))
          |> value("object_index", field(m, :object_index))
          |> body()

        %{label: label, body: body}
      end)

    append(sections, :verified_phrasal_meanings, "Verified phrase meanings",
      Integer.to_string(length(items)),
      "Actual dictionary IDs; context_score is lexical evidence, NOT a probability or device-state measurement.",
      items
    )
  end

  defp verified_section(sections, _), do: sections

  defp semantic_section(sections, choices) when is_list(choices) and choices != [] do
    items =
      Enum.map(choices, fn c ->
        %{
          label: field(c, :id) || "Semantic choice",
          body:
            []
            |> value("token_index", field(c, :token_index))
            |> value("token_span", field(c, :token_span))
            |> value("kind", field(c, :kind))
            |> value("lemma", field(c, :lemma))
            |> value("source", field(c, :source))
            |> value("score", field(c, :score))
            |> value("definition", field(c, :definition))
            |> body()
        }
      end)

    append(sections, :semantic_choices, "Canonical semantic winners",
      Integer.to_string(length(items)),
      "Exact semantic_choices returned by Core. A contiguous MWE can replace its covered individual words here; discontinuous meanings appear above.",
      items
    )
  end

  defp semantic_section(sections, _), do: sections

  defp comprehension_section(sections, summary) when is_map(summary) do
    items =
      []
      |> value("intent", field(summary, :intent))
      |> value("recognized topics (not verified)", field(summary, :recognized_topics))
      |> value("selected lexical candidates", field(summary, :selected_lexical_candidates))
      |> value("verified lexical understanding", field(summary, :understood))
      |> value("uncertain", field(summary, :uncertain))
      |> value("unresolved", field(summary, :unresolved))
      |> value("degraded?", field(summary, :degraded?))
      |> value("reasons", field(summary, :reasons))

    append(sections, :comprehension, "Comprehension", nil,
      "Recognized topics, selected lexical candidates, and verified word meanings are distinct evidence levels. Weak-token warnings are retained.",
      items
    )
  end

  defp comprehension_section(sections, _), do: sections

  defp semantic_sense_shadow_section(sections, %{metadata: meta} = evidence)
       when is_map(meta) do
    status = field(meta, :status)
    reason = field(evidence, :reason)
    sources = list(field(meta, :role_sources))
    ranked = list(field(meta, :top_candidates))
    exclusions = list(field(meta, :exclusions))

    outcome =
      []
      |> value("evaluation status", status)
      |> value("reason", reason)
      |> value("original surface sense", field(meta, :original_surface_id))
      |> value("attested base lexeme", field(meta, :attested_base_lexeme))
      |> value("dictionary senses considered", field(meta, :sense_count))
      |> value("baseline lexical hypothesis", field(meta, :baseline_id))
      |> value("shadow semantic hypothesis", field(meta, :proposed_id))
      |> value("original lexical margin", field(meta, :baseline_gap))
      |> value("expanded margin", field(meta, :evaluated_gap))
      |> value("role expansion enabled?", field(meta, :role_expansion_enabled?))
      |> value("role expansion status", field(meta, :role_expansion_status))
      |> value("excluded candidates (total)", field(meta, :excluded_count))
      |> value("meaning verified?", field(meta, :sense_verified?))
      |> value("applied to LIFG?", field(meta, :applied_to_lifg?))
      |> value("response changed?", field(meta, :response_changed?))
      |> value("synaptic weight transferred?", field(meta, :synaptic_weight_transferred?))

    role_items =
      Enum.map(sources, fn ctx ->
        %{
          label: "Selected noun role: " <> show(field(ctx, :role)),
          body:
            []
            |> value("status", field(ctx, :status))
            |> value("admission reason", field(ctx, :reason))
            |> value("selected noun sense ID", field(ctx, :noun_sense_id))
            |> value("tied dictionary sense IDs", field(ctx, :noun_sense_ids))
            |> value("tied senses considered", field(ctx, :competing_sense_count))
            |> value("gloss evidence provenance", field(ctx, :source))
            |> value("LIFG selected probability", field(ctx, :selected_probability))
            |> value("one-hop dictionary terms", field(ctx, :terms))
            |> body()
        }
      end)

    candidate_items =
      Enum.map(ranked, fn c ->
        %{
          label: "Candidate " <> show(field(c, :id)),
          body:
            []
            |> value("direct lexical fit", field(c, :lexical_score))
            |> value("patient support", field(c, :patient_support))
            |> value("actor support", field(c, :actor_support))
            |> value("combined diagnostic", field(c, :combined_diagnostic_score))
            |> value("grammar", field(c, :grammar))
            |> value("one-hop evidence", field(c, :one_hop_witnesses))
            |> body()
        }
      end)

    excluded_items =
      Enum.map(exclusions, fn c ->
        %{
          label: "Excluded " <> show(field(c, :id)),
          body:
            []
            |> value("grammatical gate", field(c, :grammar))
            |> value("missing restrictions", field(c, :missing_restrictions))
            |> value("reasons", field(c, :exclusions))
            |> body()
        }
      end)

    items = [%{label: "Read-only assessment", body: body(outcome)}] ++
      role_items ++ candidate_items ++ excluded_items

    append(sections, :semantic_sense_shadow, "Semantic sense (shadow only)",
      show(status),
      "Real post-LIFG/ATL dictionary evidence. These are provisional or abstained hypotheses, NOT verified meanings. No live LIFG selection, synaptic update or response change.",
      items)
  end

  defp semantic_sense_shadow_section(sections, _), do: sections

  defp semantic_cohort_section(sections, cohort) when is_map(cohort) do
    groups = list(field(cohort, :cohorts))

    overview =
      []
      |> value("status", field(cohort, :status))
      |> value("reason", field(cohort, :reason))
      |> value("projected SVO participants", field(cohort, :participant_surfaces))
      |> value("attested predicate base", field(cohort, :base_lexeme))
      |> value("eligible dictionary senses", field(cohort, :eligible_sense_count))
      |> value("supported cohorts", field(cohort, :cohort_count))
      |> value("shared meaning independently verified?",
        field(cohort, :semantic_equivalence_verified?))
      |> value("exact sense selected?", field(cohort, :chose_exact_sense?))
      |> value("LIFG modified?", field(cohort, :modified_lifg?))
      |> value("response changed?", field(cohort, :changed_response?))

    groups =
      Enum.map(groups, fn group ->
        members = list(field(group, :members))

        header =
          []
          |> value("support origin", field(group, :evidence_origin))
          |> value("shared contextual anchor", field(group, :anchor_term))
          |> value("member senses total", field(group, :member_count))
          |> value("truncated members?", field(group, :member_ids_truncated?))
          |> value("shared definition words",
            field(group, :shared_definition_terms))
          |> value("noun role source evidence",
            field(group, :role_evidence_provenance))
          |> value("lowest member diagnostic support",
            field(group, :minimum_member_support))
          |> value("semantic equivalence verified?",
            field(group, :shared_meaning_verified?))

        member_details =
          Enum.map(members, fn member ->
            []
            |> value("sense", field(member, :id))
            |> value("direct fit", field(member, :direct_fit))
            |> value("patient support", field(member, :patient_support))
            |> value("actor support", field(member, :actor_support))
            |> value("diagnostic score", field(member, :diagnostic_score))
            |> value("distinguishing definition words",
              field(member, :distinguishing_definition_terms))
            |> body()
          end)
          |> Enum.join("\n\n")

        %{
          label: "Shared cue: " <> show(field(group, :anchor_term)) <>
            " (" <> show(field(group, :evidence_origin)) <> ")",
          body: body(header) <> "\n\n" <> member_details
        }
      end)

    append(sections, :semantic_cohort, "Semantic cohorts (shadow only)",
      show(field(cohort, :status)),
      "Senses share independently observed lexical evidence within the same validated sentence. Cohorts can overlap; resemblance is NOT sense equivalence, a verified concept, or a chosen meaning. Restrictions were checked first.",
      [%{label: "Cohort audit", body: body(overview)} | groups])
  end

  defp semantic_cohort_section(sections, _), do: sections

  defp grammar_section(sections, signatures, constituents) do
    signatures = list(signatures)
    constituents = list(constituents)

    if signatures == [] and constituents == [] do
      sections
    else
      items =
        Enum.map(signatures, fn sig ->
          %{
            label: "Syntax #{show(field(sig, :span))}",
            body:
              []
              |> value("id", field(sig, :id))
              |> value("roles", field(sig, :roles))
              |> value("evidence", field(sig, :evidence))
              |> body()
          }
        end) ++
          Enum.map(constituents, fn con ->
            %{
              label: "ATL #{show(field(con, :span))}",
              body:
                []
                |> value("status", field(con, :status))
                |> value("roles", field(con, :roles))
                |> value("unresolved_roles", field(con, :unresolved_roles))
                |> body()
            }
          end)

      append(sections, :grammar_atl, "Syntax and ATL verb-particle roles", nil,
        "Syntax is a structural hypothesis; ATL records which individual roles actually resolved.",
        items
      )
    end
  end

  defp hypotheses_section(sections, hypotheses) when is_list(hypotheses) and hypotheses != [] do
    items =
      Enum.map(hypotheses, fn h ->
        %{
          label: field(h, :phrase) || "Possible phrasal verb",
          body:
            []
            |> value("span", field(h, :span))
            |> value("constituent_indices", field(h, :constituent_indices))
            |> value("particle_position", field(h, :particle_position))
            |> value("dictionary_check", field(h, :dictionary_check))
            |> value("verified?", field(h, :verified?))
            |> body()
        }
      end)

    append(sections, :unverified_phrasal_hypotheses, "Unverified syntax hypotheses", nil,
      "These were created before the separate dictionary verification step. A verified phrase above does not retroactively change these discovery records.",
      items
    )
  end

  defp hypotheses_section(sections, _), do: sections

  defp append(sections, key, title, tag, hint, items) do
    normalized_items =
      Enum.map(items, fn
        {label, datum} -> %{label: label, body: show(datum)}
        %{} = item -> item
      end)

    sections ++ [%{
      key: key, title: title, tag: tag, hint: hint, items: normalized_items
    }]
  end

  defp value(items, _label, nil), do: items
  defp value(items, label, datum), do: items ++ [{label, datum}]

  defp body(pairs) do
    Enum.map_join(pairs, "\n", fn {label, datum} -> "#{label}: #{show(datum)}" end)
  end

  defp show(nil), do: "nil"
  defp show(true), do: "true"
  defp show(false), do: "false"
  defp show(v) when is_atom(v), do: ":#{v}"
  defp show(v) when is_binary(v), do: v
  defp show(v) when is_number(v), do: to_string(v)
  defp show(v), do: inspect(v, limit: 40, printable_limit: 2_000)

  defp field(map, key) when is_map(map),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key)))

  defp field(_, _), do: nil

  defp list(v) when is_list(v), do: v
  defp list(_), do: []
end
