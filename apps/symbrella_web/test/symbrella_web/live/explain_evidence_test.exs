defmodule SymbrellaWeb.HomeLive.ExplainEvidenceTest do
  use ExUnit.Case, async: true

  require Phoenix.LiveViewTest

  alias SymbrellaWeb.HomeLive.ExplainEvidence
  alias SymbrellaWeb.HomeLive.HTML.Modal

  defp si(word_order) do
    {sentence, ids, span, phrase_source, obj_index, grammar} =
      case word_order do
        :separated ->
          {
            "Turn the lights off.",
            ["turn|verb|9", "the|determiner|1", "lights|noun|1", "off|adverb|2"],
            {0, 3},
            :verified_discontinuous_mwe,
            2,
            %{verb: 0, determiner: 1, object: 2, particle: 3}
          }

        :contiguous ->
          {
            "Turn off the lights.",
            ["off|adjective|1", "the|determiner|1", "lights|noun|1"],
            {0, 1},
            :mwe_lexicon,
            3,
            %{verb: 0, particle: 1}
          }

        :power ->
          {
            "Power the computer down.",
            ["power|verb|1", "the|determiner|1", "computer|noun|3", "down|adverb|16"],
            {0, 3},
            :verified_discontinuous_mwe,
            2,
            %{verb: 0, determiner: 1, object: 2, particle: 3}
          }
      end

    {phrase, sense, definition} =
      if word_order == :power do
        {"power down", "power down|verb|1", "(ambitransitive) To switch off (something electrical)."}
      else
        {"turn off", "turn off|verb|2", "(transitive) To deactivate an appliance, light or mechanism."}
      end

    choices =
      ids
      |> Enum.with_index()
      |> Enum.map(fn {id, index} ->
        %{
          id: id,
          lemma: id |> String.split("|") |> hd(),
          token_index: if(word_order == :contiguous, do: index + 1, else: index),
          pos: id |> String.split("|") |> Enum.at(1),
          score: 0.037945,
          definition: "real dictionary gloss for #{id}",
          source: :active_cells
        }
      end)

    semantic =
      if word_order == :contiguous do
        [
          %{id: sense, token_index: 0, token_span: span, kind: :mwe,
            lemma: phrase, definition: definition, score: 0.138507,
            source: :mwe_lexicon}
          | tl(choices)
        ]
      else
        choices
      end

    %{
      sentence: sentence,
      intent: :command,
      confidence: 0.82,
      conversation_act: :instruction,
      context_frame: %{intent: :command},
      selected_action: :answer_user,
      lifg_choices: choices,
      semantic_choices: semantic,
      lexical_constructions: [
        %{
          id: sense, phrase: phrase, dictionary_verified?: true,
          definition: definition, source: phrase_source,
          context_score: if(word_order == :power, do: 0.145, else: 0.29),
          constituent_indices:
            if(word_order == :contiguous, do: [0, 1], else: [0, 3]),
          span: span, object_index: obj_index
        }
      ],
      sentence_signatures: [
        %{id: :verb_particle_construction, span: span, roles: grammar,
          evidence: [:separated_particle, :determiner_noun_object]}
      ],
      atl_slate: %{
        constituents: [
          %{id: :verb_particle_construction, span: span,
            status: if(word_order == :contiguous, do: :partially_resolved, else: :resolved),
            roles: %{verb: %{sense_id: hd(ids)}}, unresolved_roles: []}
        ]
      },
      comprehension: %{
        intent: :command,
        degraded?: true,
        reasons: [:weak_decision_rate_high],
        recognized_topics: [if(word_order == :power, do: "computer", else: "lights")],
        selected_lexical_candidates: [],
        understood: [phrase],
        uncertain: [], unresolved: [],
        lexical_meanings: [%{id: sense, phrase: phrase, definition: definition}]
      },
      mwe_hypotheses:
        if(word_order == :contiguous, do: [], else: [
          %{category: :phrasal_verb, phrase: phrase, span: span,
            constituent_indices: [0, 3], particle_position: :separated,
            verified?: false, dictionary_check: :not_checked}
        ])
    }
  end

  defp payload(si) do
    Modal.explain_payload_for(%{
      id: "test-evidence",
      text: "The computer is already powered down.",
      intent: si.intent,
      confidence: si.confidence,
      cognition: ExplainEvidence.from_si(si),
      senses_selected: [
        %{label: "computer → computer pos=noun src=ltm score=0.20",
          definition: "An electronic device."}
      ],
      selected_action: si.selected_action,
      action_meta: %{safety_gate: :approved}
    })
  end

  defp section(payload, key) do
    Enum.find(payload.sections, &(&1.key == key))
  end

  test "evidence projection copies canonical Core choices and never substitutes raw words" do
    for mode <- [:separated, :contiguous, :power] do
      input = si(mode)
      evidence = ExplainEvidence.from_si(input)

      assert evidence.runtime.intent == input.intent
      assert evidence.runtime.confidence == input.confidence
      assert evidence.runtime.conversation_act == input.conversation_act
      assert evidence.runtime.context_intent == input.context_frame.intent
      assert evidence.runtime.selected_action == input.selected_action
      assert Enum.map(evidence.semantic_winners, & &1.id) ==
               Enum.map(input.semantic_choices, & &1.id)
      assert Enum.map(evidence.lexical_winners, & &1.id) ==
               Enum.map(input.lifg_choices, & &1.id)
      assert [%{id: id, context_score: context_score}] = evidence.verified_phrasal_meanings
      assert id == hd(input.lexical_constructions).id
      assert context_score == hd(input.lexical_constructions).context_score
      assert evidence.comprehension.degraded? == true
      assert evidence.comprehension.reasons == [:weak_decision_rate_high]
    end
  end

  test "HTML modal reports verified power down phrase, noun sense, ATL, and unresolved degradation" do
    input = si(:power)
    result = payload(input)

    verified = section(result, :verified_phrasal_meanings)
    assert [%{label: "power down|verb|1", body: body}] = verified.items
    assert body =~ "To switch off (something electrical)"
    assert body =~ "source: :verified_discontinuous_mwe"
    assert body =~ "context_score: 0.145"
    assert body =~ "object_index: 2"
    assert verified.hint =~ "NOT a probability"

    canonical = section(result, :semantic_choices)
    assert Enum.any?(canonical.items, &(&1.label == "computer|noun|3"))
    assert Enum.any?(canonical.items, &(&1.label == "power|verb|1"))
    refute Enum.any?(canonical.items, &(&1.label == "computer|noun|1"))

    atlas = section(result, :grammar_atl)
    assert Enum.any?(atlas.items, &(&1.label == "ATL {0, 3}" and &1.body =~ "status: :resolved"))

    summary = section(result, :comprehension)
    assert Enum.any?(summary.items, &(&1.label == "degraded?" and &1.body == "true"))
    assert Enum.any?(summary.items, &(&1.label == "reasons" and &1.body =~ ":weak_decision_rate_high"))

    assert Enum.any?(
             section(result, :runtime_decision).items,
             &(&1.label == "selected_action" and &1.body == ":answer_user")
           )

    assert section(result, :unverified_phrasal_hypotheses).hint =~
             "before the separate dictionary verification"

    raw = section(result, :senses_selected)
    assert raw.title == "Individual word senses (LIFG)"
    assert raw.hint =~ "Canonical semantic winners"
  end

  test "both turn-off constructions show same dictionary meaning but truthful different sources" do
    sep = payload(si(:separated))
    adj = payload(si(:contiguous))

    for {response, source} <- [{sep, ":verified_discontinuous_mwe"},
                               {adj, ":mwe_lexicon"}] do
      assert [%{label: "turn off|verb|2", body: body}] =
               section(response, :verified_phrasal_meanings).items
      assert body =~ "source: " <> source
      assert body =~ "deactivate an appliance"

      assert Enum.any?(section(response, :comprehension).items,
               &(&1.label == "verified lexical understanding" and &1.body =~ "turn off"))
      assert Enum.any?(section(response, :comprehension).items,
               &(&1.label == "recognized topics (not verified)" and
                 &1.body =~ "lights"))
    end

    assert Enum.any?(section(adj, :semantic_choices).items,
             &(&1.label == "turn off|verb|2" and &1.body =~ "kind: :mwe"))

    assert Enum.any?(section(sep, :semantic_choices).items, &(&1.label == "turn|verb|9"))
  end

  test "unverified hypotheses and weak token choices are not promoted to verified dictionary meaning" do
    source = si(:separated)
    not_verified = %{source | lexical_constructions: []}

    result = payload(not_verified)
    assert section(result, :verified_phrasal_meanings) == nil
    assert section(result, :unverified_phrasal_hypotheses) != nil
    assert section(result, :comprehension) != nil

    invalid = %{source | lexical_constructions: [
      %{id: "fake|verb|999", definition: "unsupported guess", dictionary_verified?: false}
    ]}

    assert ExplainEvidence.from_si(invalid).verified_phrasal_meanings == []
  end

  test "the UI cannot silently omit Core semantic senses after the old 24-item display limit" do
    choices =
      Enum.map(1..36, fn i ->
        %{id: "sample|noun|#{i}", token_index: i,
          definition: "Real recorded sense #{i}", score: 0.2}
      end)

    input = %{sentence: "A long statement.", semantic_choices: choices,
              lifg_choices: choices}

    evidence = ExplainEvidence.from_si(input)
    assert length(evidence.semantic_winners) == 36

    result = Modal.explain_payload_for(%{
      text: "A long response.",
      cognition: evidence
    })

    assert length(section(result, :semantic_choices).items) == 36
    assert List.last(section(result, :semantic_choices).items).label == "sample|noun|36"
  end

  test "Explain preserves the sentence-level comparison, evidence and actual winner" do
    input = si(:contiguous)

    competition = %{
      version: 1,
      competing_frames: 1,
      rejected_spans: [{0, 3}],
      comparisons: [
        %{
          span: {0, 3},
          phrase: "what do you know",
          dictionary_sense_ids: ["what do you know|phrase|2"],
          frame_id: :wh_aux_topic_question,
          lexical_context_fit: 0.29,
          compositional_compatibility: 0.90,
          lexical_compatibility: 0.424,
          discrimination_margin: 0.476,
          winner: :compositional_clause,
          reason: :complete_question_structure_outweighs_lexical_fit
        }
      ]
    }

    input =
      input
      |> Map.put(:sentence_competition, competition)
      |> Map.update!(:sentence_signatures, fn signatures ->
        signatures ++ [
          %{id: :wh_aux_topic_question, span: {0, 7},
            roles: %{wh: 0, auxiliary: 1, subject: 2, predicate: 3,
                     preposition: 4},
            evidence: [:wh_fronted, :auxiliary_inversion, :question_punctuation]}
        ]
      end)

    evidence = ExplainEvidence.from_si(input)
    assert evidence.sentence_competition == competition
    assert [%{id: :wh_aux_topic_question}] = evidence.question_signatures

    result = payload(input)
    section = section(result, :sentence_competition)
    assert section.title == "Sentence-level competition"
    assert section.hint =~ "not probabilities"
    assert Enum.any?(section.items, fn x ->
             x.label == "what do you know" and
               x.body =~ "winner: :compositional_clause" and
               x.body =~ "lexical_context_fit: 0.29"
           end)
  end

  test "old cached Explain payloads are upgraded from saved Core state without duplication" do
    original = %{
      title: "Explain",
      sections: [
        %{key: :intent_tone, title: "Intent & tone",
          items: [%{label: "tone", body: ":warm"}]},
        %{key: :message, title: "Message",
          items: [%{label: "content", body: "Already powered down."}]}
      ]
    }

    upgraded = Modal.enrich_saved_payload(original, si(:power))
    upgraded_twice = Modal.enrich_saved_payload(upgraded, si(:power))

    assert upgraded == upgraded_twice
    assert hd(upgraded.sections).key == :intent_tone
    assert List.last(upgraded.sections).key == :message
    assert Enum.any?(
             section(upgraded, :verified_phrasal_meanings).items,
             &(&1.label == "power down|verb|1")
           )

    assert Enum.any?(
             section(upgraded, :semantic_choices).items,
             &(&1.label == "computer|noun|3")
           )

    assert Modal.enrich_saved_payload(original, %{intent: :unknown}) == original
  end

  test "Explain shows whether grounded population revision ran and which text the LLM wrote" do
    initial = "Kamloops is around 17,800 people."
    repaired = "Wikidata lists a dated population statement for Kamloops, BC of 97,902 (2021)."

    payload =
      Modal.explain_payload_for(%{
        text: repaired,
        from: %{
          response_reflection: %{
            status: :repair,
            issues: [:geographic_claim_unverified],
            applied?: true,
            repair_count: 1,
            draft_text: initial,
            grounded_revision: %{
              attempted?: true, outcome: :rejected,
              source: :core_fallback, first_draft: initial,
              candidate: "Kamloops has 97,902 residents now.",
              candidate_issues: [:geographic_claim_unverified]
            }
          }
        }
      })

    result = section(payload, :response_reflection)
    assert result.title == "LLM response review"
    items = Map.new(result.items, &{&1.label, &1.body})

    assert items["initial model draft"] == initial
    assert items["grounded revision outcome"] == ":rejected"
    assert items["final answer origin"] == ":core_fallback"
    assert items["revision candidate"] =~ "97,902 residents now"
    assert items["revision candidate issues"] =~ ":geographic_claim_unverified"

    no_retry =
      Modal.explain_payload_for(%{
        text: repaired,
        from: %{response_reflection: %{
          status: :repair, issues: [:geographic_claim_unverified],
          applied?: true, draft_text: initial
        }}
      })
      |> section(:response_reflection)

    no_retry_items = Map.new(no_retry.items, &{&1.label, &1.body})
    assert no_retry_items["initial model draft"] == initial
    assert no_retry_items["grounded revision outcome"] == ":not_attempted"
    assert no_retry_items["final answer origin"] == ":core_fallback"
    refute Map.has_key?(no_retry_items, "revision candidate")
  end

  test "Explain distinguishes a detected semantic role substitution from lexical uncertainty" do
    payload =
      Modal.explain_payload_for(%{
        text: "I understand you're describing the baker cooking a meal.",
        from: %{
          response_reflection: %{
            status: :repair,
            issues: [:semantic_role_drift],
            applied?: true,
            repair_count: 1,
            semantic_role_fidelity: %{
              status: :role_substitution_detected,
              mismatched_roles: [:predicate, :patient],
              source: :revalidated_sentence_roles,
              event_truth_verified?: false,
              exact_sense_verified?: false,
              modified_cognition?: false
            }
          }
        }
      })

    review = section(payload, :response_reflection)
    assert review.title == "LLM response review"

    values = Map.new(review.items, &{&1.label, &1.body})
    assert values["status"] == ":repair"
    assert values["issues"] =~ ":semantic_role_drift"
    assert values["role fidelity review"] == ":role_substitution_detected"
    assert values["role differences"] == ":predicate, :patient"
    refute Map.has_key?(values, "initial model draft")
  end

  test "Explain modal uses viewport width and wraps long LIFG labels and values" do
    long_label =
      "what → what pos=pronoun src=active_cells score=0.11574459075927734 " <>
        String.duplicate("verylongunbrokentoken_", 12)

    html =
      Phoenix.LiveViewTest.render_component(&Modal.explain_modal/1,
        open?: true,
        payload: %{
          sections: [
            %{
              key: :individual_word_senses,
              title: "Individual word senses (LIFG)",
              tag: ":location_question",
              hint: String.duplicate("unbroken_hint_", 20),
              items: [%{label: long_label, body: String.duplicate("long_definition_", 30)}]
            }
          ]
        }
      )

    assert html =~ ~s(role="dialog")
    assert html =~ "fixed inset-x-0"
    assert html =~ "max-w-[100vw]"
    refute html =~ "margin-left: -50vw"
    assert html =~ "grid-cols-1"
    assert html =~ "min-w-0"
    assert html =~ "[overflow-wrap:anywhere]"
    assert html =~ "whitespace-pre-wrap"
    assert html =~ "Individual word senses (LIFG)"
    assert html =~ "verylongunbrokentoken_"
    assert html =~ "long_definition_"
  end

  test "Explain renders only genuine live semantic-sense shadow trace and preserves caveats" do
    event = %{
      stage: :semantic_sense_shadow,
      decision: :provisional_hypothesis,
      reason: :provisional_context_hypothesis,
      meta: %{
        mode: :shadow_only, status: :provisional_hypothesis,
        original_surface_id: "draws|verb|1",
        attested_base_lexeme: "draw", sense_count: 138,
        baseline_id: "draw|verb|129", proposed_id: "draw|verb|129",
        baseline_gap: 0.29, evaluated_gap: 0.29, excluded_count: 40,
        role_expansion_enabled?: true, role_expansion_status: :grounded,
        role_sources: [
          %{role: :patient, status: :grounded_one_hop,
            noun_sense_id: "picture|noun|1", terms: ["image", "representation"]}
        ],
        top_candidates: [
          %{id: "draw|verb|129", lexical_score: 0.29,
            patient_support: 0.105, actor_support: 0.0,
            combined_diagnostic_score: 0.395}
        ],
        exclusions: [
          %{id: "draw|verb|103", exclusions: [
            %{kind: :missing_restriction_witness, detail: :cricket_bowling}
          ]}
        ],
        sense_verified?: false, applied_to_lifg?: false,
        response_changed?: false, synaptic_weight_transferred?: false
      }
    }

    source = si(:power)
    with_trace = Map.put(source, :trace, [event])
    evidence = ExplainEvidence.from_si(with_trace)
    assert evidence.semantic_sense_shadow.metadata.proposed_id == "draw|verb|129"
    assert evidence.semantic_sense_shadow.metadata.mode == :shadow_only

    rendered = payload(with_trace)
    section = section(rendered, :semantic_sense_shadow)
    assert section.title == "Semantic sense (shadow only)"
    assert section.hint =~ "NOT verified meanings"
    assert Enum.any?(section.items, fn item ->
      item.label == "Read-only assessment" and
        item.body =~ "shadow semantic hypothesis: draw|verb|129" and
        item.body =~ "applied to LIFG?: false" and
        item.body =~ "response changed?: false"
    end)
    assert Enum.any?(section.items, fn item ->
      item.label == "Selected noun role: :patient" and
        item.body =~ "picture|noun|1"
    end)
    assert Enum.any?(section.items, fn item ->
      item.label == "Excluded draw|verb|103" and
        item.body =~ "cricket_bowling"
    end)

    # A cached Explain with its original SI is upgraded without duplication.
    before = %{title: "Explain", sections: []}
    upgraded = Modal.enrich_saved_payload(before, with_trace)
    assert section(upgraded, :semantic_sense_shadow) != nil
    assert Modal.enrich_saved_payload(upgraded, with_trace) == upgraded
  end

  test "Explain displays read-only sense cohorts only from genuine shadow trace evidence" do
    cohort = %{
      status: :candidate_cohorts, reason: :shared_sentence_conditioned_lexical_evidence,
      base_lexeme: "plant", eligible_sense_count: 3, cohort_count: 1,
      participant_surfaces: %{actor: "the gardener", predicate: "plants",
        patient: "a tree"},
      semantic_equivalence_verified?: false, chose_exact_sense?: false,
      modified_lifg?: false, changed_response?: false,
      cohorts: [
        %{
          evidence_origin: :patient_gloss, anchor_term: "plant",
          member_count: 2, member_ids_truncated?: false,
          shared_definition_terms: ["plant"], minimum_member_support: 0.1015,
          shared_meaning_verified?: false,
          members: [
            %{id: "plant|verb|1", direct_fit: 0.0,
              patient_support: 0.1015, actor_support: 0.0,
              diagnostic_score: 0.1015,
              distinguishing_definition_terms: ["soil", "grow"]},
            %{id: "plant|verb|2", direct_fit: 0.0,
              patient_support: 0.1015, actor_support: 0.0,
              diagnostic_score: 0.1015,
              distinguishing_definition_terms: ["area", "vegetation"]}
          ]
        }
      ]
    }

    source = si(:power)
    with_trace =
      Map.put(source, :trace, [%{
        stage: :semantic_sense_shadow, meta: %{
          mode: :shadow_only, status: :abstained,
          semantic_cohort: cohort
        }
      }])

    evidence = ExplainEvidence.from_si(with_trace)
    assert evidence.semantic_cohort == cohort
    rendered = payload(with_trace)
    section = section(rendered, :semantic_cohort)
    assert section.title == "Semantic cohorts (shadow only)"
    assert section.hint =~ "NOT sense equivalence"
    assert Enum.any?(section.items, fn item ->
      item.label == "Cohort audit" and
        item.body =~ "exact sense selected?: false" and
        item.body =~ "LIFG modified?: false"
    end)
    assert Enum.any?(section.items, fn item ->
      item.label == "Shared cue: plant (:patient_gloss)" and
        item.body =~ "plant|verb|1" and
        item.body =~ "plant|verb|2" and
        item.body =~ "soil" and
        item.body =~ "vegetation"
    end)

    cached = Modal.enrich_saved_payload(%{title: "Explain", sections: []},
      with_trace)
    assert section(cached, :semantic_cohort) != nil
    assert Modal.enrich_saved_payload(cached, with_trace) == cached

    assert section(payload(source), :semantic_cohort) == nil
    unrelated = Map.put(source, :trace, [%{
      stage: :unrelated, meta: %{mode: :shadow_only,
        semantic_cohort: cohort}
    }])
    assert section(payload(unrelated), :semantic_cohort) == nil
  end

  test "Explain cannot invent a shadow semantic sense from word winners alone" do
    source = si(:power)
    assert ExplainEvidence.from_si(source).semantic_sense_shadow == nil
    assert section(payload(source), :semantic_sense_shadow) == nil

    forged = Map.put(source, :trace, [%{
      stage: :unrelated, meta: %{mode: :shadow_only,
        proposed_id: "fake|verb|999"}
    }])
    assert section(payload(forged), :semantic_sense_shadow) == nil
  end

  test "memory and ordinary replies do not fabricate runtime semantic sections" do
    assert ExplainEvidence.from_si(nil) == %{}
    assert ExplainEvidence.from_si(%{text: "Your name is... "}) == %{}

    result = Modal.explain_payload_for(%{
      text: "Your name is Mary.",
      intent: :name_query,
      from: %{response_source: :memory, memory_key: :user_name}
    })

    assert section(result, :verified_phrasal_meanings) == nil
    assert section(result, :semantic_choices) == nil
    assert section(result, :comprehension) == nil
    assert section(result, :intent_tone) != nil
  end

  test "symbolic Explain identifies the lexical witness without calling it an LLM review" do
    payload =
      Modal.explain_payload_for(%{
        text: "Hello.",
        from: %{
          response_reflection: %{
            status: :symbolic_realized,
            speech_act: :reciprocate_greeting,
            provenance: :canonical_lifg_choice,
            lexical_witness: %{id: "hello|interjection|1", score: 0.16},
            speaker: :user,
            addressee: :self,
            vocative: %{surface: "Symbrella", resolution: :assistant_identity},
            semantic_verified?: false,
            llm_called?: false
          }
        }
      })

    section = Enum.find(payload.sections, &(&1.key == :response_reflection))
    assert section.title == "Symbolic realization"
    assert section.tag == ":symbolic_realized"
    fields = Map.new(section.items, &{&1.label, &1.body})
    assert fields["lexical witness"] == "hello|interjection|1"
    assert fields["speech act"] == ":reciprocate_greeting"
    assert fields["input speaker"] == ":user"
    assert fields["input addressee"] == ":self"
    assert fields["vocative resolution"] == ":assistant_identity"
    assert fields["sentence meaning verified?"] == "false"
    assert fields["LLM called?"] == "false"
  end

end
