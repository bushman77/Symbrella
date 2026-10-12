defmodule SymbrellaWeb.HomeLive.ExplainEvidenceTest do
  use ExUnit.Case, async: true

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
        understood: [if(word_order == :power, do: "computer", else: "lights"), phrase],
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

    assert section(result, :runtime_decision).items
           |> Enum.any?(&(&1.label == "selected_action" and &1.body == ":answer_user"))

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
               &(&1.label == "understood" and &1.body =~ "turn off"))
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
    assert section(upgraded, :verified_phrasal_meanings).items
           |> Enum.any?(&(&1.label == "power down|verb|1"))
    assert section(upgraded, :semantic_choices).items
           |> Enum.any?(&(&1.label == "computer|noun|3"))

    assert Modal.enrich_saved_payload(original, %{intent: :unknown}) == original
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
end
