defmodule Core.Syntax.SignaturesTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.Syntax.Signatures
  alias Core.TokenFilters

  # Minimal deterministic dictionary fixture. No DB reads or mutations.
  @cells [
    %{id: "got|verb|1", norm: "got", pos: "verb", semantic_atoms: ["tag:no-past-participle"]},
    %{id: "got|verb|4", norm: "got", pos: "verb", semantic_atoms: ["tag:form-of", "tag:past"]},
    %{
      id: "got|verb|5",
      norm: "got",
      pos: "verb",
      semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]
    },
    %{id: "leave|verb|1", norm: "leave", pos: "verb", semantic_atoms: []},
    %{id: "study|verb|1", norm: "study", pos: "verb", semantic_atoms: []},
    %{id: "the|determiner|1", norm: "the", pos: "determiner", semantic_atoms: []},
    %{id: "station|noun|1", norm: "station", pos: "noun", semantic_atoms: []},
    %{
      id: "running|verb|1",
      norm: "running",
      pos: "verb",
      semantic_atoms: ["tag:form-of", "tag:participle", "tag:present"]
    }
  ]

  @question_cells [
    %{norm: "what", pos: "pronoun"},
    %{norm: "do", pos: "auxiliary"},
    %{norm: "don't", pos: "auxiliary"},
    %{norm: "you", pos: "pronoun"},
    %{norm: "know", pos: "verb"},
    %{norm: "about", pos: "preposition"},
    %{norm: "japan", pos: "proper_noun"},
    %{norm: "exactly", pos: "adverb"},
    %{norm: "actually", pos: "adverb"},
    %{norm: "really", pos: "adverb"},
    %{norm: "not", pos: "adverb"}
  ]

  defp input(sentence, cells \\ @cells) do
    %SemanticInput{sentence: sentence, active_cells: cells}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  defp ids(sentence, cells \\ @cells) do
    sentence
    |> input(cells)
    |> Signatures.detect()
    |> Enum.map(& &1.id)
  end

  test "complete attested regular-past transitive frame distinguishes predicate from noun reading" do
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "checker", pos: "noun"},
      %{norm: "verified", pos: "noun"},
      %{norm: "verified", pos: "verb"},
      %{norm: "those", pos: "determiner"},
      %{norm: "claims", pos: "noun"}
    ]

    complete = Signatures.detect(input("The checker verified those claims.", cells))

    assert %{id: :bounded_regular_past_transitive,
             roles: %{subject_head: 1, predicate: 2, object_head: 4},
             source: :core_syntax} =
             Enum.find(complete, &(&1.id == :bounded_regular_past_transitive))

    for sentence <- [
          "The checker verified.",
          "The checker not verified those claims.",
          "The checker might verify those claims.",
          "The checker classified those claims."
        ] do
      refute :bounded_regular_past_transitive in ids(sentence, cells)
    end

    refute :bounded_regular_past_transitive in
             ids("The checker verified those claims.",
               Enum.reject(cells, &(&1.norm == "verified" and &1.pos == "verb")))
  end

  test "complete copular relative clause witnesses a lower-case noun complement" do
    cells = [
      %{norm: "links", pos: "noun"},
      %{norm: "were", pos: "verb"},
      %{norm: "lines", pos: "noun"},
      %{norm: "lines", pos: "proper_noun"},
      %{norm: "refute", pos: "verb"},
      %{norm: "the", pos: "determiner"},
      %{norm: "claim", pos: "noun"}
    ]

    clauses = Signatures.detect(input("Links were lines that refute the claim.", cells))
    assert %{id: :copular_relative_classification,
             roles: %{subject_head: 0, copula: 1, complement_head: 2,
               relative_marker: 3, relative_verb: 4,
               object_determiner: 5, object_head: 6},
             source: :core_syntax} =
             Enum.find(clauses, &(&1.id == :copular_relative_classification))

    for unsupported <- [
          "Links were Lines that refute the claim.",
          "Links were lines that maybe refute the claim.",
          "Links were not lines that refute the claim."
        ] do
      refute :copular_relative_classification in ids(unsupported, cells)
    end

    refute :copular_relative_classification in
             ids("Links were lines that refute the claim.",
               Enum.reject(cells, &(&1.norm == "refute")))
  end

  test "simple-past candidate is found without selecting a BrainCell" do
    assert :finite_past_candidate in ids("I got sick.")
    assert :finite_past_candidate in ids("I went to the doctor and got sick.")
    assert :finite_past_candidate in ids("They got to the station.")
  end

  test "adjective nominalization before a perfect predicate is observation only" do
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "gathered", pos: "adjective"},
      %{norm: "had", pos: "auxiliary"},
      %{norm: "grown", pos: "verb",
        semantic_atoms: ["tag:form-of", "tag:past", "tag:participle"]}
    ]

    signatures = Signatures.detect(input("The gathered had grown.", cells))

    assert [%{
      id: :nominalized_adjective_subject_candidate,
      span: {0, 1},
      roles: %{determiner: 0, adjective: 1},
      required_features: %{
        determiner: [:pos_determiner], adjective: [:pos_adjective]
      }
    }] =
      Enum.filter(signatures, &(&1.id == :nominalized_adjective_subject_candidate))

    refute Enum.any?(signatures, fn sig ->
      sig.id == :noun_phrase and sig.span == {0, 1}
    end)

    for sentence <- [
      "The gathered walked.",
      "The gathered had quickly grown.",
      "The gathered had."
    ] do
      refute :nominalized_adjective_subject_candidate in ids(sentence, cells)
    end

    refute :nominalized_adjective_subject_candidate in
      ids("The gathered had grown.",
        Enum.reject(cells, &(&1.norm == "gathered" and &1.pos == "adjective")))

    refute :nominalized_adjective_subject_candidate in
      ids("The gathered had grown.",
        Enum.reject(cells, &(&1.norm == "grown")))
  end

  test "have plus past participle supports a perfect construction" do
    assert :perfect_predicate in ids("I have got very cold.")
    assert :perfect_predicate in ids("They had got very cold.")
  end

  test "have + verb + to + verb is not confused with to + determiner" do
    assert :have_verb_to_infinitive in ids("I've got to study.")
    assert :have_verb_to_infinitive in ids("I've got to leave.")
    refute :have_verb_to_infinitive in ids("I have got to the station.")
    assert :perfect_predicate in ids("I have got to the station.")
  end

  test "ambiguous 's permits a have analysis without fixing contraction meaning" do
    signatures = Signatures.detect(input("He's got to leave."))
    assert Enum.any?(signatures, &(&1.id == :have_verb_to_infinitive))

    assert Enum.any?(signatures, fn s ->
             s.id == :perfect_predicate and :have_or_be_contraction in s.evidence
           end)

    refute :perfect_predicate in ids("He's running.")
  end

  test "unavailable dictionary evidence does not fabricate a construction" do
    assert [] == Signatures.detect(input("I have got to leave.", []))
  end

  test "signatures use original token indices" do
    signatures = Signatures.detect(input("I've got to leave."))
    sig = Enum.find(signatures, &(&1.id == :have_verb_to_infinitive))
    assert sig.span == {0, 3}
    assert sig.roles == %{auxiliary: 0, verb: 1, infinitive_marker: 2, complement: 3}
  end

  test "verb-particle grammar preserves role indices across both surface orders" do
    cells = [
      %{norm: "turn", pos: "verb"},
      %{norm: "off", pos: "particle"},
      %{norm: "the", pos: "determiner"},
      %{norm: "lights", pos: "noun"}
    ]

    adjacent =
      Signatures.detect(input("turn off the lights", cells))
      |> Enum.filter(&(&1.id == :verb_particle_construction))

    assert [%{span: {0, 1}, roles: %{verb: 0, particle: 1}}] = adjacent
    assert :adjacent_particle in hd(adjacent).evidence

    separated =
      Signatures.detect(input("turn the lights off", cells))
      |> Enum.filter(&(&1.id == :verb_particle_construction))

    assert [%{
      span: {0, 3},
      roles: %{verb: 0, determiner: 1, object: 2, particle: 3}
    }] = separated
    assert :separated_particle in hd(separated).evidence
  end

  test "real dictionary adverb/preposition particle senses support a separated verb" do
    # Live smoke test showed no off|particle sense, only lexical adverb,
    # preposition and noun alternatives. Syntax should preserve those IDs.
    cells = [
      %{norm: "turn", pos: "verb"},
      %{norm: "the", pos: "determiner"},
      %{norm: "the", pos: "preposition"},
      %{norm: "lights", pos: "verb"},
      %{norm: "lights", pos: "noun"},
      %{norm: "off", pos: "adverb"},
      %{norm: "off", pos: "preposition"},
      %{norm: "off", pos: "noun"},
      %{norm: "off", pos: "adjective"}
    ]

    si = input("Turn the lights off.", cells)
    signatures = Signatures.detect(si)

    assert Enum.any?(signatures, fn
             %{
               id: :verb_particle_construction, span: {0, 3},
               roles: %{verb: 0, determiner: 1, object: 2, particle: 3},
               required_features: %{particle: particle_pos}
             } -> :pos_adverb in particle_pos and :pos_preposition in particle_pos
             _ -> false
           end)

    assert si.active_cells == cells
  end

  test "an unrelated lexical particle POS does not invent a phrasal verb" do
    cells = [
      %{norm: "you", pos: "verb"},
      %{norm: "you", pos: "pronoun"},
      %{norm: "know", pos: "particle"},
      %{norm: "know", pos: "noun"}
    ]

    refute :verb_particle_construction in ids("you know", cells)
    refute :verb_particle_construction in ids("do you know", cells)
  end

  test "a general manner adverb never becomes a verb particle" do
    cells = [
      %{norm: "walk", pos: "verb"},
      %{norm: "quickly", pos: "adverb"},
      %{norm: "off", pos: "noun"}
    ]

    refute :verb_particle_construction in ids("walk quickly", cells)
    refute :verb_particle_construction in ids("walk off", cells)
  end

  test "bare noun and pronoun objects can separate verb and particle" do
    cells = [
      %{norm: "turn", pos: "verb"},
      %{norm: "lights", pos: "noun"},
      %{norm: "it", pos: "pronoun"},
      %{norm: "off", pos: "particle"}
    ]

    for {text, object} <- [{"turn lights off", 1}, {"turn it off", 1}] do
      matches =
        Signatures.detect(input(text, cells))
        |> Enum.filter(&(&1.id == :verb_particle_construction))

      assert [%{roles: %{verb: 0, object: ^object, particle: 2}}] = matches
    end
  end

  test "verb-object grammar is not mistaken for a verb-particle construction" do
    cells = [
      %{norm: "open", pos: "verb"},
      %{norm: "the", pos: "determiner"},
      %{norm: "file", pos: "noun"}
    ]

    refute :verb_particle_construction in ids("open the file", cells)
    refute :verb_particle_construction in ids("open file", cells)
  end

  test "a period between verb and particle blocks grammatical phrase evidence" do
    cells = [%{norm: "turn", pos: "verb"}, %{norm: "off", pos: "particle"}]

    refute :verb_particle_construction in ids("turn. off", cells)
    assert :verb_particle_construction in ids("turn off!", cells)
  end

  test "grammatical alternatives are retained without inventing senses" do
    cells = [
      %{norm: "turn", pos: "noun"},
      %{norm: "turn", pos: "verb"},
      %{norm: "off", pos: "particle"}
    ]
    si = input("turn off", cells)
    out = Signatures.attach(si)

    assert :verb_particle_construction in Enum.map(out.sentence_signatures, & &1.id)
    assert out.active_cells == si.active_cells
    assert out.sense_candidates == si.sense_candidates
    assert :ok = SemanticInput.validate_contract(out)
  end

  test "noun phrases preserve determiner, modifier and head roles" do
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "quiet", pos: "adjective"},
      %{norm: "garden", pos: "noun"}
    ]

    signatures = Signatures.detect(input("the quiet garden", cells))

    assert Enum.any?(signatures, fn sig ->
             sig.id == :noun_phrase and sig.span == {0, 2} and
               sig.roles == %{determiner: 0, modifier: 1, head: 2} and
               sig.required_features.head == [:pos_noun]
           end)

    assert Enum.any?(signatures, fn sig ->
             sig.id == :noun_phrase and sig.span == {1, 2} and
               sig.roles == %{modifier: 1, head: 2}
           end)
  end

  test "genitive noun phrase composes the live railgun topic without a Japan's sense" do
    cells = [
      %{norm: "about", pos: "preposition"},
      %{norm: "railgun", pos: "noun"},
      %{norm: "development", pos: "noun"}
    ]

    si = input("What do you know about Japan's railgun development?", cells)
    signatures = Signatures.detect(si)

    assert [%{
      id: :possessive_noun_phrase,
      span: {5, 7},
      surface_possessor: "Japan",
      roles: %{possessor: 5, modifier: 6, head: 7},
      evidence: evidence
    }] = Enum.filter(signatures, &(&1.id == :possessive_noun_phrase))

    assert :apostrophe_s in evidence
    assert :possessor_not_lexically_verified in evidence

    assert Enum.any?(signatures, fn
             %{id: :prepositional_phrase, span: {4, 7},
               roles: %{preposition: 4, possessor: 5, modifier: 6,
                        head: 7, object: 7}} -> true
             _ -> false
           end)

    out = Signatures.attach(si)
    assert out.active_cells == cells
    assert out.sense_candidates == si.sense_candidates
    assert :ok = SemanticInput.validate_contract(out)
  end

  test "a single nominal head and an adjective nominal head support possession" do
    for {sentence, cells, span, head} <- [
          {"Mary's car", [%{norm: "car", pos: "noun"}], {0, 1}, 1},
          {"the company's annual results",
           [%{norm: "annual", pos: "adjective"}, %{norm: "results", pos: "noun"}],
           {1, 3}, 3}
        ] do
      assert Enum.any?(Signatures.detect(input(sentence, cells)), fn
               %{id: :possessive_noun_phrase, span: ^span,
                 roles: %{head: ^head}} -> true
               _ -> false
             end)
    end
  end

  test "possessive syntax does not treat contractions or punctuation as a genitive" do
    cells = [
      %{norm: "time", pos: "noun"},
      %{norm: "rain", pos: "noun"},
      %{norm: "home", pos: "noun"},
      %{norm: "car", pos: "noun"},
      %{norm: "the", pos: "determiner"}
    ]

    for sentence <- [
          "It's time", "There's rain", "He's home",
          "John's. car", "John's the car"
        ] do
      refute :possessive_noun_phrase in ids(sentence, cells)
    end

    refute :possessive_noun_phrase in ids("Mary's unknown", [])
  end

  test "prepositional phrases compose around noun phrase structure" do
    cells = [
      %{norm: "in", pos: "preposition"},
      %{norm: "the", pos: "determiner"},
      %{norm: "quiet", pos: "adjective"},
      %{norm: "garden", pos: "noun"}
    ]

    signatures = Signatures.detect(input("in the quiet garden", cells))

    assert Enum.any?(signatures, fn sig ->
             sig.id == :noun_phrase and sig.span == {1, 3}
           end)

    assert Enum.any?(signatures, fn sig ->
             sig.id == :prepositional_phrase and sig.span == {0, 3} and
               sig.roles == %{
                 preposition: 0, determiner: 1, modifier: 2,
                 head: 3, object: 3
               }
           end)
  end

  test "bare nouns and pronouns form prepositional complements" do
    cells = [
      %{norm: "near", pos: "preposition"},
      %{norm: "home", pos: "noun"},
      %{norm: "with", pos: "preposition"},
      %{norm: "her", pos: "pronoun"}
    ]

    for {sentence, index} <- [{"near home", 1}, {"with her", 1}] do
      assert Enum.any?(Signatures.detect(input(sentence, cells)), fn sig ->
               sig.id == :prepositional_phrase and sig.span == {0, index} and
                 sig.roles == %{preposition: 0, object: index}
             end)
    end
  end

  test "grammar cannot cross punctuation or token index gaps" do
    cells = [
      %{norm: "in", pos: "preposition"},
      %{norm: "the", pos: "determiner"},
      %{norm: "garden", pos: "noun"}
    ]

    signatures = Signatures.detect(input("in. the garden", cells))

    refute Enum.any?(signatures, fn sig ->
             sig.id == :prepositional_phrase
           end)

    assert Enum.any?(signatures, fn sig ->
             sig.id == :noun_phrase and sig.span == {1, 2}
           end)

    refute :noun_phrase in ids("the. garden", cells)
  end

  test "an infinitive verb is not misclassified as a preposition noun complement" do
    cells = [
      %{norm: "to", pos: "preposition"},
      %{norm: "leave", pos: "verb"},
      %{norm: "the", pos: "determiner"},
      %{norm: "station", pos: "noun"}
    ]

    refute :prepositional_phrase in ids("to leave", cells)

    assert Enum.any?(Signatures.detect(input("to the station", cells)), fn sig ->
             sig.id == :prepositional_phrase and sig.span == {0, 2}
           end)
  end

  test "competing POS interpretations survive constituent recognition" do
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "record", pos: "verb"},
      %{norm: "record", pos: "noun"}
    ]

    si = input("the record", cells)
    out = Signatures.attach(si)

    assert Enum.any?(out.sentence_signatures, fn sig ->
             sig.id == :noun_phrase and sig.roles == %{determiner: 0, head: 1}
           end)

    assert out.tokens == si.tokens
    assert out.active_cells == si.active_cells
    assert out.sense_candidates == si.sense_candidates
    assert :ok = SemanticInput.validate_contract(out)
  end

  test "attach preserves sense candidates and records standard trace" do
    si = input("I have got very cold.")
    si = %{si | sense_candidates: %{2 => [%{id: "got|verb|4", score: 0.7}]}}
    out = Signatures.attach(si)

    assert %SemanticInput{} = out
    assert out.sense_candidates == si.sense_candidates
    assert out.active_cells == si.active_cells
    assert :perfect_predicate in Enum.map(out.sentence_signatures, & &1.id)
    assert [%{stage: :sentence_signatures, decision: :observed, meta: meta} | _] = out.trace
    assert meta.count >= 1
    assert :ok = SemanticInput.validate_contract(out)
  end

  test "WH-topic signature retains token roles around adverbs and negation" do
    examples = [
      {"What exactly do you actually know about Japan?",
       %{wh: 0, wh_modifier: 1, auxiliary: 2, subject: 3,
         predicate_modifier: 4, predicate: 5, preposition: 6}},
      {"What do you actually know about Japan?",
       %{wh: 0, auxiliary: 1, subject: 2, predicate_modifier: 3,
         predicate: 4, preposition: 5}},
      {"What do you not actually know about Japan?",
       %{wh: 0, auxiliary: 1, subject: 2, negation: 3,
         predicate_modifier: 4, predicate: 5, preposition: 6}},
      {"What do you really not know about Japan?",
       %{wh: 0, auxiliary: 1, subject: 2, predicate_modifier: 3,
         negation: 4, predicate: 5, preposition: 6}}
    ]

    for {sentence, expected_roles} <- examples do
      si = input(sentence, @question_cells)
      assert [%{roles: ^expected_roles} = frame] =
               Enum.filter(Signatures.detect(si), &(&1.id == :wh_aux_topic_question))
      assert frame.span == {0, length(si.tokens) - 1}
      assert frame.required_features.predicate == [:pos_verb]
      assert :explicit_prepositional_topic in frame.evidence
      assert si.active_cells == @question_cells
    end
  end

  test "negative auxiliary contractions stay one base-token role" do
    si = input("What don't you know about Japan?", @question_cells)

    assert [%{roles: %{wh: 0, auxiliary: 1, subject: 2, predicate: 3,
                       preposition: 4} = roles, evidence: evidence}] =
             Enum.filter(Signatures.detect(si), &(&1.id == :wh_aux_topic_question))
    refute Map.has_key?(roles, :negation)
    assert :negative_auxiliary_contraction in evidence
  end

  test "WH question does not leap over unknown words or punctuation" do
    without_adverb = Enum.reject(@question_cells, &(&1.norm == "exactly"))

    for {sentence, cells} <- [
          {"What exactly do you know about Japan?", without_adverb},
          {"What mystery do you know about Japan?", @question_cells},
          {"What exactly, do you know about Japan?", @question_cells},
          {"What do you actually know?", @question_cells},
          {"What do you actually know about quickly?", @question_cells}
        ] do
      refute Enum.any?(Signatures.detect(input(sentence, cells)),
               &(&1.id == :wh_aux_topic_question))
    end
  end
  test "counted plural present SVO requires attested plural noun and base verb" do
    cells = [
      %{norm: "reports", pos: "noun",
        semantic_atoms: ["tag:form-of", "tag:plural"]},
      %{norm: "support", pos: "noun", semantic_atoms: []},
      %{norm: "support", pos: "verb", semantic_atoms: []},
      %{norm: "the", pos: "determiner", semantic_atoms: []},
      %{norm: "claims", pos: "noun", semantic_atoms: []}
    ]

    sentence = "Four reports support the claims."
    assert [%{id: :counted_finite_present_svo_candidate,
             span: {0, 4},
             roles: %{count: 0, subject_head: 1, predicate: 2,
                      object_determiner: 3, object_head: 4},
             source: :core_syntax}] =
             Signatures.detect(input(sentence, cells))
             |> Enum.filter(&(&1.id == :counted_finite_present_svo_candidate))

    no_plural = Enum.reject(cells, &(&1.norm == "reports"))
    no_verb = Enum.reject(cells, &(&1.norm == "support" and &1.pos == "verb"))
    past_form =
      Enum.map(cells, fn
        %{norm: "support", pos: "verb"} = cell ->
          %{cell | semantic_atoms: ["tag:form-of", "tag:past"]}
        cell -> cell
      end)

    for bad <- [no_plural, no_verb, past_form] do
      refute :counted_finite_present_svo_candidate in ids(sentence, bad)
    end

    refute :counted_finite_present_svo_candidate in
             ids("One report support the claims.", cells)
    refute :counted_finite_present_svo_candidate in
             ids("Four reports might support the claims.", cells)
    refute :counted_finite_present_svo_candidate in
             ids("Four reports support. the claims.", cells)
  end

  test "finite present subject–verb–object clause uses exact verb morphology" do
    cells = [
      %{norm: "the", pos: "determiner"},
      %{norm: "artist", pos: "noun"},
      %{norm: "draws", pos: "noun", semantic_atoms: ["tag:form-of", "tag:plural"]},
      %{norm: "draws", pos: "verb",
        semantic_atoms: ["tag:form-of", "tag:indicative", "tag:present",
                         "tag:singular", "tag:third-person"]},
      %{norm: "a", pos: "determiner"},
      %{norm: "picture", pos: "noun"}
    ]

    clauses = Signatures.detect(input("The artist draws a picture.", cells))
    assert [%{id: :finite_present_svo_candidate,
              span: {0, 4},
              roles: %{subject_head: 1, predicate: 2, object_head: 4}}] =
             Enum.filter(clauses, &(&1.id == :finite_present_svo_candidate))

    refute :finite_present_svo_candidate in ids("The artist draws. a picture.", cells)

    nouns_only = Enum.reject(cells, &(&1.norm == "draws" and &1.pos == "verb"))
    refute :finite_present_svo_candidate in ids("The artist draws a picture.", nouns_only)
  end

  test "finite-present construction generalizes but does not promote first-person agreement" do
    cells = [
      %{norm: "the", pos: "determiner"}, %{norm: "painter", pos: "noun"},
      %{norm: "paints", pos: "verb",
        semantic_atoms: ["tag:form-of", "tag:indicative", "tag:present",
                         "tag:singular", "tag:third-person"]},
      %{norm: "a", pos: "determiner"}, %{norm: "wall", pos: "noun"},
      %{norm: "she", pos: "pronoun"}, %{norm: "i", pos: "pronoun"}
    ]

    assert :finite_present_svo_candidate in ids("The painter paints a wall.", cells)
    assert :finite_present_svo_candidate in ids("She paints a wall.", cells)
    refute :finite_present_svo_candidate in ids("I paints a wall.", cells)
    refute :finite_present_svo_candidate in ids("She paints. a wall.", cells)
  end


  test "complete definition imperatives annotate verb and common-noun object roles" do
    cells = [
      %{id: "define|verb|1", norm: "define", pos: "verb"},
      %{id: "define|noun|1", norm: "define", pos: "noun"},
      %{id: "artist|noun|1", norm: "artist", pos: "noun"},
      %{id: "artist|proper_noun|1", norm: "artist", pos: "proper_noun"},
      %{id: "the|determiner|1", norm: "the", pos: "determiner"}
    ]

    for {sentence, predicate, object} <- [
      {"Define artist.", 0, 1},
      {"please define artist", 1, 2},
      {"Define the artist.", 0, 2},
      {"Please define the artist.", 1, 3}
    ] do
      si = input(sentence, cells) |> Map.put(:intent, :define)
      signature = Enum.find(Signatures.detect(si), &(&1.id == :definition_imperative))

      assert %{source: :core_syntax, roles: %{predicate: ^predicate,
                                            object_head: ^object}} = signature
      assert signature.span == {0, object}
      assert :attested_common_noun_object in signature.evidence
      assert :attested_verb_alternative in signature.evidence
    end
  end

  test "definition signatures abstain on unsupported forms and absent POS witnesses" do
    cells = [
      %{id: "define|verb|1", norm: "define", pos: "verb"},
      %{id: "artist|noun|1", norm: "artist", pos: "noun"}
    ]

    for {sentence, intent} <- [
      {"Define artist now", :define},
      {"Define artist and painter", :define},
      {"The define artist", :define},
      {"Define artist", :unknown},
      {"Artist define", :define},
      {"Define artist about painting", :define}
    ] do
      si = input(sentence, cells) |> Map.put(:intent, intent)
      refute Enum.any?(Signatures.detect(si), &(&1.id == :definition_imperative)),
             sentence
    end

    noun_only = Enum.reject(cells, &(&1.pos == "verb"))
    refute :definition_imperative in
      (input("Define artist", noun_only)
       |> Map.put(:intent, :define) |> Signatures.detect() |> Enum.map(& &1.id))

    verb_only = Enum.reject(cells, &(&1.pos == "noun"))
    refute :definition_imperative in
      (input("Define artist", verb_only)
       |> Map.put(:intent, :define) |> Signatures.detect() |> Enum.map(& &1.id))
  end


  test "polite commands classified as :command still supply evidenced definition roles" do
    cells = [
      %{id: "define|verb|1", norm: "define", pos: "verb"},
      %{id: "define|noun|1", norm: "define", pos: "noun"},
      %{id: "artist|noun|1", norm: "artist", pos: "noun"},
      %{id: "artist|proper_noun|1", norm: "artist", pos: "proper_noun"}
    ]

    si = input("please define artist", cells) |> Map.put(:intent, :command)
    assert [%{roles: %{predicate: 1, object_head: 2},
              source: :core_syntax}] =
      Enum.filter(Signatures.detect(si), &(&1.id == :definition_imperative))

    refute :definition_imperative in
      (input("please define artist now", cells)
       |> Map.put(:intent, :command)
       |> Signatures.detect()
       |> Enum.map(& &1.id))
  end

end
