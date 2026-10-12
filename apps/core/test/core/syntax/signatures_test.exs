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

  test "simple-past candidate is found without selecting a BrainCell" do
    assert :finite_past_candidate in ids("I got sick.")
    assert :finite_past_candidate in ids("I went to the doctor and got sick.")
    assert :finite_past_candidate in ids("They got to the station.")
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
end
