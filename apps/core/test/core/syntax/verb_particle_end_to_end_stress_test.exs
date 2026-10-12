defmodule Core.Syntax.VerbParticleEndToEndStressTest do
  use ExUnit.Case, async: false

  alias Core.{SemanticInput, TokenFilters, TokenResolution}
  alias Core.Intent.Selection
  alias Core.MWE.{Lexicon, LIFGBridge}
  alias Core.Syntax.Signatures

  # Deliberately includes conflicting POS alternatives from the real smoke test:
  # "lights" is both noun and verb; "off" is adverb and preposition.
  # No synthetic off|particle entry is needed.
  @cells [
    %{norm: "turn", pos: "verb"},
    %{norm: "turn", pos: "noun"},
    %{norm: "switch", pos: "verb"},
    %{norm: "power", pos: "verb"},
    %{norm: "the", pos: "determiner"},
    %{norm: "the", pos: "preposition"},
    %{norm: "lights", pos: "noun"},
    %{norm: "lights", pos: "verb"},
    %{norm: "fan", pos: "noun"},
    %{norm: "computer", pos: "noun"},
    %{norm: "it", pos: "pronoun"},
    %{norm: "off", pos: "adverb"},
    %{norm: "off", pos: "preposition"},
    %{norm: "off", pos: "noun"},
    %{norm: "down", pos: "adverb"},
    %{norm: "quickly", pos: "adverb"}
  ]

  defp pipeline(sentence, lexical_rows \\ []) do
    %SemanticInput{sentence: sentence, active_cells: @cells, trace: []}
    |> TokenFilters.rebuild_word_ngrams(3)
    |> Map.from_struct()
    |> Selection.select()
    |> Lexicon.attach(mwe_lookup: fn _phrases -> lexical_rows end)
    |> Signatures.attach()
    |> Selection.refine_from_syntax()
    |> TokenResolution.resolve()
  end

  defp has_roles?(si, roles) do
    Enum.any?(si.sentence_signatures, fn
      %{id: :verb_particle_construction, roles: found} ->
        Enum.all?(roles, fn {role, index} -> Map.get(found, role) == index end)

      _ ->
        false
    end)
  end

  test "canonical separated imperative retains object and rejects incompatible dictionary MWE" do
    rows = [
      %{id: "lights off|verb|1", norm: "lights off", pos: "verb",
        definition: "Third-person present of light off"}
    ]

    si = pipeline("Turn the lights off.", rows)

    assert has_roles?(si, %{verb: 0, determiner: 1, object: 2, particle: 3})
    assert si.intent == :command
    assert si.confidence >= 0.80
    assert si.conversation_act == :instruction
    assert si.context_frame.intent == :command
    assert si.context_frame.conversation_act == :instruction
    assert si.context_frame.certainty == :high

    assert Enum.any?(si.mwe_lexicon, &(&1.phrase == "lights off"))
    refute Enum.any?(si.token_cover, fn
             %{kind: :phrase, token_span: {2, 3}} -> true
             _ -> false
           end)

    {input, virtual} = LIFGBridge.prepare(si)
    assert virtual == %{}
    assert input.tokens == si.tokens
    assert Enum.map(si.tokens, & &1.index) == [0, 1, 2, 3]
  end

  test "word order and object type do not block structural imperatives" do
    cases = [
      {"Turn the lights off.", %{verb: 0, determiner: 1, object: 2, particle: 3}},
      {"Turn off the lights.", %{verb: 0, particle: 1}},
      {"Turn it off!", %{verb: 0, object: 1, particle: 2}},
      {"Turn lights off!", %{verb: 0, object: 1, particle: 2}},
      {"Switch the fan off.", %{verb: 0, determiner: 1, object: 2, particle: 3}},
      {"Power the computer down.", %{verb: 0, determiner: 1, object: 2, particle: 3}}
    ]

    for {sentence, roles} <- cases do
      si = pipeline(sentence)

      assert has_roles?(si, roles), "Missing grammatical frame for #{inspect(sentence)}"
      assert si.intent == :command, "Wrong intent for #{inspect(sentence)}: #{inspect(si.intent)}"
      assert si.conversation_act == :instruction
    end
  end

  test "questions, declarative subjects, adverbs and punctuation are not false imperatives" do
    cases = [
      "Turn the lights off?",
      "They turn the lights off.",
      "I turn the lights off.",
      "The lights turn off.",
      "Turn the lights quickly.",
      "Turn. the lights off."
    ]

    for sentence <- cases do
      si = pipeline(sentence)

      refute Enum.any?(si.trace, fn
               %{stage: :intent_refinement, reason: :initial_verb_particle_imperative} -> true
               _ -> false
             end), "Incorrect grammar-backed command for #{inspect(sentence)}"
    end
  end

  test "real verb MWE survives where no other verb claims its noun-object span" do
    si =
      pipeline("He lights off the rocket.", [
        %{id: "lights off|verb|1", norm: "lights off", pos: "verb"}
      ])

    assert Enum.any?(si.token_cover, fn
             %{kind: :phrase, token_span: {1, 2}, dictionary_backed?: true} -> true
             _ -> false
           end)

    {_input, virtual} = LIFGBridge.prepare(si)
    assert map_size(virtual) == 1
    refute Enum.any?(si.trace, fn
             %{stage: :intent_refinement} -> true
             _ -> false
           end)
  end

  test "repeated deterministic input does not vary syntax or MWE cover" do
    rows = [%{id: "lights off|verb|1", norm: "lights off", pos: "verb"}]

    results =
      for _ <- 1..8 do
        si = pipeline("Turn the lights off.", rows)
        {si.intent,
         Enum.map(si.sentence_signatures, &{&1.id, &1.span, &1.roles}),
         Enum.map(si.token_cover, &{&1.kind, &1.phrase, Map.get(&1, :token_span)})}
      end

    assert length(Enum.uniq(results)) == 1
  end
end
