defmodule Brain.LIFG.ContextFitWindowTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.ContextFit

  test "unrelated distant words do not boost a sense" do
    sentence =
      "I went to the bank to get some cash, but first I sat by the riverbank and watched the current."

    wrong = %{
      definition: "To become ill with or catch a disease.",
      example: "I went on holiday and got malaria."
    }

    result = ContextFit.explain(sentence, "get", wrong)

    assert result.score == 0.0
    assert result.matches.example == []
  end

  test "plural object agrees with singular dictionary definition without changing senses" do
    deactivate = %{
      definition: "To deactivate an appliance, light, mechanism, or functionality.",
      example: "Turn off the machine and unplug it."
    }

    assert ContextFit.score("Turn the lights off.", "turn off", deactivate) > 0.0
    assert ContextFit.score("Turn off the lights.", "turn off", deactivate) > 0.0

    gloss = ContextFit.explain("Turn the lights off.", "turn off", deactivate)
    assert "light" in gloss.matches.definition
  end

  test "a different object does not produce artificial light evidence" do
    deactivate = %{definition: "To deactivate a light or lamp."}

    assert ContextFit.score("Turn off the employees.", "turn off", deactivate) == 0.0
  end

  test "related morphology is opt-in for electrical/electronic lexical evidence" do
    lexical_sense = %{definition: "To switch off something electrical."}

    base =
      ContextFit.score(
        "Power the computer down.",
        "power down",
        lexical_sense,
        extra_context: ["An electronic machine."]
      )

    related =
      ContextFit.score(
        "Power the computer down.",
        "power down",
        lexical_sense,
        extra_context: ["An electronic machine."],
        related_morphology?: true
      )

    assert base == 0.0
    assert related > 0.0
    assert "electrical" in
             ContextFit.explain(
               "Power the computer down.",
               "power down",
               lexical_sense,
               extra_context: ["An electronic machine."],
               related_morphology?: true
             ).matches.definition
  end

  test "possessives retain their base noun in contextual evidence" do
    # The previous plural-s rule turned Japan's into the invalid "japan'".
    for possessive <- ["Japan's", "Japan’s"] do
      sentence = "What do you know about #{possessive} railgun development?"

      report =
        ContextFit.explain(sentence, "what do you know", %{
          definition: "A question concerning Japan."
        })

      assert "japan" in report.context_terms
      refute "japan'" in report.context_terms
      assert "japan" in report.matches.definition
      assert report.score > 0.0
    end
  end

  test "one generic cue repeated in a definition and example counts only once" do
    sentence = "The city approved a new housing development."

    applied_ideas = %{
      definition: "The application of new ideas to practical problems.",
      example: "Our department has produced three new adhesives."
    }

    biological = %{
      definition: "The production of an organism by adding new cells.",
      example: "The organism reached a crucial stage."
    }

    ideas = ContextFit.explain(sentence, "development", applied_ideas)
    biology = ContextFit.explain(sentence, "development", biological)

    # Keep source provenance visible, but don't turn repetition of "new"
    # into twice the contextual evidence for an unrelated technical sense.
    assert ideas.matches.definition == ["new"]
    assert ideas.matches.example == ["new"]
    assert ideas.unique_matches.definition == ["new"]
    assert ideas.unique_matches.example == []
    assert_in_delta ideas.score, 0.29, 1.0e-6
    assert_in_delta biology.score, 0.29, 1.0e-6
    assert ideas.score == biology.score
  end

  test "independent cues in different dictionary fields still add evidence" do
    candidate = %{
      definition: "A financial institution dealing in money.",
      example: "A place to keep money and cash."
    }

    result = ContextFit.explain("The money bank accepted cash.", "bank", candidate)

    assert result.matches.definition == ["money"]
    assert result.matches.example == ["cash", "money"]
    assert result.unique_matches.definition == ["money"]
    assert result.unique_matches.example == ["cash"]
    assert_in_delta result.score, 0.38, 1.0e-6
  end

  test "custom evidence weights preserve the strongest source of a repeated cue" do
    candidate = %{
      definition: "One newly invented item.",
      example: "A new piece of equipment."
    }

    report =
      ContextFit.explain("The new development arrived.", "development", candidate,
        weights: %{definition: 0.0, example: 1.0, synonyms: 0.0,
                   semantic_atoms: 0.0, gram_function: 0.0})

    assert report.matches.definition == ["new"]
    assert report.matches.example == ["new"]
    assert report.unique_matches.definition == []
    assert report.unique_matches.example == ["new"]
    assert_in_delta report.score, 0.5, 1.0e-6
  end

  test "two independent technical clues retain railgun lexical evidence" do
    electromagnetic = %{
      definition: "An electromagnetic gun that uses current to propel a projectile.",
      example: nil
    }

    railway = %{definition: "A railway gun."}
    sentence = "Japan is developing an electromagnetic railgun to launch projectiles."

    strong = ContextFit.explain(sentence, "railgun", electromagnetic)
    weak = ContextFit.explain(sentence, "railgun", railway)

    assert strong.matches.definition == ["electromagnetic", "projectile"]
    assert strong.unique_matches.definition == strong.matches.definition
    assert_in_delta strong.score, 0.435, 1.0e-6
    assert weak.score == 0.0
  end

  test "nearby semantic evidence is preserved" do
    sentence = "I sat on the bank beside the river."

    river_sense = %{
      definition: "The sloping edge of a river.",
      example: "We walked along the river bank."
    }

    assert ContextFit.score(sentence, "bank", river_sense) > 0.0
  end
end
