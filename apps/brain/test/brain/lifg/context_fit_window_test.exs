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

  test "nearby semantic evidence is preserved" do
    sentence = "I sat on the bank beside the river."

    river_sense = %{
      definition: "The sloping edge of a river.",
      example: "We walked along the river bank."
    }

    assert ContextFit.score(sentence, "bank", river_sense) > 0.0
  end
end
