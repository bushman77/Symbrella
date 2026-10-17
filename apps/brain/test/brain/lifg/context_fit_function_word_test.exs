defmodule Brain.LIFG.ContextFitFunctionWordTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.ContextFit
  alias Brain.LIFG.SemanticControl

  test "about by itself is not evidence for a particular verb sense" do
    sentence = "What do you know about Japan's railgun development?"
    candidate = %{definition: "To know about a subject.", pos: "verb"}

    explanation = ContextFit.explain(sentence, "know", candidate)

    assert explanation.score == 0.0
    assert explanation.matches.definition == []
    refute "about" in explanation.context_terms
    assert "railgun" in explanation.context_terms

    scores = %{"know|verb|1" => 0.4, "know|verb|2" => 0.4}
    rows = [
      %{id: "know|verb|1", context_evidence: explanation.score},
      %{id: "know|verb|2", context_evidence: 0.0}
    ]

    assert {^scores, %{applied?: false,
                       reason: :insufficient_discriminating_evidence}} =
             SemanticControl.apply(scores, rows)
  end

  test "a content word such as river still supplies dictionary discrimination" do
    sentence = "I stood on the bank beside the river."
    supported = %{definition: "The edge of a river."}
    unsupported = %{definition: "A financial institution."}

    river = ContextFit.explain(sentence, "bank", supported)
    finance = ContextFit.explain(sentence, "bank", unsupported)

    assert "river" in river.matches.definition
    assert river.score > finance.score
    assert finance.score == 0.0

    {scores, report} =
      SemanticControl.apply(
        %{"bank|noun|river" => 0.4, "bank|noun|finance" => 0.4},
        [
          %{id: "bank|noun|river", context_evidence: river.score},
          %{id: "bank|noun|finance", context_evidence: finance.score}
        ]
      )

    assert report.applied?
    assert scores["bank|noun|river"] > scores["bank|noun|finance"]
  end
end
