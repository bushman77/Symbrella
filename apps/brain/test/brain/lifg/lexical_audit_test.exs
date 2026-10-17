defmodule Brain.LIFG.LexicalAuditTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.LexicalAudit

  test "distinguishes river-bank meaning using independent local dictionary gloss evidence" do
    senses = [
      %{id: "bank|noun|river", pos: "noun",
        definition: "The sloping edge of a river.", example: "A river bank."},
      %{id: "bank|noun|finance", pos: "noun",
        definition: "A financial institution.", example: "A bank lends money."}
    ]

    report = LexicalAudit.analyze("I stood on the bank beside the river.", "bank", senses)

    assert report.candidate_count == 2
    assert [%{pos: "noun", sense_count: 2,
              top_id: "bank|noun|river",
              eligible_for_control?: true,
              reason: :contextual_sense_discrimination}] = report.groups
    assert hd(report.senses).context_fit > 0.0
    assert "river" in hd(report.senses).matches.definition
    assert Enum.map(report.senses, & &1.id) ==
             ["bank|noun|river", "bank|noun|finance"]
  end

  test "preserves ambiguity when sentence topic supplies no discriminating lexical evidence" do
    senses = [
      %{id: "development|noun|1", pos: "noun",
        definition: "Gradual growth of an organism."},
      %{id: "development|noun|2", pos: "noun",
        definition: "A new event in an evolving situation."},
      %{id: "development|noun|3", pos: "noun",
        definition: "A group of newly constructed buildings."}
    ]

    report = LexicalAudit.analyze(
      "What do you know about Japan's railgun development?",
      "development",
      senses
    )

    assert [%{eligible_for_control?: false,
              reason: :insufficient_discriminating_evidence,
              evidence_gap: gap}] = report.groups
    assert gap == 0.0
    assert Enum.all?(report.senses, &(&1.context_fit == 0.0))
    assert Enum.all?(report.senses, fn row ->
             Enum.all?(Map.values(row.matches), fn matches -> matches == [] end)
           end)
  end

  test "repeated new in gloss and example cannot falsely select technical development sense" do
    senses = [
      %{id: "development|noun|6", pos: "noun",
        definition: "The application of new ideas to practical problems.",
        example: "Our development department produced three new adhesives."},
      %{id: "development|noun|2", pos: "noun",
        definition: "The process of producing an organism by adding new cells."},
      %{id: "development|noun|4", pos: "noun",
        definition: "A project of residential buildings."}
    ]

    result =
      LexicalAudit.analyze(
        "The city approved a new housing development.",
        "development",
        senses
      )

    assert [%{pos: "noun", eligible_for_control?: false,
              reason: :insufficient_discriminating_evidence,
              evidence_gap: gap}] = result.groups
    assert gap == 0.0

    technical = Enum.find(result.senses, &(&1.id == "development|noun|6"))
    assert technical.matches.example == ["new"]
    assert technical.unique_matches.example == []
  end

  test "grammatical class is not misrepresented as word-sense discrimination" do
    senses = [
      %{id: "record|noun|1", pos: "noun", definition: "Written information."},
      %{id: "record|verb|1", pos: "verb", definition: "To document information."}
    ]

    report = LexicalAudit.analyze("Record this information.", "record", senses)
    assert length(report.groups) == 2
    assert Enum.all?(report.groups, &(&1.sense_count == 1))
    assert Enum.all?(report.groups, &(not &1.eligible_for_control?))
  end

  test "reports only real dictionary IDs, deduplicates and rejects malformed candidates" do
    senses = [
      %{id: "bank|noun|river", definition: "The edge of a river."},
      %{id: "bank|noun|river", definition: "A duplicate row."},
      %{id: "malformed", definition: "Not a sense."},
      %{}
    ]

    report = LexicalAudit.analyze("I saw a bank by the river.", "bank", senses)
    assert report.candidate_count == 1
    assert [%{top_id: "bank|noun|river", eligible_for_control?: false}] = report.groups
    assert LexicalAudit.analyze(nil, "bank", []).candidate_count == 0
  end
end
