defmodule Brain.LIFG.GlossBridgesTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.GlossBridges

  test "connects cash gloss to a financial bank definition with exact sense provenance" do
    targets = [
      %{id: "bank|noun|finance",
        definition: "An institution that accepts deposits of money."},
      %{id: "bank|noun|river", definition: "The sloping edge of a river."}
    ]

    sources = %{
      "cash" => [
        %{id: "cash|noun|money", norm: "cash", status: "inactive",
          definition: "Money in the form of coins and notes."},
        %{id: "cash|verb|win", norm: "cash", status: "inactive",
          definition: "To exchange a coupon for payment."}
      ]
    }

    report = GlossBridges.analyze(
      "I deposited cash at the bank.", "bank", targets, sources
    )

    assert report.verified? == false
    assert report.source_sense_count == 2

    assert [%{
      context_word: "cash",
      context_sense_id: "cash|noun|money",
      context_status: "inactive",
      context_sense_count: 2,
      target_sense_id: "bank|noun|finance",
      lexical_fit: fit,
      evidence: [%{term: "money", field: :definition}],
      verified?: false
    }] = report.edges

    assert fit > 0.0
  end

  test "connects a housing gloss to residential development without hardcoded word pairs" do
    targets = [
      %{id: "development|noun|residential",
        definition: "A project of residential buildings."},
      %{id: "development|noun|technical",
        definition: "New ideas applied to practical problems."}
    ]

    sources = %{
      "housing" => [
        %{id: "housing|noun|1", norm: "housing",
          definition: "Residential accommodation and buildings."}
      ],
      "new" => []
    }

    report = GlossBridges.analyze(
      "The city approved a new housing development.",
      "development", targets, sources
    )

    assert Enum.any?(report.edges, fn edge ->
      edge.context_sense_id == "housing|noun|1" and
        edge.target_sense_id == "development|noun|residential" and
        Enum.any?(edge.evidence, &(&1.term == "residential"))
    end)

    assert report.verified? == false
  end

  test "unrelated glosses and ungrounded records do not create bridges" do
    targets = [%{id: "bank|noun|finance", definition: "An institution for money."}]

    sources = %{
      "cash" => [
        %{id: "cash|noun|1", norm: "cash", definition: "A musical keyboard."},
        %{id: "other|noun|1", norm: "unrelated", definition: "Money."},
        %{id: "cash|noun|2", norm: "cash", definition: nil}
      ]
    }

    result = GlossBridges.analyze("I deposited cash at the bank.", "bank", targets, sources)
    assert result.source_sense_count == 1
    assert result.edges == []
    assert result.edge_count == 0
  end

  test "polysemous evidence stays explicitly unverified even with several gloss matches" do
    targets = [
      %{id: "bank|noun|finance", definition: "A place to store money and make deposits."}
    ]

    sources = %{
      "cash" => [
        %{id: "cash|noun|1", norm: "cash", definition: "Money and deposits."},
        %{id: "cash|noun|2", norm: "cash", definition: "Money and a payment."}
      ]
    }

    report = GlossBridges.analyze("I deposited cash at the bank.", "bank", targets, sources)

    assert length(report.edges) == 2
    assert Enum.all?(report.edges, &(not &1.verified?))
    assert Enum.all?(report.edges, &(&1.context_sense_count == 2))
    assert report.verified? == false
  end
end
