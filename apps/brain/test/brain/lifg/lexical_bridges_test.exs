defmodule Brain.LIFG.LexicalBridgesTest do
  use ExUnit.Case, async: true

  alias Brain.LIFG.LexicalBridges

  test "shows a sense-provenanced possible cash to money bridge without promoting it" do
    targets = [
      %{id: "bank|noun|finance", definition: "An institution for depositing money."},
      %{id: "bank|noun|river", definition: "The edge of a river."}
    ]

    context = %{
      "cash" => [
        %{id: "cash|noun|1", norm: "cash", pos: "noun",
          status: "inactive", synonyms: ["money", "currency"]},
        %{id: "cash|noun|2", norm: "cash", pos: "noun",
          status: "inactive", synonyms: ["register"]}
      ]
    }

    report = LexicalBridges.analyze(
      "I deposited cash at the bank.", "bank", targets, context
    )

    assert report.verified? == false
    assert report.bridge_count == 1
    assert [%{
      context_word: "cash",
      context_sense_id: "cash|noun|1",
      context_sense_count: 2,
      context_status: "inactive",
      synonym: "money",
      target_sense_id: "bank|noun|finance",
      verified?: false
    }] = report.bridges
  end

  test "housing may bridge to residential but remains only hypothetical" do
    targets = [
      %{id: "development|noun|residential",
        definition: "A group of residential buildings."},
      %{id: "development|noun|biological",
        definition: "The growth of an organism."}
    ]

    context = %{
      "housing" => [
        %{id: "housing|noun|1", norm: "housing", pos: "noun",
          status: "inactive", synonyms: ["residential", "accommodation"]}
      ]
    }

    result = LexicalBridges.analyze(
      "The city approved a new housing development.",
      "development", targets, context
    )

    assert [%{synonym: "residential",
              target_sense_id: "development|noun|residential",
              verified?: false}] = result.bridges
    assert result.verified? == false
  end

  test "rejects unrelated synonyms, ungrounded source records and substring matches" do
    targets = [%{id: "bank|noun|finance", definition: "An institution for money."}]

    context = %{
      "cash" => [
        %{id: "cash|noun|1", norm: "cash", synonyms: ["mon", "river"]},
        %{id: "other|noun|1", norm: "different", synonyms: ["money"]}
      ]
    }

    report = LexicalBridges.analyze(
      "I deposited cash at the bank.", "bank", targets, context
    )

    assert report.bridge_count == 0
    assert report.verified? == false
  end

  test "an empty dictionary produces no invented synonym evidence" do
    report = LexicalBridges.analyze(
      "What do you know about Japan's railgun development?",
      "railgun",
      [%{id: "railgun|noun|1", definition: "An electromagnetic gun."}],
      %{}
    )

    assert report.bridge_count == 0
    assert report.verified? == false
    assert Enum.all?(report.context_inventory, &(&1.available_senses == 0))
  end
end
