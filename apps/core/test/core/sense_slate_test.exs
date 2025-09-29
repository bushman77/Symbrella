defmodule Core.SenseSlateTest do
  use ExUnit.Case, async: true
  alias Core.SenseSlate

  defp fetch_senses_mock do
    fn
      "bank" ->
        [
          %{id: "bank|noun|money", prior: 0.7, features: %{domain: :finance}},
          %{id: "bank|noun|river", prior: 0.5, features: %{domain: :geography}}
        ]

      "river bank" ->
        [%{id: "river bank|noun|0", prior: 0.6, features: %{multiword: true}}]

      _ ->
        []
    end
  end

  test "builds slate keyed by token start index; orders by prior, deduped by id" do
    tokens = [
      %{phrase: "bank", span: {0, 1}},
      %{phrase: "river", span: {1, 2}},
      %{phrase: "bank", span: {2, 3}},
      %{phrase: "river bank", mw: true, span: {1, 3}, n: 2}
    ]

    slate = SenseSlate.build(tokens, fetch_senses: fetch_senses_mock())

    # keys are token start indices: 0 (bank), 1 (river), 2 (bank), 1 again for the MWE start
    assert Map.has_key?(slate, 0)
    assert Map.has_key?(slate, 2)
    assert Map.has_key?(slate, 1)

    # index 0: "bank" senses sorted by prior
    ids0 = slate[0] |> Enum.map(& &1.id)
    assert ids0 == ["bank|noun|money", "bank|noun|river"]

    # index 1: should include the MWE sense we exposed for "river bank"
    ids1 = slate[1] |> Enum.map(& &1.id)
    assert "river bank|noun|0" in ids1

    # index 2: another "bank" occurrence
    ids2 = slate[2] |> Enum.map(& &1.id)
    assert ids2 == ["bank|noun|money", "bank|noun|river"]
  end

  test "attach/2 stores slate under :sense_candidates" do
    tokens = [%{phrase: "bank", span: {0, 1}}]
    slate = SenseSlate.build(tokens, fetch_senses: fetch_senses_mock())
    si = %{tokens: tokens}
    si2 = SenseSlate.attach(si, slate)
    assert Map.has_key?(si2, :sense_candidates)
    assert is_map(si2.sense_candidates)
  end
end
