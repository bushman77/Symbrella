defmodule Brain.PMTG.SenseCompatTest do
  use ExUnit.Case, async: true
  alias Brain.PMTG

  setup do
    :telemetry.attach(
      "pmtg-no-mwe-senses-test",
      [:brain, :pmtg, :no_mwe_senses],
      fn event, meas, meta, _ ->
        send(self(), {:telemetry, event, meas, meta})
      end,
      nil
    )

    on_exit(fn ->
      :telemetry.detach("pmtg-no-mwe-senses-test")
    end)

    :ok
  end

  test "keeps only MWE senses for MWE tokens; keeps single-word senses for 1-word tokens" do
    tokens = [
      %{index: 0, phrase: "new york", n: 2},  # MWE
      %{index: 1, phrase: "bank", n: 1}       # single
    ]

    evidence = [
      %{
        token_index: 0,
        lemma: "new york",
        lexicon: [
          %{id: "ny|noun|0", lemma: "new york"},
          %{id: "york|noun|0", lemma: "york"} # should be dropped for MWE
        ]
      },
      %{
        token_index: 1,
        lemma: "bank",
        lexicon: [
          %{id: "bank|noun|0", lemma: "bank"},
          %{id: "river bank|noun|0", lemma: "river bank"} # should be dropped for single
        ]
      }
    ]

    out = PMTG.enforce_sense_compatibility(evidence, tokens)

    [ev0, ev1] = out

    assert Enum.map(ev0.lexicon, & &1.id) == ["ny|noun|0"]
    assert Enum.map(ev1.lexicon, & &1.id) == ["bank|noun|0"]

    refute_receive {:telemetry, [:brain, :pmtg, :no_mwe_senses], _, _}, 50
  end

  test "emits telemetry and falls back when MWE has no MWE senses" do
    tokens = [
      %{index: 0, phrase: "hot dog", n: 2}
    ]

    evidence = [
      %{
        token_index: 0,
        lemma: "hot dog",
        # No space-separated lemmas -> will fallback and emit telemetry
        lexicon: [
          %{id: "hotdog|noun|0", lemma: "hotdog"},
          %{id: "dog|noun|0", lemma: "dog"}
        ]
      }
    ]

    out = PMTG.enforce_sense_compatibility(evidence, tokens)
    [ev] = out

    # Fallback: original lexicon preserved
    assert Enum.map(ev.lexicon, & &1.id) == ["hotdog|noun|0", "dog|noun|0"]

    # Telemetry proves we had to fallback
    assert_receive {:telemetry, [:brain, :pmtg, :no_mwe_senses],
                    %{orig: 2, kept: 0, token_index: 0, phrase: "hot dog"}, _}, 100
  end
end

