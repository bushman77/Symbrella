defmodule Brain.PMTG.PlanningTest do
  use ExUnit.Case, async: true

  alias Brain.PMTG

  defp choice(index, lemma, id, opts \\ []) do
    %{
      token_index: index,
      lemma: lemma,
      chosen_id: id,
      alt_ids: [id <> "-alternative"],
      margin: 0.0,
      scores: %{id => 0.10},
      weak?: true,
      semantic_control: Keyword.get(opts, :semantic_control, %{reason: :insufficient_discriminating_evidence}),
      veto?: Keyword.get(opts, :veto?, false),
      margin_weak?: Keyword.get(opts, :margin_weak?, false)
    }
  end

  test "Kamloops population question only consults unresolved content words" do
    # Mirrors the seven LIFG tokens in the 2026-10-16 brain diagnostic.
    choices = [
      choice(0, "what", "what|pronoun|1", semantic_control: %{reason: :closed_class_winner}),
      choice(1, "is", "is|auxiliary|0", semantic_control: %{reason: :closed_class_winner}),
      choice(2, "the", "the|determiner|1", semantic_control: %{reason: :closed_class_winner}),
      choice(3, "population", "population|noun|1"),
      choice(4, "of", "of|preposition|12", semantic_control: %{reason: :closed_class_winner}),
      choice(5, "Kamloops", "kamloops|proper_noun|1"),
      choice(6, "BC", "bc|proper_noun|1")
    ]

    selected = PMTG.needy_choices(choices, margin_threshold: 0.15, p_min: 0.65)
    assert Enum.map(selected, & &1.token_index) == [3, 5, 6]

    tokens =
      Enum.map(choices, fn ch ->
        %{index: ch.token_index, token_index: ch.token_index, phrase: ch.lemma}
      end)

    assert {:ok, %{queries: queries, si: si}} =
             PMTG.resolve(%{tokens: tokens, trace: []}, selected, limit: 5)

    assert Enum.map(queries, & &1.lemma) == ["population", "Kamloops", "BC"]
    assert [%{meta: %{needy_count: 3, planned_queries: 3}}] = si.trace
    assert hd(si.trace).scores.needy_ratio < 1.0
  end

  test "do not trust closed-class labels when final winner is a verb or vetoed" do
    provisional_particle = choice(0, "know", "know|verb|1",
      semantic_control: %{reason: :closed_class_winner}
    )

    vetoed = choice(1, "the", "the|determiner|1",
      semantic_control: %{reason: :closed_class_winner}, veto?: true
    )

    weak_grammar = choice(2, "what", "what|pronoun|1",
      semantic_control: %{reason: :closed_class_winner}, margin_weak?: true
    )

    assert PMTG.needy_choices([provisional_particle, vetoed, weak_grammar]) ==
             [provisional_particle, vetoed, weak_grammar]
  end

  test "semantic control must explicitly verify the grammatical selection" do
    uncertain = choice(0, "what", "what|pronoun|1")
    fallback = choice(1, "the", "the|determiner|fallback",
      semantic_control: %{reason: :closed_class_winner}
    )
    verified = choice(2, "is", "is|auxiliary|0",
      semantic_control: %{reason: :closed_class_winner}
    )

    assert PMTG.needy_choices([uncertain, fallback, verified]) == [uncertain, fallback]
  end
end
