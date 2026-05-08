defmodule Brain.ATL.StatusTest do
  use ExUnit.Case, async: false

  alias Brain.ATL

  setup do
    case Process.whereis(ATL) do
      nil -> start_supervised!({ATL, keep: 10})
      _pid -> :ok
    end

    ATL.reset()
    :ok
  end

  test "status/0 returns compact dashboard state before and after ingest" do
    assert %{
             region: :atl,
             status: :up,
             window_count: 0,
             concept_count: 0,
             sense_count: 0,
             last_winner_count: 0
           } = ATL.status()

    ATL.ingest(
      [
        %{id: "alpha|noun|0", lemma: "alpha", token_index: 0, score: 0.9},
        %{id: "beta|noun|0", lemma: "beta", token_index: 1, score: 0.8}
      ],
      [%{phrase: "alpha"}, %{phrase: "beta"}]
    )

    assert %{
             region: :atl,
             status: :up,
             window_count: 1,
             concept_count: 2,
             sense_count: 2,
             last_winner_count: 2,
             last_concept_count: 2,
             last_sense_count: 2
           } = ATL.status()
  end
end
