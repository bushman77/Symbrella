defmodule Core.LtmRetrievalAuditTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.LTM

  @rows [
    %{id: "bank|noun|1", token_index: 0, pos: "noun", norm: "bank"},
    %{id: "bank|verb|1", token_index: 0, pos: "verb", norm: "bank"},
    %{id: "bank|noun|2", token_index: 0, pos: "noun", norm: "bank"},
    %{id: "bank|noun|1", token_index: 2, pos: "noun", norm: "bank"}
  ]

  test "default unrestricted retrieval reports candidate load without altering sense rows" do
    audit = LTM.retrieval_audit(@rows)

    assert audit.budget == :all
    assert audit.candidate_rows == 4
    assert audit.token_positions == 2
    assert audit.max_senses_per_token == 3
    assert audit.ambiguous_positions == 1
    assert audit.multi_pos_positions == 1
    assert audit.sense_recall == :not_measured
    assert audit.pos_diverse? == false

    si = %{active_cells: [], activation_summary: %{db_hits: MapSet.new()}}
    out = LTM.enrich(si, @rows, MapSet.new(["bank"]))
    assert out.active_cells == @rows
    assert out.activation_summary.db_hits == MapSet.new(["bank"])
  end

  test "bounded POS-diverse retrieval is explicitly opt-in and does not claim full recall" do
    audit = LTM.retrieval_audit(@rows, limit_per_norm: 8, pos_diverse?: true)
    assert audit.budget == {:limited, 8}
    assert audit.pos_diverse_requested?
    assert audit.pos_diverse?
    assert audit.sense_recall == :not_measured

    unrestricted = LTM.retrieval_audit(@rows, pos_diverse?: true)
    assert unrestricted.budget == :all
    assert unrestricted.pos_diverse_requested?
    refute unrestricted.pos_diverse?
  end

  test "empty candidate slate reports zero without inventing a dictionary hit" do
    audit = LTM.retrieval_audit([], limit_per_norm: 4)
    assert audit.candidate_rows == 0
    assert audit.token_positions == 0
    assert audit.max_senses_per_token == 0
    assert audit.multi_pos_positions == 0
    assert audit.ambiguous_positions == 0
    assert audit.sense_recall == :not_measured
  end
end
