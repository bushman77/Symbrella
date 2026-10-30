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
  test "comparison reports excluded senses without claiming a semantic winner" do
    full = @rows
    bounded = [Enum.at(@rows, 0), Enum.at(@rows, 1), Enum.at(@rows, 3)]
    result = LTM.compare_slates(full, bounded)

    assert result.full_senses == 4
    assert result.bounded_senses == 3
    assert result.retained_senses == 3
    assert result.lost_senses == 1
    assert result.unexpected_senses == 0
    assert result.sense_id_coverage == 0.75
    assert result.lost_pos_positions == 0
    assert Enum.find(result.positions, &(&1.token_index == 0)).lost_senses == 1

    only_nouns = [Enum.at(@rows, 0), Enum.at(@rows, 2), Enum.at(@rows, 3)]
    lost_pos = LTM.compare_slates(full, only_nouns)
    assert lost_pos.lost_pos_positions == 1
    assert Enum.find(lost_pos.positions, &(&1.token_index == 0)).lost_pos == ["verb"]
  end

  test "checked comparison uses independent full and POS-balanced reads without mutating input" do
    si = Core.Pipeline.Input.prepare("bank")
    original_tokens = si.tokens
    parent = self()

    fetch = fn received, opts ->
      send(parent, {:retrieval_opts, opts, received.tokens})

      rows =
        case Keyword.fetch!(opts, :limit_per_norm) do
          :all -> Enum.take(@rows, 3)
          2 -> Enum.take(@rows, 2)
        end

      {:ok, %{rows: rows, db_hits: MapSet.new(["bank"])}}
    end

    assert {:ok, report} =
             LTM.compare_retrieval(si,
               limit_per_norm: 2, pos_diverse?: true, lookup: fetch
             )

    assert_receive {:retrieval_opts, first_opts, ^original_tokens}
    assert_receive {:retrieval_opts, second_opts, ^original_tokens}
    assert first_opts[:limit_per_norm] == :all
    assert first_opts[:pos_diverse?] == false
    assert second_opts[:limit_per_norm] == 2
    assert second_opts[:pos_diverse?] == true
    assert report.full_senses == 3
    assert report.bounded_senses == 2
    assert report.sense_id_coverage == 2 / 3
    assert report.semantic_accuracy == :not_measured
    assert report.interpretation == :lexical_candidate_overlap_only
    assert is_integer(report.full_lookup_us)
    assert is_integer(report.bounded_lookup_us)
    assert si.tokens == original_tokens
    assert si.active_cells == []
  end

  test "comparison fails explicitly on lookup errors and invalid requests" do
    si = Core.Pipeline.Input.prepare("bank")
    failing = fn _, opts ->
      if opts[:limit_per_norm] == :all,
        do: {:error, :ltm_query_failed},
        else: {:ok, %{rows: []}}
    end

    assert {:error, :ltm_query_failed} =
             LTM.compare_retrieval(si, lookup: failing)

    assert {:error, :invalid_budget} =
             LTM.compare_retrieval(si, limit_per_norm: :all)

    assert {:error, :too_many_tokens} =
             LTM.compare_retrieval(Core.Pipeline.Input.prepare("one two three"),
               max_tokens: 2, lookup: fn _, _ -> flunk("should not query") end)

    assert {:error, :no_tokens} =
             LTM.compare_retrieval(Core.Pipeline.Input.prepare(""))
  end

  test "sense comparison is position-specific and handles no full candidates" do
    full = [%{token_index: 0, id: "bank|noun|1", pos: "noun"}]
    bounded = [%{token_index: 1, id: "bank|noun|1", pos: "noun"}]
    mismatch = LTM.compare_slates(full, bounded)

    assert mismatch.retained_senses == 0
    assert mismatch.unexpected_senses == 1
    assert mismatch.sense_id_coverage == 0.0

    empty = LTM.compare_slates([], [])
    assert empty.full_senses == 0
    assert empty.sense_id_coverage == nil
    assert empty.positions == []
  end

end
