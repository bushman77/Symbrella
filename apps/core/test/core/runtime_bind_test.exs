defmodule Core.RuntimeBindTest do
  use ExUnit.Case, async: true
  alias Core.RuntimeBind
  alias Core.SemanticInput, as: SI

  defp tok(id, norm, opts \\ []) do
    base = %{
      id: id,
      text: norm,
      norm: norm,
      lemma: Keyword.get(opts, :lemma, norm),
      span: %{char_start: id * 2, char_end: id * 2 + String.length(norm)},
      is_mwe_head: Keyword.get(opts, :is_mwe_head, false),
      mwe_id: Keyword.get(opts, :mwe_id, nil),
      mwe_span: Keyword.get(opts, :mwe_span, nil),
      is_stop: false,
      is_oov: false,
      kind: :word,
      hash: Integer.to_string(:erlang.phash2(norm), 16)
    }

    base
  end

  test "binds runtime active cells by token and MWE phrase" do
    tokens = [
      tok(0, "open", lemma: "open"),
      tok(1, "a", lemma: "a"),
      tok(2, "bank", is_mwe_head: true, mwe_id: "mwe:ba:0", mwe_span: %{first_id: 2, last_id: 3}),
      tok(3, "account", mwe_id: "mwe:ba:0", mwe_span: %{first_id: 2, last_id: 3})
    ]

    si = %SI{sentence: "Open a bank account", tokens: tokens}

    snapshot =
      {123,
       [
         %{id: "cell:bank_account", keys: ["bank account", "bank", "account"], activation: 0.82},
         %{id: "cell:river_bank", keys: ["river bank", "bank"], activation: 0.41},
         %{id: "cell:unrelated", keys: ["cactus"], activation: 0.90}
       ]}

    out = RuntimeBind.bind(si, snapshot: snapshot, topk: 16)

    assert out.brain_state_ref == 123
    assert length(out.active_cells) == 2

    # First should be the higher activation one (bank_account)
    [first, second] = out.active_cells
    assert first.id == "cell:bank_account"
    assert Enum.map(first.matched_tokens, & &1.tok_id) == [2, 3]
    assert first.source == :runtime
    assert first.reason == :matched_active_cell
    assert first.activation_snapshot == 0.82

    assert second.id == "cell:river_bank"
    assert Enum.map(second.matched_tokens, & &1.tok_id) == [2]

    # Trace event present
    assert List.last(out.trace).stage == :runtime_bind
    refute List.last(out.trace).meta.timeout?
  end

  test "dedups matched tokens and merges by cell id" do
    tokens = [tok(0, "bank"), tok(1, "bank")]
    si = %SI{sentence: "bank bank", tokens: tokens}

    snap = {1, [%{id: "cell:bank", keys: ["bank"], activation: 0.2}]}

    out = RuntimeBind.bind(si, snapshot: snap)
    [only] = out.active_cells
    assert only.id == "cell:bank"
    assert Enum.map(only.matched_tokens, & &1.tok_id) == [0, 1]
  end

  test "respects topk cap" do
    tokens = [tok(0, "alpha")]
    si = %SI{sentence: "alpha", tokens: tokens}

    # 5 matching cells; cap to 3
    active =
      Enum.map(1..5, fn i ->
        %{id: "cell:#{i}", keys: ["alpha"], activation: 0.1 * i}
      end)

    out = RuntimeBind.bind(si, snapshot: {9, active}, topk: 3)
    assert length(out.active_cells) == 3

    # Sorted by activation desc
    assert Enum.map(out.active_cells, & &1.id) == ["cell:5", "cell:4", "cell:3"]
  end

  test "soft-fails on missing snapshot" do
    tokens = [tok(0, "nothing")]
    si = %SI{sentence: "nothing", tokens: tokens}

    # no snapshot provided
    out = RuntimeBind.bind(si)
    assert out.active_cells == []
    ev = List.last(out.trace)
    assert ev.stage == :runtime_bind
    assert ev.meta.timeout?
  end

  test "merge preserves stronger source precedence" do
    tokens = [tok(0, "alpha")]

    si = %SI{
      sentence: "alpha",
      tokens: tokens,
      active_cells: [
        %{
          id: "cell:1",
          matched_tokens: [%{tok_id: 0, conf: nil}],
          activation_snapshot: 0.3,
          source: :runtime,
          reason: :matched_active_cell,
          score: nil,
          ts_ms: 42
        }
      ]
    }

    # incoming from hypothetical recency bind
    incoming = {9, [%{id: "cell:1", keys: ["alpha"], activation: 0.2}]}

    out = RuntimeBind.bind(si, snapshot: incoming)
    [only] = out.active_cells

    assert only.source == :runtime
    assert only.activation_snapshot == 0.3
  end
end
