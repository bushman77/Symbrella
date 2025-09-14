defmodule Core.RecallExecuteTest do
  use ExUnit.Case, async: true
  alias Core.SemanticInput, as: SI
  alias Core.Recall.{Plan, Execute}

  defp token(phrase, opts \\ []) do
    %Core.Token{
      phrase: phrase,
      span: Keyword.get(opts, :span, {0, String.length(phrase)}),
      mw: Keyword.get(opts, :mw, false),
      source: :assumed,
      instances: [],
      n: Keyword.get(opts, :n, 1)
    }
  end

  defp si_stub() do
    %SI{
      original_sentence: "open a bank account",
      sentence: "open a bank account",
      source: :test,
      tokens: [
        token("open"),
        token("a"),
        token("bank", mw: true),
        token("account")
      ],
      # Assume runtime already covered token idx 2 (bank)
      active_cells: [
        %{
          id: "cell:bank_account_runtime",
          matched_tokens: [%{tok_id: 2, conf: nil}],
          activation_snapshot: 0.8,
          source: :runtime,
          reason: :matched_active_cell,
          score: nil,
          ts_ms: 1
        }
      ],
      brain_state_ref: 123,
      trace: []
    }
  end

  test "exact recall adds refs for uncovered keys and respects max_items" do
    plan = %Plan{triggers: [:low_conf], budget_ms: 100, max_items: 1, strategies: [:exact]}
    si = si_stub()

    exists_fun = fn
      "open" -> true
      "a" -> false
      "account" -> true
      _ -> false
    end

    si2 = Execute.execute(si, plan, exists_fun: exists_fun, neg_exists_fun: fn _ -> false end, neg_put_fun: fn _ -> :ok end)

    # Only 1 item allowed by max_items; should not duplicate the runtime one
    assert length(si2.active_cells) == 2
    assert Enum.any?(si2.active_cells, &match?({:db_word, "open"}, &1.id)) or Enum.any?(si2.active_cells, &match?({:db_word, "account"}, &1.id))

    ev = List.last(si2.trace)
    assert ev.stage == :recall_exec
    assert ev.meta.counts.exact in 0..1
  end

  test "uses negcache to skip misses and records new misses" do
    plan = %Plan{triggers: [:oov_terms_present], budget_ms: 100, max_items: 10, strategies: [:exact]}

    seen = :ets.new(:neg, [:set, :public])

    neg_exists = fn k -> :ets.lookup(seen, k) != [] end
    neg_put = fn k -> :ets.insert(seen, {k, true}); :ok end

    si = %SI{
      original_sentence: "foo bar",
      sentence: "foo bar",
      source: :test,
      tokens: [token("foo"), token("bar")],
      active_cells: [],
      brain_state_ref: nil,
      trace: []
    }

    # All miss
    exists_fun = fn _ -> false end
    si2 = Execute.execute(si, plan, exists_fun: exists_fun, neg_exists_fun: neg_exists, neg_put_fun: neg_put)

    # Both should be recorded as misses in ETS
    assert :ets.lookup(seen, "foo") != []
    assert :ets.lookup(seen, "bar") != []

    ev = List.last(si2.trace)
    assert ev.meta.counts.exact == 0
  end
end

