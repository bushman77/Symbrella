# Proposed regression tests: T-002, T-003, T-005

Status: **UNTESTED proposals.** Claude cannot run Elixir; none of this has been compiled. They are not applied to the codebase and need `Approve: P-###` from Brad before they become test files. Fixtures follow the existing `diagnosis_test.exs`, `comprehension_gaps_test.exs` and `jsonl_import_batch_test.exs`.

Expected result today: the T-002 and T-003 "bad input" cases should be **red** (they demonstrate the bugs). The T-005 characterization test should be **green** (it documents current behavior); the contract test stays skipped until the contract is chosen.

## T-002: gap gate parity (add to `apps/core/test/core/comprehension/diagnosis_test.exs`)

Idea: do not hand-assert each outcome. Assert that the diagnosis gate agrees with the real gate it claims to mirror, across a table of inputs.

```elixir
  alias Core.Curiosity.ComprehensionGaps

  defp gate_si(overrides) do
    base = %{
      source: :user,
      session_id: "gate-parity",
      sentence: "I have a glimmerflux.",
      comprehension: %{
        degraded?: true,
        reasons: [:unresolved_content_tokens],
        uncertain: ["glimmerflux"],
        unresolved: ["glimmerflux"],
        understood: [],
        lexical_meanings: [],
        stats: %{total: 1, weak_rate: 1.0, fallback_rate: 0.0, unresolved_content: 1}
      }
    }

    Enum.reduce(overrides, base, fn
      {:comprehension, c}, acc -> %{acc | comprehension: Map.merge(acc.comprehension, c)}
      {k, v}, acc -> Map.put(acc, k, v)
    end)
  end

  @gate_cases [
    {"valid", %{}, true},
    {"one nonblank among junk", %{comprehension: %{unresolved: [nil, "x"]}}, true},
    {"empty-string unresolved", %{comprehension: %{unresolved: [""]}}, false},
    {"nil unresolved item", %{comprehension: %{unresolved: [nil]}}, false},
    {"whitespace unresolved", %{comprehension: %{unresolved: ["  "]}}, false},
    {"mixed blank items", %{comprehension: %{unresolved: [nil, ""]}}, false},
    {"no unresolved", %{comprehension: %{unresolved: []}}, false},
    {"no uncertain", %{comprehension: %{uncertain: []}}, false},
    {"not degraded", %{comprehension: %{degraded?: false}}, false},
    {"no session", %{session_id: nil}, false},
    {"assistant source", %{source: :assistant}, false},
    {"empty sentence", %{sentence: ""}, false}
  ]

  for {name, overrides, eligible?} <- @gate_cases do
    test "gap gate agrees with ComprehensionGaps: #{name}" do
      si = gate_si(unquote(Macro.escape(overrides)))
      real = match?({:ok, _}, ComprehensionGaps.from_completed_turn(si, si.session_id))

      # Anchor the table itself so a wrong expectation cannot hide behind parity.
      assert real == unquote(eligible?)
      assert (Diagnosis.assess(si).curiosity_gap_gate == :eligible_for_review) == real
    end
  end
```

## T-003: `feedback_applied` distinguishes unobserved from false (same file)

```elixir
  for {name, feedback, expected} <- [
        {"true", %{mode: :observe_only, applied?: true}, true},
        {"false", %{mode: :observe_only, applied?: false}, false},
        {"key absent", %{mode: :observe_only}, :not_observed},
        {"nil", %{mode: :observe_only, applied?: nil}, :not_observed},
        {"string", %{mode: :observe_only, applied?: "yes"}, :not_observed},
        {"integer", %{mode: :observe_only, applied?: 1}, :not_observed}
      ] do
    test "feedback_applied for applied?: #{name}" do
      si = gate_si(%{}) |> Map.put(:lifg_feedback, unquote(Macro.escape(feedback)))
      assert Diagnosis.assess(si).feedback_applied == unquote(Macro.escape(expected))
    end
  end

  test "feedback_applied is :not_observed when lifg_feedback is not a map" do
    si = gate_si(%{}) |> Map.put(:lifg_feedback, nil)
    assert Diagnosis.assess(si).feedback_applied == :not_observed
  end
```

(`gate_si/1` is reused from the T-002 block; keep both in the same module or move it to a shared helper.)

## T-005: importer failure propagation (new file `apps/db/test/db/jsonl_import_failure_test.exs`)

Contract question for Brad and GPT before patching: what should `import_all/1` return for `on_db_error: :halt`? Proposal: `{:error, {:db_error, exception, %{upserted_so_far: n, batches: b}}}`, the same shape `Db.JSONL.Upsert`'s bulk function already returns.

```elixir
defmodule Db.JSONL.ImportFailureTest do
  use ExUnit.Case, async: true
  @moduletag :tmp_dir

  alias Db.JSONL.Import

  defmodule FailingRepo do
    def insert_all(Db.BrainCell, _rows, _opts), do: raise("boom")
  end

  defmodule CollectingRepo do
    def insert_all(Db.BrainCell, rows, opts) do
      send(self(), {:inserted, rows, opts})
      {length(rows), nil}
    end
  end

  defp write_dump(dir, words) do
    path = Path.join(dir, "english.jsonl")

    lines =
      for w <- words do
        Jason.encode!(%{"word" => w, "pos" => "noun", "senses" => [%{"glosses" => ["A #{w}."]}]})
      end

    File.write!(path, Enum.join(lines, "\n") <> "\n")
    path
  end

  defp opts(path, repo, extra) do
    [path: path, repo: repo, batch_size: 500, validate?: true, every: 0,
     on_decode_error: :raise] ++ extra
  end

  test "on_db_error :count records the failure and continues", %{tmp_dir: dir} do
    path = write_dump(dir, ["water"])

    assert {:ok, %{db_errors: 1, upserted: 0}} =
             Import.import_all(opts(path, FailingRepo, on_db_error: :count))
  end

  test "on_db_error :raise surfaces the repo exception", %{tmp_dir: dir} do
    path = write_dump(dir, ["water"])

    assert_raise RuntimeError, "boom", fn ->
      Import.import_all(opts(path, FailingRepo, on_db_error: :raise))
    end
  end

  # CHARACTERIZATION of today's behavior (T-005): :halt escapes as an uncaught throw.
  # Delete this test when the contract below is implemented.
  test "CURRENT: on_db_error :halt throws out of import_all/1", %{tmp_dir: dir} do
    path = write_dump(dir, ["water"])

    assert {:db_error, %RuntimeError{message: "boom"}, %{upserted_so_far: 0}} =
             catch_throw(Import.import_all(opts(path, FailingRepo, on_db_error: :halt)))
  end

  # DESIRED contract (pending a decision). Remove @tag :skip once agreed.
  @tag :skip
  test "DESIRED: on_db_error :halt returns an error tuple", %{tmp_dir: dir} do
    path = write_dump(dir, ["water"])

    assert {:error, {:db_error, %RuntimeError{message: "boom"}, %{upserted_so_far: 0}}} =
             Import.import_all(opts(path, FailingRepo, on_db_error: :halt))
  end

  # assume_sorted? is a no-op today. Characterize what actually happens with
  # unsorted input so removing or implementing the option cannot change IDs silently.
  test "unsorted input still yields unique ids and loses no rows", %{tmp_dir: dir} do
    path = write_dump(dir, ["water", "well", "water"])

    assert {:ok, %{upserted: 3}} =
             Import.import_all(opts(path, CollectingRepo, assume_sorted?: false, on_db_error: :raise))

    ids =
      for _ <- 1..2, reduce: [] do
        acc ->
          receive do
            {:inserted, rows, _} -> acc ++ Enum.map(rows, & &1.id)
          after
            0 -> acc
          end
      end

    assert Enum.sort(ids) == ["water|noun|1", "water|noun|2", "well|noun|1"]
  end
end
```

Notes for whoever runs these:
- The unsorted-input test assumes one flush; if `batch_size` or flush timing differs from my reading, collect with `assert_received` per batch instead. I read `Import.flush_pending/3` but did not execute it.
- `Import` is `@moduledoc false` but public; the existing `jsonl_import_batch_test.exs` already calls `Import.import_all/1` the same way.
