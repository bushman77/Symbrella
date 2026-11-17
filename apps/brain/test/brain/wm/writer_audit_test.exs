defmodule Brain.WM.WriterAuditTest do
  use ExUnit.Case, async: true

  alias Brain.WM.WriterAudit

  @root Path.expand("../../../../../", __DIR__)

  test "known production writers are explicitly reviewed and no new AST write has appeared" do
    assert {:ok, report} = WriterAudit.verify(@root)
    assert report.status == :reviewed_static_inventory
    assert report.recognized_writes == 10
    assert report.files == 5
    refute report.runtime_coverage_proven?
    assert report.dynamic_bypass_excluded?

    assert Enum.count(report.events, &(&1.kind == :working_memory_upsert)) == 2
    assert Enum.count(report.events, &(&1.kind == :map_put)) == 4
    assert Enum.count(report.events, &(&1.kind == :map_update)) == 4
    assert Enum.all?(report.events, &(&1.line > 0))

    observed =
      report.events
      |> Enum.group_by(& &1.path)
      |> Map.new(fn {path, rows} ->
        {path, Enum.frequencies_by(rows, & &1.kind)}
      end)

    assert WriterAudit.expected() == observed
  end

  test "scanner detects direct and piped map writes, map updates, and upserts" do
    source = """
    defmodule UnreviewedWriter do
      def direct(state), do: Map.put(state, :wm, [])
      def piped(state), do: state |> Map.put(:wm, [])
      def update(state), do: %{state | wm: []}
      def alternative(state), do: Map.replace!(state, :wm, [])
      def insert(wm, item, cfg), do: WorkingMemory.upsert(wm, item, cfg)
    end
    """

    assert {:ok, events} = WriterAudit.source_events(source)
    kinds = Enum.frequencies_by(events, & &1.kind)
    assert kinds == %{map_put: 3, map_update: 1, working_memory_upsert: 1}
    assert Enum.all?(events, &(&1.line > 0))
  end

  test "scanner does not count WM snapshots, local map literals, or removal-only operations" do
    source = """
    defmodule ReadOnlyConsumer do
      def inspect_state(state) do
        _ = %{wm: state.wm}
        _ = Map.get(state, :wm)
        _ = WorkingMemory.remove(state.wm, "old")
        state |> Map.put(:wm_last_ms, 10)
      end
    end
    """

    assert {:ok, []} = WriterAudit.source_events(source)
  end

  test "new code in an unreviewed production module is rejected" do
    root = Path.join(System.tmp_dir!(),
      "symbrella-wm-write-audit-#{System.unique_integer([:positive])}")

    dir = Path.join(root, "apps/brain/lib/brain")
    File.mkdir_p!(dir)
    File.write!(Path.join(dir, "rogue.ex"), """
    defmodule Rogue do
      def force(state), do: Map.put(state, :wm, [%{id: "unreviewed"}])
    end
    """)

    on_exit(fn -> File.rm_rf!(root) end)

    assert {:ok, [%{kind: :map_put, path: "apps/brain/lib/brain/rogue.ex"}]} =
      WriterAudit.inventory(root)

    assert {:error, {:writer_inventory_changed, %{observed: observed}}} =
      WriterAudit.verify(root)

    assert observed == %{"apps/brain/lib/brain/rogue.ex" => %{map_put: 1}}
  end

  test "parse failures and empty production source trees fail closed" do
    assert {:error, _} = WriterAudit.source_events("defmodule Broken do")
    assert {:error, :invalid_source} = WriterAudit.source_events(:not_elixir)
    assert {:error, :no_production_elixir_sources} =
      WriterAudit.inventory(Path.join(System.tmp_dir!(), "nonexistent-wm-scan-#{System.unique_integer([:positive])}"))
  end
end
