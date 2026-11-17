# Read-only source-level audit of recognized production WM mutation sites.
#
# mix run --no-start scripts/audit_wm_writers.exs
#
# This is a source inventory, NOT proof that BEAM runtime code cannot directly
# replace process state or generate code outside reviewed source paths.

alias Brain.WM.WriterAudit

root = Path.expand("..", __DIR__)

case WriterAudit.verify(root) do
  {:ok, report} ->
    IO.puts("=== Phase 9A - Working Memory source-writer inventory ===")
    IO.puts("Status: #{report.status}")
    IO.puts("Recognized Brain WM write operations: #{report.recognized_writes}")
    IO.puts("Reviewed non-runtime empty-map defaults: #{report.reviewed_empty_map_defaults}")
    IO.puts("Inspected WM-key operations: #{report.inspected_operations}")
    IO.puts("Reviewed modules: #{report.files}")

    Enum.each(report.events, fn item ->
      IO.puts("#{item.path}:#{item.line} | #{item.kind}")
    end)

    IO.puts("Dynamic bypass coverage: NOT ESTABLISHED")
    IO.puts("Runtime admission coverage: NOT CERTIFIED BY THIS SCAN")
    IO.puts("V1 gate: STILL OPEN; requires independent evidence")

  {:error, {:writer_inventory_changed, info}} ->
    IO.puts(:stderr, "Unreviewed or missing production WM writer detected.")
    IO.inspect(info.observed, label: "Observed")
    IO.inspect(info.expected, label: "Expected")
    System.halt(1)

  {:error, reason} ->
    IO.puts(:stderr, "Cannot complete WM writer inventory: #{inspect(reason)}")
    System.halt(2)
end
