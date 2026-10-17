# Read-only audit of historical Hippocampal episodes.
#
# Examples:
# mix run scripts/audit_episodic_reconstruction.exs --limit 100
# mix run scripts/audit_episodic_reconstruction.exs --limit 1000 --include-legacy
# mix run scripts/audit_episodic_reconstruction.exs --after-id 500 --output tmp/episodic_audit.jsonl
#
# No flag permits production writes.
import Ecto.Query

{opts, extra, invalid} =
  OptionParser.parse(System.argv(),
    strict: [limit: :integer, after_id: :integer, include_legacy: :boolean,
      output: :string]
  )

if extra != [] or invalid != [] do
  raise ArgumentError, "Unsupported options: #{inspect({extra, invalid})}"
end

limit = opts |> Keyword.get(:limit, 100) |> min(1000) |> max(1)
after_id = opts |> Keyword.get(:after_id, 0) |> max(0)
include_legacy? = Keyword.get(opts, :include_legacy, false)

rows =
  Db.all(
    from(e in Db.Episode,
      where: e.id > ^after_id,
      order_by: [asc: e.id],
      limit: ^limit,
      select: %{
        id: e.id, sentence: e.sentence, role: e.role, source: e.source,
        session_id: e.session_id, inserted_at: e.inserted_at, meta: e.meta
      })
  )

previews =
  rows
  |> Enum.map(&Core.Episodic.Reconstruction.preview_row/1)
  |> Enum.filter(fn result ->
    result.candidate_count > 0 and
      (include_legacy? or result.attribution == :attributed_user)
  end)

IO.puts("Read-only reconstruction audit: inspected=#{length(rows)} " <>
  "candidate_rows=#{length(previews)} include_legacy=#{include_legacy?}")
IO.puts("No PostgreSQL rows have been created or modified.")

out = Enum.map_join(previews, "\n", &Jason.encode!/1)
if is_binary(opts[:output]) do
  File.mkdir_p!(Path.dirname(opts[:output]))
  File.write!(opts[:output], out <> if(out == "", do: "", else: "\n"))
  IO.puts("Candidate review file: #{opts[:output]}")
else
  if out != "", do: IO.puts(out)
end
