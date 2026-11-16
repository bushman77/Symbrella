# Operational self-awareness v1 evidence-register audit.
#
# No application or database startup required:
#   mix run --no-start scripts/audit_operational_awareness_v1.exs
#   mix run --no-start scripts/audit_operational_awareness_v1.exs --receipts tmp/awareness_v1_receipts.json
#   mix run --no-start scripts/audit_operational_awareness_v1.exs --receipts tmp/awareness_v1_receipts.json --strict
#
# This cannot authenticate evidence or approve the release. The register
# checks shape and completeness only; an independent evaluator must reproduce
# the referenced artifacts and verify their applicability.

alias Core.Release.OperationalAwarenessV1, as: ReleaseGate

{opts, rest, errors} =
  OptionParser.parse(System.argv(),
    strict: [receipts: :string, strict: :boolean, help: :boolean]
  )

cond do
  opts[:help] ->
    IO.puts("Usage: mix run --no-start scripts/audit_operational_awareness_v1.exs [--receipts FILE] [--strict]")

  errors != [] or rest != [] ->
    IO.puts(:stderr, "Invalid audit arguments. Use --help.")
    System.halt(2)

  true ->
    receipts =
      case opts[:receipts] do
        nil ->
          []

        path ->
          with {:ok, raw} <- File.read(path),
               {:ok, list} when is_list(list) <- Jason.decode(raw) do
            list
          else
            _ ->
              IO.puts(:stderr, "Receipts file must be a JSON array.")
              System.halt(2)
          end
      end

    assessment = ReleaseGate.assess(receipts)

    IO.puts("\n=== Operational self-awareness v1 evidence readiness ===")
    IO.puts("Status: #{inspect(assessment.status)}")

    if Map.has_key?(assessment, :criteria) do
      requirements = ReleaseGate.criteria() |> Map.new(&{&1.id, &1})

      Enum.each(assessment.criteria, fn row ->
        criterion = Map.fetch!(requirements, row.id)
        IO.puts("- #{criterion.title}: #{row.status} (#{row.reason})")
      end)

      IO.puts("Evidence declarations: #{assessment.declared}/#{assessment.total}")
      IO.puts("Missing: #{assessment.missing} | Invalid: #{assessment.invalid}")
    else
      IO.puts("Rejected: #{inspect(assessment.reason)}")
    end

    IO.puts("External verification: NOT ESTABLISHED")
    IO.puts("Release approval: NOT GRANTED")
    IO.puts("Consciousness: NOT TESTED")
    IO.puts("No runtime cognition or policy was modified.")

    if opts[:strict] and not assessment.ready_for_external_audit?,
      do: System.halt(1)
end
