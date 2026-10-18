# Compare two-turn comprehension with and without isolated neuron carryover.
#
# Non-persistent experiment: no user-event writes, no LLM response generation,
# no neuron stimulation. Existing owner-scoped Hippocampal event memory is
# intentionally NOT queried (that would contaminate the comparison).
#
# The neural arm receives a *synthetic, bounded* prior projected from the first
# turn's actual recruitment + evidence-tiered feedback plan, then decayed with
# Brain.Cell.Activation. This is not measured GenServer activity, nor proof of
# neural causation; other singleton subsystems still share in-memory state.
#
# mix run scripts/compare_neuron_comprehension.exs
# mix run scripts/compare_neuron_comprehension.exs --scenario artist --gap-ms 5000
# mix run scripts/compare_neuron_comprehension.exs --scenario bank_switch
# mix run scripts/compare_neuron_comprehension.exs \
#   --sentence "The artist draws a picture." --follow-up "what did the artist draw?"
# mix run scripts/compare_neuron_comprehension.exs --output tmp/neuron_ab.json

{options, rest, invalid} =
  OptionParser.parse(System.argv(),
    strict: [
      scenario: :string,
      gap_ms: :integer,
      sentence: :string,
      follow_up: :string,
      output: :string,
      help: :boolean
    ]
  )

if rest != [] or invalid != [] do
  raise ArgumentError, "Unsupported options: #{inspect({rest, invalid})}"
end

if options[:help] do
  IO.puts("""
  Usage: mix run scripts/compare_neuron_comprehension.exs
    [--scenario artist|bank_switch|all]
    [--gap-ms 1000]
    [--sentence FIRST --follow-up SECOND]
    [--output tmp/neuron_ab.json]

  Isolated, non-persistent counterfactual comparison. It does NOT test
  actual live Hippocampal event retrieval or neuron GenServer propagation.
  """)
else
  alias Core.Brain.ActivationProbe

  custom? = is_binary(options[:sentence]) or is_binary(options[:follow_up])

  cases =
    if custom? do
      if not (is_binary(options[:sentence]) and is_binary(options[:follow_up])) do
        raise ArgumentError, "Supply both --sentence and --follow-up."
      end

      [{"custom", options[:sentence], options[:follow_up]}]
    else
      name = Keyword.get(options, :scenario, "all")

      case name do
        "all" -> ActivationProbe.scenarios()
        "artist" -> Enum.filter(ActivationProbe.scenarios(), &(elem(&1, 0) == "artist"))
        "bank_switch" ->
          Enum.filter(ActivationProbe.scenarios(), &(elem(&1, 0) == "bank_switch"))
        _ -> raise ArgumentError, "Unknown scenario #{inspect(name)}."
      end
    end

  gap_ms = Keyword.get(options, :gap_ms, 1_000)

  IO.puts("\n=== Neuron carryover vs baseline: read-only comprehension probe ===")
  IO.puts("Gap: #{gap_ms}ms. No LLM call, no durable event write, no BrainCell stimulation.")
  IO.puts("This is a synthetic prior, not recorded live neuron activation.")
  IO.puts("The response-path recall reported by live chat is NOT exercised here.\n")

  results =
    Enum.map(cases, fn {label, first, second} ->
      report = ActivationProbe.run(first, second, gap_ms: gap_ms)

      IO.puts("\n--- #{label} ---")
      IO.puts("First: #{first}")
      IO.puts("Next:  #{second}")
      IO.puts("Active neurons in synthetic carryover: #{report.synthetic_prior_count}")
      IO.inspect(report.read_only_event_answer, label: "Read-only validated-event answer")
      IO.inspect(report.first_turn.senses, label: "First-turn LIFG IDs")
      IO.inspect(report.baseline.senses, label: "Second-turn baseline LIFG IDs")
      IO.inspect(report.neural.senses, label: "Second-turn neural-prior LIFG IDs")
      IO.inspect(report.differences, label: "Observed differences", pretty: true)
      IO.inspect(
        %{
          baseline: %{
            degraded: report.baseline.comprehension_degraded?,
            uncertain: report.baseline.uncertain,
            episodic_candidates: report.baseline.projected_event_count
          },
          neural: %{
            degraded: report.neural.comprehension_degraded?,
            uncertain: report.neural.uncertain,
            episodic_candidates: report.neural.projected_event_count
          }
        },
        label: "Comprehension comparison",
        pretty: true
      )

      Map.put(report, :scenario, label)
    end)

  if is_binary(options[:output]) do
    path = options[:output]
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Jason.encode!(results, pretty: true) <> "\n")
    IO.puts("\nDiagnostic written to #{path}")
  end

  IO.puts("\nInterpretation: a changed LIFG ID is not necessarily an improvement.")
  IO.puts("Compare with independently reviewed sense IDs and live chat traces.")
end
