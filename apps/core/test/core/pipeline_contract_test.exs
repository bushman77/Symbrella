defmodule Core.PipelineContractTest do
  use ExUnit.Case, async: false

  alias Core.Invariants
  alias Core.SemanticInput

  defmodule LexiconPass do
    @moduledoc false
    def run(si), do: si
    def run(si, _opts), do: si
  end

  defmodule GateSkip do
    @moduledoc false
    def gate(si, _opts), do: {:skip, si}
  end

  defmodule ExecutePass do
    @moduledoc false
    def execute(si, _plan), do: si
  end

  @resolve_opts [
    mode: :test,
    lexicon_mod: LexiconPass,
    gate_mod: GateSkip,
    execute_mod: ExecutePass
  ]

  test "resolve_input returns a bounded SemanticInput with safe token invariants" do
    si = Core.resolve_input("Kick the bucket near the river bank", @resolve_opts)

    assert %SemanticInput{} = si
    assert is_binary(si.sentence)
    assert si.source == :test
    assert is_atom(si.intent)
    assert bounded01_or_nil?(si.confidence)
    assert is_list(si.tokens)

    assert :ok = Invariants.assert_no_chargrams!(si.tokens)
    assert :ok = Invariants.assert_boundary_only_or_mwe!(si.tokens, si.sentence)

    for {token_index, candidates} <- si.sense_candidates do
      assert is_integer(token_index) and token_index >= 0
      assert is_list(candidates)
    end
  end

  test "major Core trace entries use the audit contract shape" do
    si = Core.resolve_input("Hello Symbrella", @resolve_opts)

    intent_event = Enum.find(si.trace, &match?(%{stage: :intent}, &1))
    assert_trace_contract!(intent_event)

    assert intent_event.decision == si.intent
    assert intent_event.reason == :intent_selection
    assert bounded_scores?(intent_event.scores)
    assert intent_event.meta.keyword == si.keyword
  end

  test "prod pipeline attaches a comprehension summary" do
    si = Core.resolve_input("tell me about your working memory", max_wordgram_n: 3)

    assert %{intent: intent, understood: understood, uncertain: uncertain, degraded?: degraded?} =
             si.comprehension

    assert is_atom(intent)
    assert is_list(understood)
    assert is_list(uncertain)
    assert is_boolean(degraded?)

    event = Enum.find(si.trace, &match?(%{stage: :comprehension}, &1))
    assert_trace_contract!(event)
  end

  defp assert_trace_contract!(%{} = event) do
    for key <- [:stage, :input_summary, :decision, :reason, :scores, :meta, :ts_ms] do
      assert Map.has_key?(event, key), "missing trace key #{inspect(key)} in #{inspect(event)}"
    end

    assert is_atom(event.stage)
    assert is_map(event.input_summary)
    assert is_atom(event.decision)
    assert is_atom(event.reason) or is_binary(event.reason)
    assert bounded_scores?(event.scores)
    assert is_map(event.meta)
    assert is_integer(event.ts_ms)
  end

  defp assert_trace_contract!(other),
    do: flunk("expected trace event map, got: #{inspect(other)}")

  defp bounded_scores?(scores) when is_map(scores) do
    Enum.all?(scores, fn {_key, value} -> is_number(value) and value >= 0.0 and value <= 1.0 end)
  end

  defp bounded_scores?(_), do: false

  defp bounded01_or_nil?(nil), do: true
  defp bounded01_or_nil?(value) when is_number(value), do: value >= 0.0 and value <= 1.0
  defp bounded01_or_nil?(_), do: false
end
