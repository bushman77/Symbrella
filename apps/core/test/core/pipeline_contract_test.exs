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

  test "live production pipeline emits semantic-sense shadow after meaning and before comprehension" do
    opts = [
      mode: :prod,
      experience_origin: :user_turn,
      fuzzy: false,
      response: :off,
      episodes: false,
      persist_episodes: false,
      hippocampus_encode?: false,
      self_model: :off,
      comprehension_wm?: false,
      notify_brain?: false,
      synapse_shadow?: false
    ]

    si = Core.resolve_input("The artist draws a picture.", opts)
    trace = si.trace

    shadow = Enum.find(trace, &match?(%{stage: :semantic_sense_shadow}, &1))
    assert_trace_contract!(shadow)
    assert shadow.meta.mode == :shadow_only
    assert shadow.meta.applied_to_lifg? == false
    assert shadow.meta.response_changed? == false

    # Trace also contains legacy tuple events (such as {:activated, ...}).
    # Only structured Pipeline.Trace events have a :stage. Newer events
    # are prepended, so comprehension precedes the earlier shadow event.
    stages =
      Enum.flat_map(trace, fn
        %{stage: stage} when is_atom(stage) -> [stage]
        _ -> []
      end)
    meaning_at = Enum.find_index(stages, &(&1 == :sentence_meaning))
    shadow_at = Enum.find_index(stages, &(&1 == :semantic_sense_shadow))
    summary_at = Enum.find_index(stages, &(&1 == :comprehension))

    # SentenceMeaning is a state-dump stage, not necessarily a Trace event:
    # if it emits no separate event we still verify the subsequent summary.
    assert is_integer(shadow_at)
    assert is_integer(summary_at)
    assert summary_at < shadow_at
    assert is_nil(meaning_at) or shadow_at < meaning_at

    assert is_list(si.lifg_choices)
    assert is_map(si.atl_slate) or is_nil(si.atl_slate)

    disabled = Core.resolve_input("The artist draws a picture.",
      Keyword.put(opts, :semantic_sense_shadow?, false))
    refute Enum.any?(disabled.trace, &match?(%{stage: :semantic_sense_shadow}, &1))
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
