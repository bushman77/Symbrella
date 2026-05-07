defmodule Brain.ScientificPipelineContractTest do
  use ExUnit.Case, async: false

  alias Brain.WorkingMemory

  defmodule LexiconFake do
    @moduledoc false

    def lookup("bank", _limit) do
      [
        %{
          id: "bank|finance",
          lemma: "bank",
          features: %{lex_fit: 0.90, rel_prior: 0.80, activation: 0.00, intent_bias: 0.00}
        },
        %{
          id: "bank|river",
          lemma: "bank",
          features: %{lex_fit: 0.20, rel_prior: 0.30, activation: 0.00, intent_bias: 0.00}
        }
      ]
    end

    def lookup(_, _limit), do: []
  end

  setup do
    :ok = Brain.PMTG.reset()
    :ok
  end

  test "pMTG rerun mode does not rerun strong, non-needy LIFG choices" do
    tokens = [%{index: 0, token_index: 0, phrase: "bank", n: 1}]

    choices = [
      %{
        token_index: 0,
        lemma: "bank",
        chosen_id: "bank|finance",
        alt_ids: ["bank|river"],
        margin: 0.72,
        probs: %{"bank|finance" => 0.91, "bank|river" => 0.09}
      }
    ]

    {:ok, result} =
      Brain.PMTG.consult_sync(
        choices,
        tokens,
        mode: :rerun,
        margin_threshold: 0.15,
        p_min: 0.65,
        lexicon_mod: LexiconFake
      )

    assert result.rerun? == false
    assert result.queries == []
    assert result.evidence == []
    assert result.choices == choices

    assert [%{stage: :pmtg} = trace_event | _] = result.si.trace
    assert_trace_contract!(trace_event)
    assert trace_event.decision == :skip
    assert trace_event.reason == :no_needy_lifg_choices
  end

  test "pMTG rerun stays bounded and explainable for weak LIFG choices" do
    tokens = [%{index: 0, token_index: 0, phrase: "bank", n: 1}]

    choices = [
      %{
        token_index: 0,
        lemma: "bank",
        chosen_id: "bank|river",
        alt_ids: ["bank|finance"],
        margin: 0.0,
        probs: %{"bank|river" => 0.50, "bank|finance" => 0.50}
      }
    ]

    {:ok, result} =
      Brain.PMTG.consult_sync(
        choices,
        tokens,
        mode: :rerun,
        margin_threshold: 0.15,
        p_min: 0.65,
        limit: 5,
        lexicon_mod: LexiconFake
      )

    assert result.rerun? == true
    assert length(result.queries) <= length(choices)
    assert [%{stage: :pmtg} = trace_event | _] = result.si.trace
    assert_trace_contract!(trace_event)
    assert trace_event.decision == :plan
    assert trace_event.reason == :weak_or_conflicted_lifg_choices
    assert trace_event.meta.needy_count == 1

    for choice <- result.choices do
      assert bounded01_or_nil?(choice[:margin])
      assert bounded_map?(choice[:probs] || %{})
      assert bounded_map?(choice[:scores] || %{})
    end
  end

  test "WM focus rejects low-confidence candidates through gate policy" do
    state = %{
      wm: [],
      attention: %{min_score: 0.0, capacity: 3},
      wm_cfg: %{
        capacity: 3,
        decay_ms: 30_000,
        gate_threshold: 0.85,
        fallback_scale: 0.70,
        lemma_budget: 16,
        replace_margin: 0.10,
        allow_unk?: true,
        allow_seed?: true,
        allow_fallback_into_wm?: true
      }
    }

    {wm, accepted, rejected} =
      Brain.WM.Focus.run(
        state,
        [%{id: "curiosity|probe|weak", lemma: "probe", score: 0.05, source: :curiosity}],
        []
      )

    assert wm == []
    assert accepted == 0
    assert rejected == 0
  end

  test "WorkingMemory normalization clamps externally supplied numeric state" do
    item = WorkingMemory.normalize(%{id: "x", score: 9.0, activation: -3.0}, 1_000)

    assert item.score == 1.0
    assert item.activation == 0.0
    assert item.inserted_at == 1_000
    assert item.last_bump == 1_000
  end

  test "LIFG Stage1 produces bounded margins and probabilities" do
    si = %{
      sentence: "bank",
      tokens: [%{index: 0, token_index: 0, phrase: "bank", span: {0, 1}}],
      sense_candidates: %{
        0 => [
          %{id: "bank|finance", features: %{lex_fit: 0.9, rel_prior: 0.8, activation: 0.2}},
          %{id: "bank|river", features: %{lex_fit: 0.7, rel_prior: 0.6, activation: 0.1}}
        ]
      }
    }

    assert {:ok, %{choices: [_ | _] = choices, audit: audit}} = Brain.LIFG.Stage1.run(si)
    assert is_map(audit)

    for choice <- choices do
      assert bounded01_or_nil?(choice[:margin])
      assert bounded_map?(choice[:probs] || %{})
      assert bounded_map?(choice[:scores] || %{})
    end
  end

  defp assert_trace_contract!(%{} = event) do
    for key <- [:stage, :input_summary, :decision, :reason, :scores, :meta, :ts_ms] do
      assert Map.has_key?(event, key), "missing trace key #{inspect(key)} in #{inspect(event)}"
    end

    assert is_atom(event.stage)
    assert is_map(event.input_summary)
    assert is_atom(event.decision)
    assert is_atom(event.reason) or is_binary(event.reason)
    assert bounded_map?(event.scores)
    assert is_map(event.meta)
    assert is_integer(event.ts_ms)
  end

  defp bounded_map?(scores) when is_map(scores) do
    Enum.all?(scores, fn {_key, value} -> is_number(value) and value >= 0.0 and value <= 1.0 end)
  end

  defp bounded_map?(_), do: false

  defp bounded01_or_nil?(nil), do: true
  defp bounded01_or_nil?(value) when is_number(value), do: value >= 0.0 and value <= 1.0
  defp bounded01_or_nil?(_), do: false
end
