defmodule Db.Lexicon do
  @moduledoc false
  # Stub so pMTG.fetch_evidence/3 finds lexicon senses during the test.
  def lookup("bank", _limit) do
    [
      %{
        id: "bank|finance",
        lemma: "bank",
        features: %{lex_fit: 0.90, rel_prior: 0.60, activation: 0.00, intent_bias: 0.00}
      },
      %{
        id: "bank|river",
        lemma: "bank",
        features: %{lex_fit: 0.20, rel_prior: 0.40, activation: 0.00, intent_bias: 0.00}
      }
    ]
  end

  def lookup(_, _), do: []
end

defmodule Brain.PMTG.RerunTest do
  use ExUnit.Case, async: false

  setup do
    # keep server state clean between tests
    :ok = Brain.PMTG.reset()

    handler_id = "pmtg-rerun-#{System.unique_integer([:positive])}"
    test_pid = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :pmtg, :rerun],
        fn event, meas, meta, _config ->
          # IMPORTANT: send to the *test* process, not the callback's self()
          send(test_pid, {:telemetry, event, meas, meta})
        end,
        nil
      )

    on_exit(fn -> :telemetry.detach(handler_id) end)
    :ok
  end

  test "consult_sync/3 reruns and returns merged choices, emitting telemetry" do
    # Minimal SI surface for the token under test
    tokens = [%{index: 0, phrase: "bank", n: 1}]

    # Pretend Stage-1 returned a low-confidence tie favoring the wrong sense initially.
    choices = [
      %{
        token_index: 0,
        lemma: "bank",
        chosen_id: "bank|river",
        alt_ids: ["bank|finance"],
        margin: 0.0,
        scores: %{"bank|river" => 0.5, "bank|finance" => 0.5}
      }
    ]

    # Run pMTG in :rerun mode *synchronously* so we get merged results back.
    {:ok, %{choices: merged, mode: :rerun, rerun?: true}} =
      Brain.PMTG.consult_sync(
        choices,
        tokens,
        mode: :rerun,
        already_needy: true,
        limit: 5,
        # Optional bias to make the flip toward lex_fit even crisper
        rerun_weights_bump: %{lex_fit: 0.05}
      )

    # Expect the richer lexicon evidence to flip the winner to "bank|finance".
    assert [%{token_index: 0, chosen_id: "bank|finance"}] = merged

    # And we should observe the rerun telemetry (emitted synchronously before the reply).
    assert_receive {:telemetry, [:brain, :pmtg, :rerun], _meas, _meta}, 500
  end
end
