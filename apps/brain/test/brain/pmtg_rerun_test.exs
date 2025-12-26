defmodule Brain.PMTG.RerunTest do
  use ExUnit.Case, async: false

  defmodule TestLexicon do
    @moduledoc false

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

  def handle_rerun_event(event, meas, meta, test_pid) do
    send(test_pid, {:telemetry, event, meas, meta})
  end

  setup do
    :ok = Brain.PMTG.reset()

    handler_id = "pmtg-rerun-#{System.unique_integer([:positive])}"
    test_pid = self()

    :ok =
      :telemetry.attach(
        handler_id,
        [:brain, :pmtg, :rerun],
        &Brain.PMTG.RerunTest.handle_rerun_event/4,
        test_pid
      )

    on_exit(fn -> :telemetry.detach(handler_id) end)
    :ok
  end

  test "consult_sync/3 reruns and returns merged choices, emitting telemetry" do
    tokens = [%{index: 0, phrase: "bank", n: 1}]

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

    {:ok, %{choices: merged, mode: :rerun, rerun?: true}} =
      Brain.PMTG.consult_sync(
        choices,
        tokens,
        mode: :rerun,
        already_needy: true,
        limit: 5,
        lexicon_mod: TestLexicon,
        rerun_weights_bump: %{lex_fit: 0.05}
      )

    assert [%{token_index: 0, chosen_id: "bank|finance"}] = merged
    assert_receive {:telemetry, [:brain, :pmtg, :rerun], _meas, _meta}, 500
  end
end
