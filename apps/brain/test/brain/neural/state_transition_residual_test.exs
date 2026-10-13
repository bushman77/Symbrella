defmodule Brain.Neural.StateTransitionResidualTest do
  use ExUnit.Case, async: true

  alias Brain.Neural.{StateTransitionBuilder, StateTransitionMask, StateTransitionResidual}

  @width length(Brain.Neural.StateTransitionBuilder.feature_names())

  defp batch(current, observed) do
    %{
      x: Nx.tensor(current, type: :f32),
      y: Nx.tensor(observed, type: :f32),
      mask:
        Nx.tensor(
          Enum.map(current, fn _ ->
            Enum.map(StateTransitionBuilder.feature_names(), fn name ->
              if name in [
                   :confidence_present, :mood_da_present, :mood_5ht_present,
                   :mood_glu_present, :mood_ne_present, :mood_present,
                   :self_monitor_present
                 ],
                do: 0.0,
                else: 1.0
            end)
          end),
          type: :f32
        ),
      feature_names: StateTransitionBuilder.feature_names()
    }
  end

  defp data do
    a = List.duplicate(0.3, @width)
    b = List.duplicate(0.4, @width)
    c = List.duplicate(0.7, @width)
    d = List.duplicate(0.65, @width)

    %{
      train: batch([a, b], [b, a]),
      eval: batch([c], [d]),
      metadata: %{
        task: :state_transition,
        feature_schema_version: StateTransitionBuilder.feature_schema_version(),
        train_sessions: ["session-train"],
        eval_sessions: ["session-holdout"],
        split_mode: :session_holdout
      }
    }
  end

  test "untrained zero-initialized delta network reproduces persistence exactly" do
    x = Nx.tensor([List.duplicate(0.35, @width)], type: :f32)
    model = StateTransitionResidual.build_model(hidden_units: 4)
    {init, predict} = Axon.build(model)

    params = init.(Nx.template({1, @width}, :f32), Axon.ModelState.empty())
    delta = predict.(params, x)

    for v <- delta |> Nx.to_flat_list() do
      assert_in_delta v, 0.0, 1.0e-7
    end

    predicted = StateTransitionResidual.predict_next(x, delta)
    assert Nx.to_list(predicted) == Nx.to_list(x)
  end

  test "residual state reconstruction clamps to valid state range" do
    x = Nx.tensor([[0.9, 0.1, 0.6]], type: :f32)
    predicted_delta = Nx.tensor([[0.5, -0.8, -0.1]], type: :f32)
    [[a, b, c]] = x |> StateTransitionResidual.predict_next(predicted_delta) |> Nx.to_list()

    assert a == 1.0
    assert b == 0.0
    assert_in_delta c, 0.5, 0.000001
  end

  test "per-feature MSE matches calculated values and baseline" do
    names = [:dopamine, :confidence]

    x = Nx.tensor([[0.25, 0.25], [0.75, 0.75]])
    y = Nx.tensor([[0.5, 0.25], [0.5, 0.75]])
    predicted = Nx.tensor([[0.5, 0.25], [0.5, 0.75]])

    report = StateTransitionResidual.metrics(
      names, predicted, x, y, Nx.broadcast(1.0, Nx.shape(x))
    )

    assert_in_delta report.model_mse, 0.0, 1.0e-8
    assert_in_delta report.persistence_mse, 0.03125, 1.0e-8
    assert [
             %{feature: :dopamine, model_mse: model, persistence_mse: persistence},
             %{feature: :confidence, model_mse: _, persistence_mse: zero}
           ] = report.per_feature
    assert_in_delta model, 0.0, 1.0e-8
    assert_in_delta persistence, 0.0625, 1.0e-8
    assert_in_delta zero, 0.0, 1.0e-8
  end

  test "trains deltas, reports held-out feature metrics, stays isolated from live runtime" do
    assert {:ok, result} = StateTransitionResidual.train(data(), epochs: 1, hidden_units: 4)
    assert result.model_version == "state-transition-residual-v3"
    assert result.task == :state_transition_residual
    assert result.objective == :observed_next_minus_current
    assert result.zero_initialized_residual? == true
    assert result.evaluated? == true
    assert result.split_mode == :session_holdout
    assert result.train_examples == 2
    assert result.eval_examples == 1
    assert result.train_sessions == ["session-train"]
    assert result.eval_sessions == ["session-holdout"]
    assert result.train_mse >= 0.0
    assert result.eval_mse >= 0.0
    assert result.persistence_eval_mse > 0.0
    assert result.valid_eval_cells == @width - 7
    assert result.valid_train_cells == 2 * (@width - 7)
    assert length(result.eval_per_feature) == @width
    assert Enum.map(result.eval_per_feature, & &1.feature) == StateTransitionBuilder.feature_names()
    assert Enum.all?(result.eval_per_feature, fn row ->
             row.model_mse >= 0.0 and row.persistence_mse >= 0.0
           end)
    assert result.deployment_status == :experimental_not_for_runtime
  end

  test "rejects schema and session leakage, except deliberately marked smoke runs" do
    d = data()
    assert {:error, :incorrect_training_task} =
             StateTransitionResidual.train(put_in(d, [:metadata, :task], :uncertainty))

    assert {:error, :feature_schema_mismatch} =
             StateTransitionResidual.train(put_in(d, [:train, :feature_names], [:something]))

    assert {:error, :session_leakage} =
             StateTransitionResidual.train(
               put_in(d, [:metadata, :eval_sessions], ["session-train"])
             )

    assert {:error, :invalid_training_options} =
             StateTransitionResidual.train(d, epochs: 0)

    smoke =
      d
      |> put_in([:metadata, :split_mode], :in_sample_smoke)
      |> put_in([:metadata, :eval_sessions], ["session-train"])

    assert {:ok, result} = StateTransitionResidual.train(smoke, epochs: 1)
    assert result.evaluated? == false
  end

  test "writes a distinct private checkpoint, without touching the old direct pilot" do
    assert {:ok, result} = StateTransitionResidual.train(data(), epochs: 1)
    dir = Path.join(System.tmp_dir!(), "symbrella_residual_#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)

    direct_path = Path.join(dir, "state_transition_pilot.etf")
    File.write!(direct_path, "old-baseline-weights")
    residual_v1_path = Path.join(dir, "state_transition_residual_pilot.etf")
    File.write!(residual_v1_path, "old-residual-weights")
    residual_v2_path = Path.join(dir, "state_transition_residual_v2_pilot.etf")
    File.write!(residual_v2_path, "old-v2-residual-weights")
    residual_path = Path.join(dir, "state_transition_residual_v3_pilot.etf")

    assert :ok == StateTransitionResidual.save(result, residual_path)
    assert File.read!(direct_path) == "old-baseline-weights"
    assert File.read!(residual_v1_path) == "old-residual-weights"
    assert File.read!(residual_v2_path) == "old-v2-residual-weights"
    assert Bitwise.band(File.stat!(residual_path).mode, 0o777) == 0o600

    loaded = residual_path |> File.read!() |> :erlang.binary_to_term([:safe])
    assert loaded.model_version == result.model_version
    assert loaded.feature_names == StateTransitionBuilder.feature_names()
    assert loaded.params != nil
  end
  test "missing target values have zero effect on the masked objective or MSE" do
    predicted = Nx.tensor([[0.25, 0.9], [0.75, 0.2]], type: :f32)
    truth_a = Nx.tensor([[0.5, 0.1], [0.5, 0.1]], type: :f32)
    truth_b = Nx.tensor([[0.5, 100.0], [0.5, -100.0]], type: :f32)
    mask = Nx.tensor([[1.0, 0.0], [1.0, 0.0]], type: :f32)

    loss_a = StateTransitionMask.loss(%{values: truth_a, mask: mask}, predicted) |> Nx.to_number()
    loss_b = StateTransitionMask.loss(%{values: truth_b, mask: mask}, predicted) |> Nx.to_number()
    assert_in_delta loss_a, 0.0625, 1.0e-7
    assert_in_delta loss_a, loss_b, 1.0e-7

    a = StateTransitionMask.metrics([:observed, :missing], predicted, predicted, truth_a, mask)
    b = StateTransitionMask.metrics([:observed, :missing], predicted, predicted, truth_b, mask)
    assert a.valid_cells == 2
    assert_in_delta a.model_mse, b.model_mse, 1.0e-7
    assert [%{valid_pairs: 2}, %{valid_pairs: 0, model_mse: :not_observed,
                                  persistence_mse: :not_observed}] = a.per_feature
  end

  test "trainer refuses to use all-missing targets instead of reporting fake zero error" do
    dataset = data()
    blank = Nx.broadcast(0.0, Nx.shape(dataset.train.mask))
    dataset = put_in(dataset, [:train, :mask], blank)
    assert {:error, :no_observed_training_targets} =
             StateTransitionResidual.train(dataset, epochs: 1)
  end


end
