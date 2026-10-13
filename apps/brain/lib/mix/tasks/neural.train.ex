defmodule Mix.Tasks.Neural.Train do
  @moduledoc """
  Offline, advisory Axon memory-relevance pilot:

      mix neural.train --task memory_relevance --dir tmp/training
      mix neural.train --task memory_relevance --epochs 50

  Requires human-reviewed relevance labels from independent sessions. Never
  applies model weights to the live Hippocampus or any response component.
  """
  use Mix.Task

  alias Brain.Neural.{TrainingBuilder, MemoryRelevancePilot}

  @shortdoc "Train and evaluate experimental reviewed-memory Axon model"

  @impl Mix.Task
  def run(argv) do
    {opts, positional, invalid} =
      OptionParser.parse(argv,
        strict: [task: :string, dir: :string, epochs: :integer, hidden_units: :integer, smoke: :boolean]
      )

    if invalid != [] or positional != [] or
         Keyword.get(opts, :task) != "memory_relevance" do
      Mix.raise(
        "Usage: mix neural.train --task memory_relevance [--dir DIR] [--epochs N] [--smoke]"
      )
    end

    Mix.Task.run("compile")
    dir = Keyword.get(opts, :dir, "tmp/training")

    options =
      opts
      |> Keyword.take([:epochs, :hidden_units])

    smoke? = Keyword.get(opts, :smoke, false)

    dataset_result =
      if smoke?,
        do: TrainingBuilder.build_smoke(dir, :memory_relevance),
        else: TrainingBuilder.build(dir, :memory_relevance)

    with {:ok, dataset} <- dataset_result,
         {:ok, artifact} <- MemoryRelevancePilot.train(dataset, options) do
      Mix.shell().info("Experimental memory-relevance Axon pilot")
      Mix.shell().info("Train: #{artifact.train_examples} reviewed candidates")
      Mix.shell().info("Training sessions: #{length(artifact.train_sessions)}")
      Mix.shell().info("Training MSE: #{Float.round(artifact.train_mse, 5)}")
      Mix.shell().info("Training labels: #{artifact.labels.train_positive} relevant / #{artifact.train_examples - artifact.labels.train_positive} irrelevant")
      Mix.shell().info("Train-label prevalence baseline MSE: #{Float.round(artifact.class_prior_train_mse, 5)}")

      if smoke? do
        Mix.shell().info("SMOKE TEST: training and evaluation use the SAME examples.")
        Mix.shell().info("In-sample model MSE: #{Float.round(artifact.eval_mse, 5)}")
        Mix.shell().info("In-sample recall-score baseline MSE: #{Float.round(artifact.baseline_eval_mse, 5)}")
        Mix.shell().info("In-sample class-prior baseline MSE: #{Float.round(artifact.class_prior_eval_mse, 5)}")
        Mix.shell().info("NO holdout evaluation; the model has NOT demonstrated generalization.")
      else
        Mix.shell().info("Holdout: #{artifact.eval_examples} reviewed candidates")
        Mix.shell().info("Evaluation sessions: #{length(artifact.eval_sessions)}")
        Mix.shell().info("Holdout MSE: #{Float.round(artifact.eval_mse, 5)}")
        Mix.shell().info("Recall-score baseline holdout MSE: #{Float.round(artifact.baseline_eval_mse, 5)}")
        Mix.shell().info("Class-prior baseline holdout MSE: #{Float.round(artifact.class_prior_eval_mse, 5)}")
      end

      if artifact.train_examples < 20 or artifact.eval_examples < 20 do
        Mix.shell().info("WARNING: Tiny pilot sample: the metrics are not a reliable quality estimate.")
      end

      if artifact.labels.train_positive == 0 or
           artifact.labels.train_positive == artifact.train_examples do
        Mix.shell().info("WARNING: Training labels have only one class; collect both y and n.")
      end

      filename = if smoke?, do: "memory_relevance_smoke.etf", else: "memory_relevance_pilot.etf"
      path = Path.join(dir, filename)

      case MemoryRelevancePilot.save(artifact, path) do
        :ok ->
          Mix.shell().info("Saved experimental checkpoint: #{path}")
          Mix.shell().info("Live memory ranking and Symbrella responses remain unchanged.")

        {:error, reason} ->
          Mix.raise("Trained, but could not save experimental checkpoint: #{inspect(reason)}")
      end
    else
      {:error, reason} ->
        Mix.raise("Cannot run reviewed Axon pilot: #{inspect(reason)}")
    end
  end
end
