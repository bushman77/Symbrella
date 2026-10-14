defmodule Mix.Tasks.Neural.Dataset do
  @moduledoc """
  Inspect versioned Nx training datasets without changing model weights.

      mix neural.dataset --task memory_relevance
      mix neural.dataset --task uncertainty
      mix neural.dataset --task state_transition
      mix neural.dataset --task state_transition --smoke

  The state-transition dataset is self-supervised from observed successive
  cognitive turns. Human-reviewed relevance/uncertainty remain separate.
  """

  use Mix.Task

  @shortdoc "Build and inspect Nx datasets from the Symbrella training archive"

  @impl Mix.Task
  def run(argv) do
    {opts, args, invalid} =
      OptionParser.parse(argv,
        strict: [task: :string, dir: :string, eval_fraction: :float, smoke: :boolean]
      )

    if invalid != [] or args != [] do
      Mix.raise("Usage: mix neural.dataset --task memory_relevance|uncertainty|state_transition [--dir DIR] [--smoke]")
    end

    task =
      case Keyword.get(opts, :task) do
        "memory_relevance" -> :memory_relevance
        "uncertainty" -> :uncertainty
        "state_transition" -> :state_transition
        _ -> Mix.raise("Specify --task memory_relevance, uncertainty or state_transition")
      end

    Mix.Task.run("compile")
    dir = Keyword.get(opts, :dir, "tmp/training")
    smoke? = Keyword.get(opts, :smoke, false)

    result =
      case task do
        :state_transition ->
          Brain.Neural.StateTransitionBuilder.build(dir, smoke: smoke?)

        task ->
          if smoke? do
            Brain.Neural.TrainingBuilder.build_smoke(dir, task)
          else
            fraction = Keyword.get(opts, :eval_fraction, 0.2)
            Brain.Neural.TrainingBuilder.build(dir, task, eval_fraction: fraction)
          end
      end

    case result do
      {:ok, %{metadata: metadata, train: train, eval: evaluation}} ->
        Mix.shell().info("Task: #{task}")
        Mix.shell().info("Feature schema: #{metadata.feature_schema_version}")
        Mix.shell().info("Features: #{inspect(metadata.feature_names)}")
        Mix.shell().info("Training rows: #{metadata.train_examples}")
        Mix.shell().info("Evaluation rows: #{metadata.eval_examples}")
        Mix.shell().info("Training tensor: #{inspect(Nx.shape(train.x))}")
        Mix.shell().info("Target tensor: #{inspect(Nx.shape(train.y))}")
        Mix.shell().info("Evaluation tensor: #{inspect(Nx.shape(evaluation.x))}")
        Mix.shell().info("Split mode: #{Map.get(metadata, :split_mode, :session_holdout)}")
        if task == :state_transition do
          Mix.shell().info(
            "Raw neuromodulator snapshots: #{metadata.complete_modulator_turns}/" <>
              "#{metadata.input_turns} complete; #{metadata.missing_modulator_turns} missing/incomplete"
          )
          Mix.shell().info(
            "Complete process-local Hippocampus traces: #{metadata.complete_process_traces}/" <>
              "#{metadata.input_turns}"
          )
          Mix.shell().info(
            "Explicit memory fast-path turns: #{metadata.memory_fast_path_turns}/" <>
              "#{metadata.input_turns}"
          )
          Mix.shell().info(
            "Observed-only target cells: #{metadata.train_valid_cells}/#{metadata.train_possible_cells} training; " <>
              "#{metadata.eval_valid_cells}/#{metadata.eval_possible_cells} evaluation"
          )
          Mix.shell().info("Presence/availability flags are inputs, not prediction targets.")
        end
        Mix.shell().info("No model weights were changed.")

      {:error, reason} ->
        Mix.raise("Cannot build training data: #{inspect(reason)}")
    end
  end
end
