defmodule Mix.Tasks.Neural.Dataset do
  @moduledoc """
  Build and summarize a reviewed neural training dataset without starting
  the Symbrella runtime or changing model weights.

      mix neural.dataset --task memory_relevance
      mix neural.dataset --task uncertainty --dir tmp/training

  The command validates the private JSONL archives in the chosen directory.
  It prints only counts and feature names, never conversation contents.
  """

  use Mix.Task

  @shortdoc "Validate reviewed Symbrella training archive and build Nx batches"

  @impl Mix.Task
  def run(argv) do
    {opts, args, invalid} =
      OptionParser.parse(argv,
        strict: [task: :string, dir: :string, eval_fraction: :float]
      )

    if invalid != [] or args != [] do
      Mix.raise("Invalid options. Usage: mix neural.dataset --task memory_relevance|uncertainty [--dir DIR]")
    end

    task =
      case Keyword.get(opts, :task) do
        "memory_relevance" -> :memory_relevance
        "uncertainty" -> :uncertainty
        _ -> Mix.raise("Specify --task memory_relevance or --task uncertainty")
      end

    Mix.Task.run("compile")

    dir = Keyword.get(opts, :dir, "tmp/training")

    build_opts =
      case Keyword.fetch(opts, :eval_fraction) do
        {:ok, fraction} -> [eval_fraction: fraction]
        :error -> []
      end

    case Brain.Neural.TrainingBuilder.build(dir, task, build_opts) do
      {:ok, %{metadata: metadata, train: train, eval: eval_batch}} ->
        Mix.shell().info("Task: #{task}")
        Mix.shell().info("Feature schema: #{metadata.feature_schema_version}")
        Mix.shell().info("Features: #{inspect(metadata.feature_names)}")
        Mix.shell().info("Training rows: #{metadata.train_examples}")
        Mix.shell().info("Evaluation rows: #{metadata.eval_examples}")
        Mix.shell().info("Training tensor: #{inspect(Nx.shape(train.x))}")
        Mix.shell().info("Evaluation tensor: #{inspect(Nx.shape(eval_batch.x))}")
        Mix.shell().info("No weights were updated or saved.")

      {:error, reason} ->
        Mix.raise("Cannot build reviewed training data: #{inspect(reason)}")
    end
  end
end
