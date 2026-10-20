defmodule Mix.Tasks.Neural.Train do
  @moduledoc """
  Two **separate** experimental, offline Axon learning objectives:

    mix neural.train --task memory_relevance --smoke
    mix neural.train --task state_transition --smoke
    mix neural.train --task state_transition
    mix neural.train --task state_transition_residual

  Memory relevance requires independent human ratings. State transition
  learns from the next archived cognitive state automatically and needs no
  human labels. Neither model is installed in the running assistant.
  """
  use Mix.Task

  alias Brain.Neural.{TrainingBuilder, MemoryRelevancePilot}
  alias Brain.Neural.{StateTransitionBuilder, StateTransitionPilot, StateTransitionResidual}

  @shortdoc "Train isolated Axon pilots on human feedback or observed state transitions"

  @impl Mix.Task
  def run(argv) do
    {opts, positional, invalid} =
      OptionParser.parse(argv,
        strict: [
          task: :string,
          dir: :string,
          epochs: :integer,
          hidden_units: :integer,
          smoke: :boolean
        ]
      )

    task = Keyword.get(opts, :task)

    if invalid != [] or positional != [] or
         task not in ["memory_relevance", "state_transition", "state_transition_residual"] do
      Mix.raise(
        "Usage: mix neural.train --task memory_relevance|state_transition|state_transition_residual [--dir DIR] [--epochs N] [--smoke]"
      )
    end

    Mix.Task.run("compile")
    dir = Keyword.get(opts, :dir, "priv")
    smoke? = Keyword.get(opts, :smoke, false)
    settings = Keyword.take(opts, [:epochs, :hidden_units])

    case task do
      "memory_relevance" -> train_relevance(dir, smoke?, settings)
      "state_transition" -> train_transition(dir, smoke?, settings)
      "state_transition_residual" -> train_residual(dir, smoke?, settings)
    end
  end

  defp train_relevance(dir, smoke?, settings) do
    dataset =
      if smoke?,
        do: TrainingBuilder.build_smoke(dir, :memory_relevance),
        else: TrainingBuilder.build(dir, :memory_relevance)

    with {:ok, data} <- dataset,
         {:ok, result} <- MemoryRelevancePilot.train(data, settings) do
      Mix.shell().info("Experimental memory-relevance Axon pilot")
      Mix.shell().info("Train: #{result.train_examples} reviewed candidates")
      Mix.shell().info("Training sessions: #{length(result.train_sessions)}")
      Mix.shell().info("Training MSE: #{Float.round(result.train_mse, 5)}")
      Mix.shell().info("Training labels: #{result.labels.train_positive} relevant / #{result.train_examples - result.labels.train_positive} irrelevant")
      Mix.shell().info("Train-label prevalence baseline MSE: #{Float.round(result.class_prior_train_mse, 5)}")

      if smoke? do
        Mix.shell().info("SMOKE TEST: same observations in training and evaluation.")
        Mix.shell().info("In-sample MSE: #{Float.round(result.eval_mse, 5)}")
        Mix.shell().info("In-sample recall-score baseline: #{Float.round(result.baseline_eval_mse, 5)}")
        Mix.shell().info("In-sample class-prior baseline: #{Float.round(result.class_prior_eval_mse, 5)}")
      else
        Mix.shell().info("Session-held-out examples: #{result.eval_examples}")
        Mix.shell().info("Holdout MSE: #{Float.round(result.eval_mse, 5)}")
        Mix.shell().info("Recall-score baseline holdout MSE: #{Float.round(result.baseline_eval_mse, 5)}")
        Mix.shell().info("Class-prior baseline holdout MSE: #{Float.round(result.class_prior_eval_mse, 5)}")
      end

      if result.train_examples < 20 or result.eval_examples < 20 do
        Mix.shell().info("WARNING: insufficient samples for reliable quality estimates.")
      end

      if result.labels.train_positive in [0, result.train_examples] do
        Mix.shell().info("WARNING: one-class labels; collect relevant and irrelevant examples.")
      end

      filename = if smoke?, do: "memory_relevance_smoke.etf", else: "memory_relevance_pilot.etf"
      save_relevance(result, Path.join(dir, filename))
    else
      {:error, reason} -> Mix.raise("Cannot train reviewed memory relevance: #{inspect(reason)}")
    end
  end

  defp save_relevance(artifact, path) do
    case MemoryRelevancePilot.save(artifact, path) do
      :ok -> complete(path)
      {:error, reason} -> Mix.raise("Cannot save experimental weights: #{inspect(reason)}")
    end
  end

  defp train_transition(dir, smoke?, settings) do
    with {:ok, data} <- StateTransitionBuilder.build(dir, smoke: smoke?),
         {:ok, result} <- StateTransitionPilot.train(data, settings) do
      print_modulator_coverage(data)
      Mix.shell().info("Experimental self-supervised cognitive-state Axon pilot (schema v4; complete runtime snapshots and observed-only targets)")
      Mix.shell().info("Training transitions: #{result.train_examples}")
      Mix.shell().info("Training sessions: #{length(result.train_sessions)}")
      Mix.shell().info("Valid observed feature pairs: #{result.valid_train_cells} training / #{result.valid_eval_cells} holdout")
      Mix.shell().info("Model training MSE: #{Float.round(result.train_mse, 5)}")
      Mix.shell().info("Persistence baseline training MSE: #{Float.round(result.persistence_train_mse, 5)}")

      if smoke? do
        Mix.shell().info("SMOKE TEST: NO independent evaluation.")
        Mix.shell().info("In-sample MSE: #{Float.round(result.eval_mse, 5)}")
      else
        Mix.shell().info("Holdout transitions: #{result.eval_examples}")
        Mix.shell().info("Holdout MSE: #{Float.round(result.eval_mse, 5)}")
        Mix.shell().info("Holdout persistence baseline MSE: #{Float.round(result.persistence_eval_mse, 5)}")
      end

      if result.train_examples < 20 or result.eval_examples < 20 do
        Mix.shell().info("WARNING: tiny transition sample, not evidence of useful learned cognition.")
      end

      filename = if smoke?, do: "state_transition_v4_smoke.etf", else: "state_transition_v4_pilot.etf"
      path = Path.join(dir, filename)

      case StateTransitionPilot.save(result, path) do
        :ok -> complete(path)
        {:error, reason} -> Mix.raise("Cannot save experimental weights: #{inspect(reason)}")
      end
    else
      {:error, reason} -> Mix.raise("Cannot train state transitions: #{inspect(reason)}")
    end
  end


  defp train_residual(dir, smoke?, settings) do
    with {:ok, data} <- StateTransitionBuilder.build(dir, smoke: smoke?),
         {:ok, result} <- StateTransitionResidual.train(data, settings) do
      print_modulator_coverage(data)
      Mix.shell().info("Experimental residual cognitive-state Axon pilot (schema v4; complete runtime snapshots and observed-only targets)")
      Mix.shell().info("Train transitions: #{result.train_examples}")
      Mix.shell().info("Training sessions: #{length(result.train_sessions)}")
      Mix.shell().info("Valid observed feature pairs: #{result.valid_train_cells} training / #{result.valid_eval_cells} holdout")
      Mix.shell().info("Model train MSE: #{Float.round(result.train_mse, 5)}")
      Mix.shell().info("Persistence train MSE: #{Float.round(result.persistence_train_mse, 5)}")

      if smoke? do
        Mix.shell().info("SMOKE TEST: evaluation uses training examples. NO independent assessment.")
        Mix.shell().info("In-sample model MSE: #{Float.round(result.eval_mse, 5)}")
      else
        Mix.shell().info("Held-out transitions: #{result.eval_examples}")
        Mix.shell().info("Held-out sessions: #{length(result.eval_sessions)}")
        Mix.shell().info("Model holdout MSE: #{Float.round(result.eval_mse, 5)}")
      end

      Mix.shell().info("Persistence #{if(smoke?, do: "in-sample", else: "holdout")} MSE: #{Float.round(result.persistence_eval_mse, 5)}")
      Mix.shell().info("Per-feature #{if(smoke?, do: "in-sample", else: "held-out")} errors (lower MSE is better):")
      Mix.shell().info("  feature | valid pairs | residual MSE | persistence MSE | improvement")

      Enum.each(result.eval_per_feature, fn row ->
        Mix.shell().info(
          "  #{row.feature}: " <>
            "#{row.valid_pairs} | " <>
            "#{format_metric(row.model_mse)} | " <>
            "#{format_metric(row.persistence_mse)} | " <>
            "#{format_metric(row.improvement)}"
        )
      end)

      if result.eval_mse < result.persistence_eval_mse do
        Mix.shell().info("Residual model beat persistence on this #{if(smoke?, do: "in-sample run", else: "held-out sample")}.")
      else
        Mix.shell().info("Residual model did NOT beat persistence on this #{if(smoke?, do: "in-sample run", else: "held-out sample")}.")
      end

      Mix.shell().info("Evidence assessment: #{result.assessment.status}")
      Mix.shell().info("Assessment reasons: #{inspect(result.assessment.reasons)}")
      Mix.shell().info("Relative improvement over persistence: #{inspect(result.assessment.relative_improvement)}")
      Mix.shell().info("Runtime deployment authorized: false (shadow research only)")

      if result.train_examples < 20 or result.eval_examples < 20 do
        Mix.shell().info("WARNING: tiny transition sample; results are not reliable evidence of generalization.")
      end

      filename =
        if smoke?,
          do: "state_transition_residual_v4_smoke.etf",
          else: "state_transition_residual_v4_pilot.etf"

      path = Path.join(dir, filename)

      case StateTransitionResidual.save(result, path) do
        :ok -> complete(path)
        {:error, reason} -> Mix.raise("Cannot save residual weights: #{inspect(reason)}")
      end
    else
      {:error, reason} -> Mix.raise("Cannot train residual state transition: #{inspect(reason)}")
    end
  end

  defp print_modulator_coverage(%{metadata: metadata}) do
    total = metadata.input_turns
    complete = metadata.complete_modulator_turns
    missing = metadata.missing_modulator_turns

    Mix.shell().info(
      "Raw neuromodulator coverage: #{complete}/#{total} archived turns complete; " <>
        "#{missing} incomplete/missing (per-level presence flags retained)."
    )
    Mix.shell().info("Complete process traces: #{metadata.complete_process_traces}/#{total}")
    Mix.shell().info("Memory fast-path turns: #{metadata.memory_fast_path_turns}/#{total}")
  end

  defp format_metric(:not_observed), do: "n/a"
  defp format_metric(value) when is_number(value), do: Float.round(value * 1.0, 6)

  defp complete(path) do
    Mix.shell().info("Saved offline experiment: #{path}")
    Mix.shell().info("Live cognitive decisions and hippocampal ranking remain unchanged.")
  end
end
