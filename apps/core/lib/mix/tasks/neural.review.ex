defmodule Mix.Tasks.Neural.Review do
  @moduledoc """
  Interactively review *existing* archived hippocampal attachment candidates:

      mix neural.review --dir tmp/training

  Accept y/n for verified relevance, s to skip, q to quit. Nothing is
  automatically labeled and the conversation text stays on the local terminal.
  """
  use Mix.Task

  alias Core.Neural.ReviewQueue

  @shortdoc "Review existing retrieved memories for supervised neural training"

  @impl Mix.Task
  def run(argv) do
    {opts, rest, invalid} = OptionParser.parse(argv, strict: [dir: :string, recheck: :boolean, turn: :string])
    if rest != [] or invalid != [], do: Mix.raise("Usage: mix neural.review [--dir DIR] [--recheck] [--turn TURN_ID]")

    Mix.Task.run("compile")
    dir = Keyword.get(opts, :dir, "tmp/training")

    recheck? = Keyword.get(opts, :recheck, false)
    queue = if recheck?, do: ReviewQueue.reviewed(dir), else: ReviewQueue.pending(dir)

    case queue do
      {:ok, []} ->
        Mix.shell().info("No #{if(recheck?, do: "reviewed", else: "pending")} episodic attachment candidates.")

      {:ok, candidates} ->
        turn_id = Keyword.get(opts, :turn)
        selected =
          if is_binary(turn_id) and turn_id != "",
            do: Enum.filter(candidates, &(&1.turn_id == turn_id)),
            else: candidates

        if selected == [] do
          Mix.shell().info("No matching memory candidates for the requested turn.")
        else
          Mix.shell().info("#{length(selected)} memory candidates for #{if(recheck?, do: "rechecking", else: "review")}.")
          Mix.shell().info("Only choose y/n when you can judge relevance from the evidence.")
          review_each(dir, selected, 1, recheck?)
        end

      {:error, reason} ->
        Mix.raise("Cannot read review queue: #{inspect(reason)}")
    end
  end

  defp review_each(_dir, [], _index, _recheck?), do: Mix.shell().info("Review queue complete.")

  defp review_each(dir, [candidate | rest], index, recheck?) do
    Mix.shell().info("\nReview #{index}")
    Mix.shell().info("Turn: #{candidate.turn_id}")
    Mix.shell().info("User asked: #{String.slice(candidate.user_text, 0, 400)}")
    Mix.shell().info("Recalled memory: #{String.slice(candidate.memory_summary, 0, 500)}")
    Mix.shell().info("Retrieval score: #{candidate.score}")

    if recheck? do
      Mix.shell().info("Previous human label: #{if(candidate.previous_relevant, do: "relevant (y)", else: "irrelevant (n)")}")
    end

    case IO.gets("Relevant? [y]es / [n]o / [s]kip / [q]uit: ") do
      input when is_binary(input) ->
        case String.downcase(String.trim(input)) do
          "y" -> record_then_continue(dir, candidate, true, rest, index, recheck?)
          "n" -> record_then_continue(dir, candidate, false, rest, index, recheck?)
          "s" -> review_each(dir, rest, index + 1, recheck?)
          "q" -> Mix.shell().info("Review ended. Previous labels remain saved.")
          _ ->
            Mix.shell().info("Enter y, n, s or q.")
            review_each(dir, [candidate | rest], index, recheck?)
        end

      _ ->
        Mix.shell().info("Input closed; leaving existing feedback unchanged.")
    end
  end

  defp record_then_continue(dir, candidate, relevant, rest, index, recheck?) do
    case ReviewQueue.review(dir, candidate, relevant, correct: recheck?) do
      :ok ->
        Mix.shell().info(if(recheck?, do: "Human correction appended.", else: "Human review saved."))
        review_each(dir, rest, index + 1, recheck?)

      :unchanged ->
        Mix.shell().info("Label unchanged; no duplicate feedback appended.")
        review_each(dir, rest, index + 1, recheck?)

      {:error, reason} ->
        Mix.raise("Review not saved: #{inspect(reason)}")
    end
  end
end
