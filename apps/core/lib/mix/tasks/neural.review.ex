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
    {opts, rest, invalid} = OptionParser.parse(argv, strict: [dir: :string])
    if rest != [] or invalid != [], do: Mix.raise("Usage: mix neural.review [--dir DIR]")

    Mix.Task.run("compile")
    dir = Keyword.get(opts, :dir, "tmp/training")

    case ReviewQueue.pending(dir) do
      {:ok, []} ->
        Mix.shell().info("No pending general episodic attachment candidates to review.")

      {:ok, candidates} ->
        Mix.shell().info("#{length(candidates)} retrieved memory candidates awaiting human review.")
        Mix.shell().info("Only choose y/n when you can judge relevance from the evidence.")
        review_each(dir, candidates, 1)

      {:error, reason} ->
        Mix.raise("Cannot read review queue: #{inspect(reason)}")
    end
  end

  defp review_each(_dir, [], _index), do: Mix.shell().info("Review queue complete.")

  defp review_each(dir, [candidate | rest], index) do
    Mix.shell().info("\nReview #{index}")
    Mix.shell().info("Turn: #{candidate.turn_id}")
    Mix.shell().info("User asked: #{String.slice(candidate.user_text, 0, 400)}")
    Mix.shell().info("Recalled memory: #{String.slice(candidate.memory_summary, 0, 500)}")
    Mix.shell().info("Retrieval score: #{candidate.score}")

    case IO.gets("Relevant? [y]es / [n]o / [s]kip / [q]uit: ") do
      input when is_binary(input) ->
        case String.downcase(String.trim(input)) do
          "y" -> record_then_continue(dir, candidate, true, rest, index)
          "n" -> record_then_continue(dir, candidate, false, rest, index)
          "s" -> review_each(dir, rest, index + 1)
          "q" -> Mix.shell().info("Review ended. Previous labels remain saved.")
          _ ->
            Mix.shell().info("Enter y, n, s or q.")
            review_each(dir, [candidate | rest], index)
        end

      _ ->
        Mix.shell().info("Input closed; leaving existing feedback unchanged.")
    end
  end

  defp record_then_continue(dir, candidate, relevant, rest, index) do
    case ReviewQueue.review(dir, candidate, relevant) do
      :ok ->
        Mix.shell().info("Human review saved.")
        review_each(dir, rest, index + 1)

      {:error, reason} ->
        Mix.raise("Review not saved: #{inspect(reason)}")
    end
  end
end
