defmodule Mix.Tasks.Neural.Catalog do
  @moduledoc """
  Categorize all archived Symbrella cognitive observations by their actual
  structured metadata. This is NOT supervised ground-truth generation.

      mix neural.catalog
      mix neural.catalog --dir tmp/training

  Produces a private, reproducible catalog.jsonl index with source row
  pointers, observed state categories and provenance, without raw conversations.
  The original training archive is not modified.
  """

  use Mix.Task

  alias Core.Neural.Catalog

  @shortdoc "Categorize all archived cognitive and memory observations"

  @impl Mix.Task
  def run(argv) do
    {opts, arguments, invalid} =
      OptionParser.parse(argv, strict: [dir: :string, output: :string])

    if invalid != [] or arguments != [] do
      Mix.raise("Usage: mix neural.catalog [--dir DIR] [--output PATH]")
    end

    Mix.Task.run("compile")
    dir = Keyword.get(opts, :dir, "tmp/training")
    output = Keyword.get(opts, :output, Path.join(dir, "catalog.jsonl"))

    case Catalog.write(dir, output) do
      {:ok, %{summary: summary, output: path}} ->
        Mix.shell().info("Neural archive catalog (observations only)")
        Mix.shell().info("Indexed #{summary.total_records} records from #{dir}")
        Mix.shell().info("Source files:")
        Enum.each(Enum.sort(summary.source_files), fn {name, count} ->
          Mix.shell().info("  #{name}: #{count}")
        end)

        Mix.shell().info("Automatic categories:")
        Enum.each(Enum.sort(summary.categories), fn {category, count} ->
          Mix.shell().info("  #{category}: #{count}")
        end)

        Mix.shell().info("Cognitive signal coverage:")
        Enum.each(Enum.sort(summary.signal_groups), fn {group, count} ->
          Mix.shell().info("  #{group}: #{count}")
        end)

        Mix.shell().info("Catalog: #{path}")
        Mix.shell().info("Categories are observed metadata, not correctness labels.")
        Mix.shell().info("The Mix task did not train or change model weights.")

      {:error, reason} ->
        Mix.raise("Cannot categorize archive: #{inspect(reason)}")
    end
  end
end
