defmodule Mix.Tasks.Db.BrainCellCorpus.Audit do
  @moduledoc """
  Audits BrainCell rows for Axon sense-embedding training readiness.

      mix db.brain_cell_corpus.audit
      mix db.brain_cell_corpus.audit --sample 12
      mix db.brain_cell_corpus.audit --export priv/brain_cell_sense_cards.jsonl --limit 5000
  """

  use Mix.Task

  @shortdoc "Audit/export BrainCell sense-card training corpus"

  @impl Mix.Task
  def run(args) do
    {opts, _argv, invalid} =
      OptionParser.parse(args,
        strict: [
          sample: :integer,
          export: :string,
          limit: :integer,
          include_inactive: :boolean
        ]
      )

    if invalid != [] do
      Mix.raise("Invalid options: #{inspect(invalid)}")
    end

    :ok = start_repo()

    only_active? = not Keyword.get(opts, :include_inactive, false)
    sample_limit = Keyword.get(opts, :sample, 8)
    audit = Db.BrainCellCorpus.audit(sample_limit: sample_limit, only_active: only_active?)

    print_audit(audit)

    case Keyword.fetch(opts, :export) do
      {:ok, path} ->
        limit = Keyword.get(opts, :limit, 1_000)

        case Db.BrainCellCorpus.export_jsonl(path, limit: limit, only_active: only_active?) do
          {:ok, count} -> Mix.shell().info("\nExported #{count} sense cards to #{path}")
          {:error, reason} -> Mix.raise("Failed to export #{path}: #{inspect(reason)}")
        end

      :error ->
        :ok
    end
  end

  defp start_repo do
    {:ok, _apps} = Application.ensure_all_started(:ecto_sql)
    {:ok, _apps} = Application.ensure_all_started(:postgrex)

    case Db.start_link() do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _pid}} -> :ok
      {:error, reason} -> Mix.raise("Failed to start Db repo: #{inspect(reason)}")
    end
  end

  defp print_audit(%{} = audit) do
    Mix.shell().info("BrainCell corpus audit")
    Mix.shell().info("")
    Mix.shell().info("Totals")
    print_map(audit.totals)

    Mix.shell().info("")
    Mix.shell().info("Training readiness")
    print_map(audit.readiness)

    Mix.shell().info("")
    Mix.shell().info("Ambiguous active norms")
    print_map(Map.delete(audit.ambiguous_norms, :examples))
    print_rows("Top ambiguous examples", audit.ambiguous_norms.examples)

    print_rows("POS breakdown", audit.by_pos)
    print_rows("Sample sense cards", audit.samples)
  end

  defp print_map(map) when is_map(map) do
    map
    |> Enum.sort_by(fn {key, _value} -> to_string(key) end)
    |> Enum.each(fn {key, value} ->
      Mix.shell().info("  #{key}: #{format_value(value)}")
    end)
  end

  defp print_rows(_label, []), do: :ok

  defp print_rows(label, rows) when is_list(rows) do
    Mix.shell().info("")
    Mix.shell().info(label)

    Enum.each(rows, fn row ->
      Mix.shell().info("  - #{format_row(row)}")
    end)
  end

  defp format_row(%{id: id, norm: norm, pos: pos, definition: definition}) do
    "#{id} #{inspect(norm)}/#{pos} #{String.slice(definition || "", 0, 96)}"
  end

  defp format_row(%{} = row) do
    row
    |> Enum.sort_by(fn {key, _value} -> to_string(key) end)
    |> Enum.map(fn {key, value} -> "#{key}=#{format_value(value)}" end)
    |> Enum.join(", ")
  end

  defp format_value(value) when is_float(value), do: :erlang.float_to_binary(value, decimals: 4)
  defp format_value(value) when is_list(value), do: inspect(value)
  defp format_value(value), do: to_string(value)
end
