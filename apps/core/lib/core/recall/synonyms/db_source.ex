# apps/core/lib/core/recall/synonyms/db_source.ex
defmodule Core.Recall.Synonyms.DbSource do
  @moduledoc """
  Db-backed synonym source.

  Tries `Db.Lexicon.fetch_by_norms_grouped/1` first; if unavailable,
  falls back to `Db.Lexicon.fetch_by_norms/1` and groups locally.

  All calls are **safe** (wrapped in try/catch) to avoid crashing recall.
  """

  @behaviour Core.Recall.Synonyms.Source

  @impl true
  def fetch_grouped(norms) when is_list(norms) do
    keys =
      norms
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&normalize/1)
      |> Enum.reject(&(&1 in [nil, ""]))
      |> Enum.uniq()

    case keys do
      [] ->
        %{}

      _ ->
        grouped_via_db(keys)
    end
  end

  # ---------- internals ----------

  defp grouped_via_db(keys) do
    cond do
      Code.ensure_loaded?(Db.Lexicon) and function_exported?(Db.Lexicon, :fetch_by_norms_grouped, 1) ->
        safe(fn -> Db.Lexicon.fetch_by_norms_grouped(keys) end, %{})

      Code.ensure_loaded?(Db.Lexicon) and function_exported?(Db.Lexicon, :fetch_by_norms, 1) ->
        rows = safe(fn -> Db.Lexicon.fetch_by_norms(keys) end, [])
        group_locally(keys, rows)

      true ->
        %{}
    end
  end

  defp group_locally(keys, rows) when is_list(rows) do
    grouped =
      rows
      |> Enum.group_by(fn r -> row_norm(r) end)

    # Ensure only requested keys are present; missing keys map to []
    Enum.reduce(keys, %{}, fn k, acc ->
      Map.put(acc, k, Map.get(grouped, k, []))
    end)
  end

  defp group_locally(keys, _), do: Enum.into(keys, %{}, &{&1, []})

  defp row_norm(%{} = r) do
    (Map.get(r, :norm) || Map.get(r, "norm") ||
       Map.get(r, :word) || Map.get(r, "word") || "")
    |> normalize()
  end

  defp row_norm(_), do: ""

  defp normalize(nil), do: nil
  defp normalize(b) when is_binary(b), do: b |> String.downcase() |> String.trim()
  defp normalize(v), do: v |> to_string() |> String.downcase() |> String.trim()

  defp safe(fun, fallback) when is_function(fun, 0) do
    try do
      fun.()
    rescue
      _ -> fallback
    catch
      _, _ -> fallback
    end
  end
end

