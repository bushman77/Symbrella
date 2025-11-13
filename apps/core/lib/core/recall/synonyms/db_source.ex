defmodule Core.Recall.Synonyms.DbSource do
  @moduledoc """
  Db-backed synonym source.

  Tries `Db.Lexicon.fetch_by_norms_grouped/1` first; if unavailable,
  falls back to `Db.Lexicon.fetch_by_norms/1` and groups locally.

  All calls are **safe** (wrapped in try/catch) to avoid crashing recall.
  Uses dynamic `apply/3` so there is **no compile-time coupling** to `Db.*`.
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
      [] -> %{}
      _ -> grouped_via_db(keys)
    end
  end

  # ---------- internals ----------

  defp grouped_via_db(keys) do
    mod = Module.concat(Db, Lexicon)

    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, :fetch_by_norms_grouped, 1) ->
        safe(
          fn ->
            mod
            |> apply(:fetch_by_norms_grouped, [keys])
            |> normalize_grouped(keys)
          end,
          %{}
        )

      Code.ensure_loaded?(mod) and function_exported?(mod, :fetch_by_norms, 1) ->
        rows = safe(fn -> apply(mod, :fetch_by_norms, [keys]) end, [])
        group_locally(keys, rows)

      true ->
        %{}
    end
  end

  # Accept any grouped shape and ensure we only keep requested keys.
  # Unknown/missing keys map to [].
  defp normalize_grouped(%{} = grouped, keys) do
    Enum.reduce(keys, %{}, fn k, acc ->
      v =
        case Map.get(grouped, k) do
          nil -> []
          list when is_list(list) -> list
          other -> List.wrap(other)
        end

      Map.put(acc, k, v)
    end)
  end

  defp normalize_grouped(_, keys), do: Enum.into(keys, %{}, &{&1, []})

  defp group_locally(keys, rows) when is_list(rows) do
    grouped = Enum.group_by(rows, &row_norm/1)

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
