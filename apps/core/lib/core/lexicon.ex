defmodule Core.Lexicon do
  @moduledoc """
  Core ↔ Lexicon wiring.

  • `all/2` — run the lexicon stage (scrape/normalize/upsert) and then
    re-query storage by norms, merging rows into `SI.active_cells` so the
    same turn can use fresh definitions.
  • `lookup/1` — proxy to the external `Lexicon` client (guarded).
  • `bulk_upsert_senses/1` — delegate to DB (guarded).

  Design goals:
  - Never crash if the DB or external client is absent.
  - Accept flexible token shapes; only `:phrase` is required per token.
  """

  alias Core.SemanticInput, as: SI
  alias Core.Lexicon.Stage
  alias Db.Lexicon, as: DbLex
  alias Db

  @doc """
  Run the lexicon stage (if enabled), then re-query storage by norms and merge into SI.
  """
  @spec all(SI.t(), Keyword.t()) :: SI.t()
  def all(%SI{} = si, opts \\ []) do
    if Keyword.get(opts, :lexicon_stage?, true) do
      si1 = Stage.run(si)

      norms =
        si1
        |> Map.get(:tokens, [])
        |> tokens_to_norms()

      rows = if norms == [], do: [], else: safe_fetch_by_norms(norms)

      if rows == [] do
        si1
      else
        merged =
          (si1.active_cells || [])
          |> Kernel.++(rows)
          |> dedup_by_id()

        %{si1 | active_cells: merged}
      end
    else
      si
    end
  end

  @doc """
  Proxy to the external `Lexicon` app (used by LV fallback display). Guarded.
  """
  @spec lookup(String.t()) :: map()
  def lookup(word) when is_binary(word) do
    try do
      cond do
        Code.ensure_loaded?(Lexicon) and function_exported?(Lexicon, :lookup, 1) ->
          Lexicon.lookup(word)
        Code.ensure_loaded?(Lexicon) and function_exported?(Lexicon, :lookup, 2) ->
          Lexicon.lookup(word, 8)
        true ->
          %{word: word, senses: []}
      end
    rescue
      _ -> %{word: word, senses: []}
    catch
      _, _ -> %{word: word, senses: []}
    end
  end

  @doc """
  Ensure a seed BrainCell exists for each missing norm (single words only).
  Creates rows like "norm|unk|seed|" with type="seed" and pos="unk". No-ops on conflict.
  """
  @spec ensure_cells([String.t()]) :: :ok
  def ensure_cells(norms) when is_list(norms) do
    norms =
      norms
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&normalize/1)
      |> Enum.reject(&String.contains?(&1, " "))
      |> Enum.uniq()

    if norms == [] do
      :ok
    else
      now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

      rows =
        for norm <- norms do
          %{
            id: norm <> "|unk|seed|",
            status: "active",
            type: "seed",
            norm: norm,
            pos: "unk",
            word: norm,
            inserted_at: now,
            updated_at: now
          }
        end

      _ = Db.insert_all(Db.BrainCell, rows, on_conflict: :nothing)
      :ok
    end
  end

  @doc """
  DB bulk upsert for senses (called by Stage). Guarded.
  """
  @spec bulk_upsert_senses(list()) :: :ok
  def bulk_upsert_senses(rows) when is_list(rows) do
    try do
      if Code.ensure_loaded?(DbLex) and function_exported?(DbLex, :bulk_upsert_senses, 1) do
        DbLex.bulk_upsert_senses(rows)
      else
        :ok
      end
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  def bulk_upsert_senses(_), do: :ok

  # ─── helpers ──────────────────────────────────────────────────────────────

  defp safe_fetch_by_norms(norms) when is_list(norms) do
    try do
      if Code.ensure_loaded?(DbLex) and function_exported?(DbLex, :fetch_by_norms, 1) do
        DbLex.fetch_by_norms(norms)
      else
        []
      end
    rescue
      _ -> []
    catch
      _, _ -> []
    end
  end

  defp tokens_to_norms(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(fn t -> Map.get(t, :phrase) || Map.get(t, "phrase") || "" end)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&normalize/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp tokens_to_norms(_), do: []

  defp normalize(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end

  defp normalize(_), do: ""

  defp get_id(%Db.BrainCell{id: id}), do: id
  defp get_id(%{id: id}) when is_binary(id), do: id
  defp get_id(%{"id" => id}) when is_binary(id), do: id
  defp get_id(_), do: nil

  defp dedup_by_id(list) do
    {acc, _seen} =
      Enum.reduce(list, {[], MapSet.new()}, fn x, {acc, seen} ->
        case get_id(x) do
          nil -> {acc, seen}
          id ->
            if MapSet.member?(seen, id) do
              {acc, seen}
            else
              {[x | acc], MapSet.put(seen, id)}
            end
        end
      end)

    Enum.reverse(acc)
  end
end

