defmodule Core.Lexicon do
  @moduledoc """
  Core <-> Lexicon wiring.

  - `all/2`: run the lexicon stage (scrape/normalize/upsert) and then
    immediately re-query DB and merge those rows into SI.active_cells
    so the same turn can use the new definitions.
  - `lookup/1`: proxy to the `lexicon` app client (for UI fallbacks, etc).
  - `bulk_upsert_senses/1`: delegate to DB.
  """

  alias Core.SemanticInput, as: SI
  alias Core.Lexicon.Stage
  alias Db.Lexicon, as: DbLex
alias Db
alias Db.BrainCell

  @doc """
  Run the lexicon stage (if enabled), then re-query DB by norms and merge into SI.
  """
  @spec all(SI.t(), Keyword.t()) :: SI.t()
  def all(%SI{} = si, opts \\ []) do
    if Keyword.get(opts, :lexicon_stage?, true) do
      si1 = Stage.run(si)

      norms =
        si1.tokens
        |> Enum.map(&(&1[:phrase]))
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&String.downcase/1)
        |> Enum.uniq()

      rows = if norms == [], do: [], else: DbLex.fetch_by_norms(norms)

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
  Proxy to the external `lexicon` app (used by LV fallback display).
  """
  @spec lookup(String.t()) :: map()
  def lookup(word) when is_binary(word) do
    # The `lexicon` app defines a top-level `Lexicon` module.
    # If you renamed it, update this line accordingly.
    Lexicon.lookup(word)
  end

@doc """
Ensure a seed BrainCell exists for each missing norm.
Creates rows like "\#{norm}|unk|seed|" with type="seed" and pos="unk".
No-ops on conflict.
"""
@spec ensure_cells([String.t()]) :: :ok
def ensure_cells(norms) when is_list(norms) do
  norms =
    norms
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.downcase/1)
    |> Enum.uniq()

  if norms == [] do
    :ok
  else
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    rows =
      for norm <- norms do
        %{
          id: "#{norm}|unk|seed|",
          status: "active",
          type: "seed",
          norm: norm,
          pos: "unk",
          word: norm,
          inserted_at: now,
          updated_at: now
        }
      end

    _ = Db.insert_all(BrainCell, rows, on_conflict: :nothing)
    :ok
  end
end


  @doc """
  DB bulk upsert for senses (called by Stage).
  """
  @spec bulk_upsert_senses(list()) :: :ok
  def bulk_upsert_senses(rows) when is_list(rows), do: DbLex.bulk_upsert_senses(rows)
  def bulk_upsert_senses(_), do: :ok

  # ─── helpers ──────────────────────────────────────────────────────────────

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

