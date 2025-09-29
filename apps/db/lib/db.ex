defmodule Db do
  @moduledoc """
  Umbrella-wide Repo. One DB, one config source.

  LTM owns lexicon enrichment:
    • Collect unique norms from si.tokens (word-true).
    • Load known BrainCell rows.
    • For missing norms, call Core.Lexicon.ensure_cells/1 (if enabled).
    • Re-query for newly created rows.
    • Merge with si.active_cells and cast activation to Brain.
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/repo"

  import Ecto.Query, only: [from: 2]

  alias Db.BrainCell
  alias Core.Lexicon
  alias Brain

  @payload %{delta: 1.0, decay: 0.98}

  @spec ltm(map(), keyword()) :: map()
  def ltm(%{tokens: toks} = si, opts \\ []) do
    norms =
      toks
      |> Enum.map(&norm(&1.phrase))
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    existing_rows =
      case norms do
        [] -> []
        _ -> from(b in BrainCell, where: b.norm in ^norms) |> Db.all()
      end

    existing_norms = MapSet.new(for r <- existing_rows, do: r.norm)
    missing_norms = Enum.reject(norms, &MapSet.member?(existing_norms, &1))

    # Enrich via Lexicon only for missing norms (owned by LTM)
    if missing_norms != [] and Keyword.get(opts, :enrich_lexicon?, true) do
      _ = Lexicon.ensure_cells(missing_norms)
    end

    new_rows =
      case missing_norms do
        [] -> []
        _ -> from(b in BrainCell, where: b.norm in ^missing_norms) |> Db.all()
      end

    rows = existing_rows ++ new_rows

    if rows != [] do
      payload = Map.put(@payload, :via, :ltm)

      if Process.whereis(Brain) do
        Brain.activate_cells(rows, payload)
      end
    end

    active_cells =
      si
      |> Map.get(:active_cells, [])
      |> Enum.flat_map(&sanitize_cell/1)
      |> Kernel.++(rows)
      |> Enum.reject(&(cell_id(&1) == nil))
      |> Enum.uniq_by(&cell_id/1)

    db_hits = MapSet.new(for r <- rows, do: r.norm)

    activation_summary =
      si
      |> Map.get(:activation_summary, %{})
      |> Map.update(:db_hits, db_hits, fn acc -> MapSet.union(acc, db_hits) end)

    si
    |> Map.put(:active_cells, active_cells)
    |> Map.put(:activation_summary, activation_summary)
  end

  @doc """
  Return `true` if a *word* exists; guards invalid inputs without hitting the DB.
  """
  @spec word_exists?(term) :: boolean()
  def word_exists?(term)

  def word_exists?(term) when is_binary(term) do
    word = String.trim(term)
    if word == "", do: false, else: exists?(word)
  end

  def word_exists?(_), do: false

  # ----------------- helpers -----------------

  defp sanitize_cell(%BrainCell{} = s), do: [s]
  defp sanitize_cell(%{id: _} = m), do: [m]
  defp sanitize_cell(%{"id" => _} = m), do: [m]
  defp sanitize_cell(id) when is_binary(id), do: [%{id: id}]
  defp sanitize_cell(_), do: []

  defp cell_id(%BrainCell{id: id}), do: id
  defp cell_id(%{id: id}), do: id
  defp cell_id(%{"id" => id}), do: id
  defp cell_id(id) when is_binary(id), do: id
  defp cell_id(_), do: nil

  defp norm(nil), do: ""
  defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)
end
