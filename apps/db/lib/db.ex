 defmodule Db do
   @moduledoc """
   Umbrella-wide Repo. One DB, one config source.

   LTM owns lexicon enrichment:
     • Collect unique norms from si.tokens (word-true).
     • Load known BrainCell rows.
    • Report missing norms (Core will decide whether to create/ensure).
    • Return loaded rows; Core will merge active_cells and notify Brain.
   """
   use Ecto.Repo,
     otp_app: :db,
     adapter: Ecto.Adapters.Postgres,
     priv: "priv/repo"

   import Ecto.Query, only: [from: 2]

   alias Db.BrainCell
  # No upward aliases here — keep Db decoupled

@spec ltm(map(), keyword()) ::
        {:ok, %{rows: [%Db.BrainCell{}], missing_norms: [binary()], db_hits: MapSet.t(binary())}}
def ltm(%{tokens: toks} = _si, _opts \\ []) do
  norms =
    toks
    |> Enum.map(&norm(&1.phrase))
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()

  if norms == [] do
    {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}
  else
    existing_rows =
      from(b in BrainCell, where: b.norm in ^norms)
      |> Db.all()

    existing_norms = MapSet.new(for r <- existing_rows, do: r.norm)
    missing_norms = Enum.reject(norms, &MapSet.member?(existing_norms, &1))

    rows = existing_rows
    db_hits = MapSet.new(for r <- rows, do: r.norm)

    {:ok, %{rows: rows, missing_norms: missing_norms, db_hits: db_hits}}
  end
end

# keep this helper since ltm/2 uses it
defp norm(nil), do: ""
defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)


end

