defmodule Db do
  @moduledoc """
  Umbrella‑wide Repo (single database).

  Long‑term memory (LTM) helpers:
  • Collect normalized `norm`s from `si.tokens`.
  • Load matching `Db.BrainCell` rows.
  • Report which norms are missing so callers can decide whether to create.
  • Return loaded rows along with a set of DB hits.
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/repo"

  import Ecto.Query, only: [from: 2]
  alias Db.BrainCell

  @type norm :: String.t()

  @doc """
  Look up BrainCell rows for the tokens inside an SI map.

  `si.tokens` may contain:
  • maps with `:phrase` (preferred) or `:norm`
  • plain strings

  Returns `{:ok, %{rows: rows, missing_norms: missing, db_hits: hits}}`.
  """
# move defaults to a head
@spec ltm(map(), keyword()) ::
        {:ok, %{rows: [Db.BrainCell.t()], missing_norms: [binary()], db_hits: MapSet.t(binary())}}
def ltm(si, opts \\ [])

# primary clause
def ltm(%{tokens: tokens} = _si, _opts) when is_list(tokens) do
  norms =
    tokens
    |> Enum.map(&token_to_phrase/1)
    |> Enum.map(&norm/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()

  if norms == [] do
    {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}
  else
    rows =
      from(b in Db.BrainCell, where: b.norm in ^norms)
      |> Db.all()

    existing = MapSet.new(for r <- rows, do: r.norm)
    missing  = Enum.reject(norms, &MapSet.member?(existing, &1))
    hits     = MapSet.new(for r <- rows, do: r.norm)
    {:ok, %{rows: rows, missing_norms: missing, db_hits: hits}}
  end
end

# fallback
def ltm(_si, _opts), do: {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}

  @doc """
  Returns `true` if a *word* exists by `norm` in `brain_cells`.
  Guards invalid/blank inputs without querying for blanks.
  """
  @spec word_exists?(term()) :: boolean()
  def word_exists?(term) when is_binary(term) do
    case norm(term) do
      "" -> false
      n -> from(b in BrainCell, where: b.norm == ^n) |> Db.exists?()
    end
  end

  def word_exists?(_), do: false

  # -- helpers ----------------------------------------------------------------

  # Accept token map or string, extract phrase text
  defp token_to_phrase(%{phrase: p}) when is_binary(p), do: p
  defp token_to_phrase(%{"phrase" => p}) when is_binary(p), do: p
  defp token_to_phrase(%{norm: n}) when is_binary(n), do: n
  defp token_to_phrase(%{"norm" => n}) when is_binary(n), do: n
  defp token_to_phrase(s) when is_binary(s), do: s
  defp token_to_phrase(_), do: ""

  defp norm(nil), do: ""
  defp norm(s) when is_binary(s), do: s |> String.trim() |> String.downcase(:default)
  defp norm(_), do: ""
end
 
