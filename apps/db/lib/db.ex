defmodule Db do
  @moduledoc """
  Umbrella-wide Repo (single database).

  Long-term memory (LTM) helpers:
  • Collect normalized `norm`s from `si.tokens`.
  • Load matching `Db.BrainCell` rows (active-only by default).
  • Report which norms are missing so callers can decide whether to create.
  • Return loaded rows along with a set of DB hits.

  ## Options
    * `:only_active` (boolean, default: `true`) — filter rows with `status == "active"`.
    * `:limit_per_norm` (pos_integer | :all, default: `:all`) — cap rows returned per norm.
  """

  use Ecto.Repo,
    otp_app: :db,
    adapter: Ecto.Adapters.Postgres,
    priv: "priv/db"

  # Need full query macros (row_number/0, over/2, subquery/1, etc.)
  import Ecto.Query
  alias Db.BrainCell

  @type norm :: String.t()

  @doc """
  Look up BrainCell rows for the tokens inside an SI-like map.

  `si.tokens` may contain:
  • maps with `:phrase` (preferred) or `:norm`
  • plain strings

  Returns `{:ok, %{rows: rows, missing_norms: missing, db_hits: MapSet.t()}}`.

  See module docs for options.
  """
  @spec ltm(map(), keyword()) ::
          {:ok, %{rows: [Db.BrainCell.t()], missing_norms: [binary()], db_hits: MapSet.t()}}
  def ltm(si, opts \\ [])

  def ltm(%{tokens: tokens} = _si, opts) when is_list(tokens) do
    only_active?   = Keyword.get(opts, :only_active, true)
    limit_per_norm = Keyword.get(opts, :limit_per_norm, :all)

    norms =
      tokens
      |> Enum.map(&token_to_phrase/1)
      |> Enum.map(&norm/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    if norms == [] do
      {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}
    else
      where_dyn =
        if only_active? do
          dynamic([b], b.status == "active" and b.norm in ^norms)
        else
          dynamic([b], b.norm in ^norms)
        end

      base_q =
        from b in BrainCell,
          where: ^where_dyn

      # Optional per-norm limiting via window function (row_number over partition)
      q =
        case limit_per_norm do
          :all ->
            from b in base_q, select: b

          n when is_integer(n) and n > 0 ->
            # 1) Select a map with the struct and the row_number window
            q1 =
              from b in base_q,
                select: %{b: b, rn: over(row_number(), :norm_part)},
                windows: [norm_part: [partition_by: b.norm, order_by: [desc: b.updated_at]]]

            # 2) Filter by rn in the outer query and select the original struct
            from s in subquery(q1),
              where: s.rn <= ^n,
              select: s.b

          _ ->
            from b in base_q, select: b
        end

      rows = Db.all(q)

      existing = MapSet.new(for r <- rows, do: r.norm)
      missing  = Enum.reject(norms, &MapSet.member?(existing, &1))
      hits     = existing

      {:ok, %{rows: rows, missing_norms: missing, db_hits: hits}}
    end
  end

  def ltm(_si, _opts), do: {:ok, %{rows: [], missing_norms: [], db_hits: MapSet.new()}}

  @doc """
  Returns `true` if a *word* exists by `norm` in `brain_cells`.

  Guards invalid/blank inputs without querying.
  When `:only_active` is `true` (default), requires `status == "active"`.
  """
  @spec word_exists?(term(), keyword()) :: boolean()
  def word_exists?(term, opts \\ [])

  def word_exists?(term, opts) when is_binary(term) do
    only_active? = Keyword.get(opts, :only_active, true)

    case norm(term) do
      "" ->
        false

      n ->
        where_dyn =
          if only_active? do
            dynamic([b], b.status == "active" and b.norm == ^n)
          else
            dynamic([b], b.norm == ^n)
          end

        from(b in BrainCell, where: ^where_dyn, select: true)
        |> Db.exists?()
    end
  end

  def word_exists?(_, _opts), do: false

  # -- helpers ----------------------------------------------------------------

  # Accept token map or string, extract phrase text
  defp token_to_phrase(%{phrase: p}) when is_binary(p), do: p
  defp token_to_phrase(%{"phrase" => p}) when is_binary(p), do: p
  defp token_to_phrase(%{norm: n}) when is_binary(n), do: n
  defp token_to_phrase(%{"norm" => n}) when is_binary(n), do: n
  defp token_to_phrase(s) when is_binary(s), do: s
  defp token_to_phrase(_), do: ""

  # P-213: Unicode-punctuation-safe normalization for lookups
  defp norm(nil), do: ""

  defp norm(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    # strip leading/trailing punctuation (Unicode-aware); keep inner apostrophes/hyphens
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(_), do: ""
end

