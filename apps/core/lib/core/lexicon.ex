defmodule Core.Lexicon do
  @moduledoc """
  Public Lexicon API used by Core pipeline stages.
  """

  import Ecto.Query, only: [from: 2]

  alias Db
  alias Db.BrainCell
  alias Db.Lexicon, as: DbLex

  @compile {:no_warn_undefined, Db.Lexicon}

  @type word :: binary()
  @type norm :: binary()
  @type row :: %BrainCell{}

  @doc """
  Ensure cells exist for the given list of normalized words.
  (Delegates to DB layer; idempotent.)
  """
@spec ensure_cells([binary()]) :: :ok
def ensure_cells(norms) when is_list(norms) do
  alias Db.BrainCell
  alias Db

  fields = BrainCell.__schema__(:fields) |> MapSet.new()
  now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

  entries =
    norms
    |> Enum.map(&(&1 |> to_string() |> String.trim() |> String.downcase(:default)))
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.map(fn n ->
      pos  = "unk"
      type = "seed"
      gram = ""           # keep 4-part ID shape: norm|pos|type|gram
      id   = Enum.join([n, pos, type, gram], "|")

      %{}
      |> Map.put(:id, id)
      |> Map.put(:norm, n)
      |> maybe_put(:pos, pos, fields)
      |> maybe_put(:type, type, fields)
      |> maybe_put(:status, "active", fields)
      |> maybe_put(:word, n, fields)    # optional; added only if column exists
      |> maybe_put(:lemma, n, fields)   # optional; added only if column exists
      |> maybe_put(:inserted_at, now, fields)
      |> maybe_put(:updated_at, now, fields)
      |> Map.take(MapSet.to_list(fields))
    end)

  if entries != [] do
    Db.insert_all(BrainCell, entries, on_conflict: :nothing)
  end

  :ok
end

defp maybe_put(map, key, val, fields),
  do: if(MapSet.member?(fields, key), do: Map.put(map, key, val), else: map)

  @doc """
  Public `lookup/1` used by callers that expect it to exist.
  Delegates to `safe_lookup/1`.
  """
  @spec lookup(word()) :: any()
  def lookup(word), do: safe_lookup(word)

  @doc """
  Safe lookup that tolerates non-binary input and normalizes the word.
  Replace with your real implementation as needed.
  """
  @spec safe_lookup(term()) :: any()
  def safe_lookup(term) do
    word =
      case term do
        s when is_binary(s) -> String.trim(s)
        _ -> ""
      end

    if word == "" do
      nil
    else
      q =
        from(b in BrainCell,
          where: b.norm == ^String.downcase(word, :default),
          limit: 1
        )

      Db.one(q)
    end
  end

  @doc """
  No-op passthrough used in the Core pipeline; keeps shape for callers.
  """
  @spec all(map()) :: map()
  def all(si) when is_map(si), do: si

  # Re-export stage helper if you need it:
  @doc false
  def bulk_upsert_senses(rows) when is_list(rows), do: DbLex.bulk_upsert_senses(rows)
end

