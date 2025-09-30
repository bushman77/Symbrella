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
  @spec ensure_cells([norm()]) :: :ok
  def ensure_cells(norms) when is_list(norms) do
    # TODO: wire to a proper DB upsert stage (Db.Lexicon.bulk_upsert_senses/1 stub exists)
    _ = norms
    :ok
  end

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

