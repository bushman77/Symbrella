defmodule Db.JSONL do
  @moduledoc false

  @type row :: map()

  @type fetch_opt ::
          {:path, String.t()}
          | {:field, String.t() | atom()}
          | {:limit, non_neg_integer() | :infinity}
          | {:decode?, boolean()}
          | {:on_decode_error, :skip | :raise | :count}

  @type populate_opt ::
          fetch_opt()
          | {:type, String.t() | nil}
          | {:status, String.t() | nil}
          | {:pos_fallback, String.t()}
          | {:include_semantic_atoms?, boolean()}
          | {:fill_missing_relations?, boolean()}

  @type upsert_opt ::
          populate_opt()
          | {:repo, module()}
          | {:batch_size, pos_integer()}
          | {:validate?, boolean()}
          | {:on_db_error, :raise | :halt | :count}
          | {:returning?, boolean()}

  @type import_opt ::
          upsert_opt()
          | {:every, non_neg_integer()}
          | {:assume_sorted?, boolean()}
          | {:limit_words, non_neg_integer() | :infinity}

  alias Db.JSONL.{IO, Fetch, Populate, Upsert, Import}

  # ───────────────────────── paths / head ─────────────────────────

  @spec default_paths() :: [String.t()]
  def default_paths, do: IO.default_paths()

  @spec default_path() :: String.t()
  def default_path, do: IO.default_path()

  @spec head(non_neg_integer(), keyword()) :: {:ok, [binary()]} | {:error, any()}
  def head(n \\ 3, opts \\ []), do: IO.head(n, opts)

  # ───────────────────────── fetch entries / senses ─────────────────────────

  @spec fetch_word_stream(String.t(), [fetch_opt()]) :: {:ok, Enumerable.t()} | {:error, any()}
  def fetch_word_stream(word, opts \\ []), do: Fetch.fetch_word_stream(word, opts)

  @spec fetch_word(String.t(), [fetch_opt()]) :: {:ok, list(row() | binary())} | {:error, any()}
  def fetch_word(word, opts \\ []), do: Fetch.fetch_word(word, opts)

  @spec fetch_senses(String.t(), keyword()) :: {:ok, list(map())} | {:error, any()}
  def fetch_senses(word, opts \\ []), do: Fetch.fetch_senses(word, opts)

  # ───────────────────────── senses → structs ─────────────────────────

  @spec populate_structs(String.t(), [populate_opt()]) ::
          {:ok, list(Db.BrainCell.t())} | {:error, any()}
  def populate_structs(word, opts \\ []), do: Populate.populate_structs(word, opts)

  @spec populate_structs_from_senses([map()], [populate_opt()]) :: list(Db.BrainCell.t())
  def populate_structs_from_senses(senses, opts \\ []),
    do: Populate.populate_structs_from_senses(senses, opts)

  # ───────────────────────── upsert into brain_cells ─────────────────────────

  @spec upsert_word(String.t(), [upsert_opt()]) :: {:ok, map()} | {:error, any()}
  def upsert_word(word, opts \\ []), do: Upsert.upsert_word(word, opts)

  @spec upsert_cells(list(), [upsert_opt()]) :: {:ok, map()} | {:error, any()}
  def upsert_cells(cells, opts \\ []), do: Upsert.upsert_cells(cells, opts)

  # ───────────────────────── import all ─────────────────────────

  @spec import_all([import_opt()]) :: {:ok, map()} | {:error, any()}
  def import_all(opts \\ []), do: Import.import_all(opts)

  # ───────────────────────── summarize helpers ─────────────────────────

  @spec summarize_entries([row()]) :: map()
  def summarize_entries(entries), do: Fetch.summarize_entries(entries)

  @spec summarize_senses([map()]) :: map()
  def summarize_senses(senses), do: Fetch.summarize_senses(senses)
end
