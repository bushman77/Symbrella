defmodule Db.Lexicon do
  @moduledoc """
  DB helpers for Lexicon/Brain integration.
  """
  import Ecto.Query, warn: false
  alias Db
  alias Db.BrainCell

  @pos_inventory ~w(noun verb adj adv pron det adp num cconj sconj part propn punct intj sym x)

  @doc """
  Fetch all brain_cells whose norm is in the provided list.
  """
  @spec fetch_by_norms([String.t()]) :: [BrainCell.t()]
  def fetch_by_norms(norms) when is_list(norms) do
    norms = Enum.uniq(norms)
    Db.all(from(b in BrainCell, where: b.norm in ^norms))
  end

  @doc """
  Fetch all brain_cells by their exact IDs.
  """
  @spec fetch_by_ids([String.t()]) :: [BrainCell.t()]
  def fetch_by_ids(ids) when is_list(ids) do
    ids = Enum.uniq(ids)
    Db.all(from(b in BrainCell, where: b.id in ^ids))
  end

  # Optional, retained for feature-flag use
  def ensure_pos_variants_from_tokens(tokens, opts \\ []) do
    only_mw? = !!opts[:only_mw]

    pos_list =
      opts[:pos_inventory] ||
        Application.get_env(:symbrella, :pos_inventory) ||
        @pos_inventory

    gram_function = opts[:gram_function] || ""

    now_naive = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    selected =
      if only_mw?, do: Enum.filter(tokens, &match?(%{mw: true}, &1)), else: tokens

    rows =
      for %{phrase: phrase, mw: mw?} <- selected,
          pos <- pos_list do
        norm = normize(phrase)
        type = opts[:type] || if(mw?, do: "phrase", else: "word")
        id = [norm, pos || "", type, gram_function || ""] |> Enum.join("|")

        %{
          id: id,
          word: phrase,
          norm: norm,
          pos: pos,
          type: type,
          gram_function: gram_function,
          status: "active",
          synonyms: [],
          antonyms: [],
          semantic_atoms: [],
          activation: 0.0,
          modulated_activation: 0.0,
          dopamine: 0.0,
          serotonin: 0.0,
          connections: [],
          inserted_at: now_naive,
          updated_at: now_naive
        }
      end
      |> Enum.uniq_by(& &1.id)

    _ =
      Db.insert_all(BrainCell, rows,
        on_conflict: :nothing,
        conflict_target:
          {:unsafe_fragment,
           " (norm, COALESCE(pos,''), COALESCE(type,''), COALESCE(gram_function,'')) "}
      )

    ids = Enum.map(rows, & &1.id)

    _ =
      from(b in BrainCell, where: b.id in ^ids)
      |> Db.update_all(set: [status: "active", updated_at: now_naive])

    Db.all(from(b in BrainCell, where: b.id in ^ids))
  end

  defp normize(p),
    do: p |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

@spec bulk_upsert_senses(list()) :: :ok
def bulk_upsert_senses([]), do: :ok
def bulk_upsert_senses(rows) when is_list(rows) do
  fields = BrainCell.__schema__(:fields) |> MapSet.new()
  now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

  entries =
    rows
    |> Enum.map(&sense_to_entry(&1, fields, now))
    |> Enum.reject(&is_nil/1)

  if entries != [] do
    conflict_target =
      if MapSet.member?(fields, :id) do
        :id
      else
        # Fallback to composite uniqueness used elsewhere in this module
        {:unsafe_fragment,
         " (norm, COALESCE(pos,''), COALESCE(type,''), COALESCE(gram_function,'')) "}
      end

    Db.insert_all(BrainCell, entries,
      on_conflict: :nothing,
      conflict_target: conflict_target
    )
  end

  :ok
end

# ---------- helpers ----------

defp sense_to_entry(row, fields, now) do
  word =
    (get(row, :word) || "")
    |> to_string()
    |> String.trim()

  norm = normize(word)
  if norm == "", do: nil, else: build_entry(row, fields, now, word, norm)
end

defp build_entry(row, fields, now, word, norm) do
  pos = get(row, :pos) || "unk"
  type = get(row, :type) || "lexicon"
  gram_function = get(row, :gram_function)

  # Prefer caller-provided ID; otherwise synthesize a stable composite if :id exists
  id =
    get(row, :id) ||
      (if MapSet.member?(fields, :id),
         do: [norm, pos || "", type || "", gram_function || ""] |> Enum.join("|"),
         else: nil)

  base = %{
    id: id,
    word: word,
    norm: norm,
    pos: pos,
    type: type,
    gram_function: gram_function,
    status: get(row, :status) || "active",
    definition: get(row, :definition) || get(row, :def),
    example: get(row, :example) || get(row, :ex),
    synonyms: get(row, :synonyms) || get(row, :syns) || [],
    antonyms: get(row, :antonyms) || get(row, :ants) || [],
    semantic_atoms: get(row, :semantic_atoms) || []
  }

  base
  |> maybe_put(:inserted_at, now, fields)
  |> maybe_put(:updated_at, now, fields)
  |> Map.take(MapSet.to_list(fields))
end

defp get(map, k), do: Map.get(map, k) || Map.get(map, to_string(k))

defp maybe_put(map, key, val, fields) do
  if MapSet.member?(fields, key), do: Map.put(map, key, val), else: map
end

end
