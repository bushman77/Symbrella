defmodule Db.Lexicon do
  @moduledoc """
  DB helpers for Lexicon/Brain integration.

  Return shapes:
    • fetch_by_norms/1  → [sense_map, ...]   (flat list; Core-safe)
    • fetch_by_ids/1    → [sense_map, ...]   (flat list)
    • fetch_by_norms_grouped/1 → %{norm => [sense_map, ...]}

  Sense maps expose fields commonly used by Core/LIFG:
    id, word, lemma, norm, pos, type, definition, example, synonyms, antonyms, features(%{lemma,pos})
  """

  import Ecto.Query, warn: false
  alias Db
  alias Db.BrainCell

  @pos_inventory ~w(noun verb adj adv pron det adp num cconj sconj part propn punct intj sym x)

  # ---------- Reads ----------

  @doc """
  Fetch lexicon senses whose norm is in `norms`.

  Returns a **flat list** of sense maps.
  """
  @spec fetch_by_norms([String.t()]) :: [map()]
  def fetch_by_norms(norms) when is_list(norms) do
    norms =
      norms
      |> Enum.map(&normize/1)
      |> Enum.uniq()

    Db.all(
      from(b in BrainCell,
        where: b.norm in ^norms and b.status == "active",
        select: b
      )
    )
    |> Enum.map(&map_for_core/1)
  end

  @doc """
  Same as fetch_by_norms/1 but grouped by norm.
  Returns: %{ "hello" => [%{...}], ... }
  """
  @spec fetch_by_norms_grouped([String.t()]) :: %{optional(String.t()) => [map()]}
  def fetch_by_norms_grouped(norms) when is_list(norms) do
    fetch_by_norms(norms) |> Enum.group_by(& &1.norm)
  end

  @doc """
  Fetch senses by exact IDs. Returns a **flat list** of sense maps.
  """
  @spec fetch_by_ids([String.t()]) :: [map()]
  def fetch_by_ids(ids) when is_list(ids) do
    ids = Enum.uniq(ids)

    Db.all(
      from(b in BrainCell,
        where: b.id in ^ids,
        select: b
      )
    )
    |> Enum.map(&map_for_core/1)
  end

  # ---------- Seeding / Upserts ----------

  @doc """
  Insert/Update lexicon senses as BrainCell rows.

  Expected rows like:
    %{id, word, pos, type: "lexicon", definition, example, synonyms, antonyms}
  """
  @spec bulk_upsert_senses(list()) :: :ok
  def bulk_upsert_senses([]), do: :ok

  def bulk_upsert_senses(rows) when is_list(rows) do
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    rows1 =
      Enum.map(rows, fn r ->
        word = get(r, :word, "")
        norm = normize(word)
        pos = get(r, :pos, "unk")
        id = get(r, :id) || build_lex_id(norm, pos, r)

        %{
          id: id,
          status: "active",
          type: get(r, :type, "lexicon"),
          norm: norm,
          pos: pos,
          word: word,
          definition: get(r, :definition),
          example: get(r, :example),
          synonyms: List.wrap(get(r, :synonyms, [])),
          antonyms: List.wrap(get(r, :antonyms, [])),
          semantic_atoms: List.wrap(get(r, :semantic_atoms, [])),
          inserted_at: now,
          updated_at: now
        }
      end)

    # Upsert on PK id; replace content fields on conflict
    _ =
      Db.insert_all(
        BrainCell,
        rows1,
        on_conflict:
          {:replace,
           [:word, :norm, :pos, :type, :definition, :example, :synonyms, :antonyms, :updated_at]},
        conflict_target: [:id]
      )

    :ok
  end

  def bulk_upsert_senses(_), do: :ok

  @doc """
  Seed POS variants for tokens. Pass `only_mw?: true` to restrict to MWEs.
  Returns **flat list** of the seeded rows as sense maps.
  """
  @spec ensure_pos_variants_from_tokens(list(), keyword()) :: [map()]
  def ensure_pos_variants_from_tokens(tokens, opts \\ []) do
    only_mw? = !!opts[:only_mw]

    pos_list =
      opts[:pos_inventory] ||
        Application.get_env(:symbrella, :pos_inventory) ||
        @pos_inventory

    gram_function = opts[:gram_function] || ""
    type_override = opts[:type]

    now_naive = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    selected =
      if only_mw?, do: Enum.filter(tokens, &match?(%{mw: true}, &1)), else: tokens

    rows =
      for %{phrase: phrase, mw: mw?} <- selected, pos <- pos_list do
        norm = normize(phrase)
        type = type_override || if(mw?, do: "phrase", else: "word")
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
    |> Enum.map(&to_plain_map/1)
  end

  # ---------- PMTG compatibility ----------

  @doc """
  Return up to `limit` senses for a lemma, shaped for PMTG evidence.
  Prefers rows with type = 'lexicon'.
  """
  @spec lookup(String.t(), non_neg_integer()) :: [map()]
  def lookup(lemma, limit) when is_binary(lemma) and is_integer(limit) do
    norm = normize(lemma)

    Db.all(
      from(b in BrainCell,
        where: b.norm == ^norm and b.status == "active",
        order_by: [
          asc: fragment("CASE WHEN ? = 'lexicon' THEN 0 ELSE 1 END", b.type),
          desc: b.updated_at
        ],
        limit: ^max(limit, 0),
        select: %{
          id: b.id,
          lemma: b.word,
          pos: b.pos,
          type: b.type,
          gloss: b.definition,
          example: b.example,
          synonyms: b.synonyms,
          antonyms: b.antonyms
        }
      )
    )
  end

  @doc "Alias kept for compatibility with other callers."
  @spec definitions_for(String.t(), non_neg_integer()) :: [map()]
  def definitions_for(lemma, limit), do: lookup(lemma, limit)

  # ---------- Helpers ----------

  # Normalize BrainCell to a Core-friendly shape.
  defp map_for_core(%BrainCell{} = b) do
    %{
      id: b.id,
      type: b.type,
      word: b.word,
      lemma: b.word,
      norm: b.norm,
      pos: b.pos,
      definition: b.definition,
      example: b.example,
      synonyms: b.synonyms || [],
      antonyms: b.antonyms || [],
      features: %{
        lemma: b.word,
        pos: b.pos
      }
    }
  end

  defp to_plain_map(%BrainCell{} = b), do: map_for_core(b)

  defp normize(p),
    do: p |> to_string() |> String.downcase() |> String.trim() |> String.replace(~r/\s+/, " ")

  defp get(m, k, alt \\ nil), do: Map.get(m, k, Map.get(m, to_string(k), alt))

  defp build_lex_id(norm, pos, r) do
    base = :erlang.phash2({norm, pos, get(r, :definition)})
    "#{norm}|#{pos}|lex|#{base}"
  end
end
