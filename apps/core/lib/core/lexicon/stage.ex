defmodule Core.Lexicon.Stage do
  @moduledoc """
  Pipeline stage:
  - checks Core.NegCache
  - uses Core.Lexicon.lookup/1
  - on 404: negcache + (optionally) drop token
  - on 200: upsert BrainCell + start Brain.Cell; keep token
  Returns updated %Core.SemanticInput{}.
  """

  alias Core.SemanticInput, as: SI
  alias Core.NegCache
  alias Core.Lexicon, as: Lx

  alias Db, as: Repo
  alias Db.BrainCell, as: BrainCellSchema
  alias Brain.Cell, as: BrainCellProc

  @type opts :: [drop_unknown: boolean]

  @spec run(SI.t(), opts) :: SI.t()
  def run(%SI{} = si, opts \\ []) do
    drop_unknown? = Keyword.get(opts, :drop_unknown, true)

    tokens = si.tokens || []

    # de-dupe phrases for this SI to avoid repeated calls
    phrases =
      tokens
      |> Enum.map(&token_phrase/1)
      |> Enum.reject(&(&1 in [nil, ""]))
      |> Enum.uniq()

    # lookups map: %{phrase => {:ok, entries} | {:error, :not_found} | {:error, reason}}
    results =
      Map.new(phrases, fn phrase ->
        res =
          cond do
            NegCache.exists?(phrase) ->
              {:error, :neg_cached}

            true ->
              case Lx.lookup(phrase) do
                {:ok, entries} -> {:ok, entries}
                {:error, :not_found} -> (NegCache.put(phrase); {:error, :not_found})
                {:error, reason} -> {:error, reason}
              end
          end

        {phrase, res}
      end)

    # Upsert cells for found phrases once; gather created/loaded cells
    cells =
      results
      |> Enum.flat_map(fn
        {phrase, {:ok, entries}} ->
          case ensure_cell(primary_attrs(entries)) do
            {:ok, cell} ->
              _ = maybe_start_cell(cell)
              [cell]
            _ -> []
          end

        _ -> []
      end)

    # Rebuild token list depending on drop policy
    kept_tokens =
      Enum.reduce(tokens, [], fn tok, acc ->
        phrase = token_phrase(tok)

        case Map.get(results, phrase) do
          {:ok, _} -> [tok | acc]  # keep on success
          {:error, :not_found} -> if drop_unknown?, do: acc, else: [mark_unknown(tok) | acc]
          {:error, :neg_cached} -> if drop_unknown?, do: acc, else: [mark_unknown(tok) | acc]
          {:error, _reason} -> [tok | acc] # transient/transport error → keep
          nil -> [tok | acc]               # phrase was nil/empty or not processed
        end
      end)
      |> Enum.reverse()

    si
    |> put_tokens(kept_tokens)
    |> put_cells(merge_cells(si, cells))
  end

  # ——— helpers ———

  defp token_phrase(%{phrase: p}) when is_binary(p), do: String.trim(p)
  defp token_phrase(p) when is_binary(p), do: String.trim(p)
  defp token_phrase(_), do: nil

  # annotate unknowns if you choose to keep them
  defp mark_unknown(tok) do
    meta = Map.get(tok, :meta, %{}) |> Map.put(:lex, :unknown)
    Map.put(tok, :meta, meta)
  end

  defp merge_cells(%SI{} = si, new_cells) do
    existing = Map.get(si, :cells, [])
    # avoid dupes by id or word if present
    (existing ++ new_cells)
    |> Enum.uniq_by(fn
      %{id: id} when not is_nil(id) -> {:id, id}
      %{word: w} -> {:word, w}
      other -> other
    end)
  end

  defp put_tokens(%SI{} = si, tokens) do
    if function_exported?(SI, :put_tokens, 2), do: SI.put_tokens(si, tokens), else: %{si | tokens: tokens}
  end

  defp put_cells(%SI{} = si, cells) do
    if Map.has_key?(si, :cells), do: %{si | cells: cells}, else: si
  end

  # Choose a primary definition from entries
  defp primary_attrs(entries) do
    entry   = List.first(entries) || %{}
    word    = entry["word"] || ""
    meaning = entry["meanings"] |> List.first() || %{}
    defn    = meaning["definitions"] |> List.first() || %{}

    %{
      word: word,
      pos: meaning["partOfSpeech"],
      definition: defn["definition"],
      example: defn["example"],
      synonyms: defn["synonyms"] || [],
      antonyms: defn["antonyms"] || []
    }
  end

  # DB upsert (simple + safe)
  defp ensure_cell(%{word: word} = attrs) when is_binary(word) and word != "" do
    case Repo.get_by(BrainCellSchema, word: word) do
      %BrainCellSchema{} = cell -> {:ok, cell}
      nil ->
        %BrainCellSchema{}
        |> BrainCellSchema.changeset(%{
          word: attrs.word,
          pos: Map.get(attrs, :pos),
          definition: Map.get(attrs, :definition),
          example: Map.get(attrs, :example),
          synonyms: Map.get(attrs, :synonyms, []),
          antonyms: Map.get(attrs, :antonyms, [])
        })
        |> Repo.insert()
    end
  end
  defp ensure_cell(_), do: {:error, :invalid}

  defp maybe_start_cell(%BrainCellSchema{} = schema) do
    case BrainCellProc.start_link(schema) do
      {:ok, _pid} -> :ok
      {:error, {:already_started, _}} -> :ok
      _ -> :ok
    end
  end
end

