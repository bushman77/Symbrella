defmodule Core.Lexicon.Stage do
  @moduledoc false

  alias Core.SemanticInput, as: SI
  alias Core.NegCache
  alias Core.Lexicon, as: Lx

  alias Db
  alias Db.BrainCell
  alias Brain.Cell, as: BrainCellProc  # prefer BrainCellProc.start/1

  @spec run(SI.t()) :: SI.t()
  def run(%SI{tokens: tokens} = si) when is_list(tokens) do
    t0 = now_ms()

    phrases =
      tokens
      |> Enum.map(&phrase_of/1)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()

    # key by normalized phrase so NegCache/DB/API all align
    results =
      Map.new(phrases, fn raw ->
        key = norm_phrase(raw)

        res =
          cond do
            NegCache.exists?(key) ->
              {:error, :neg_cached}

            true ->
              case Lx.lookup(key) do
                {:ok, entries} ->
                  entries
                  |> enumerate_senses()
                  |> Enum.each(&upsert_and_maybe_start/1)

                  {:ok, :inserted}

                {:error, :not_found} ->
                  NegCache.put(key)
                  {:error, :not_found}

                {:error, reason} ->
                  {:error, reason}
              end
          end

        {key, res}
      end)

    {kept, dropped} =
      Enum.reduce(tokens, {[], 0}, fn tok, {acc, dropN} ->
        k = tok |> phrase_of() |> norm_phrase()
        case Map.get(results, k) do
          {:error, :not_found}   -> {acc, dropN + 1}
          {:error, :neg_cached}  -> {acc, dropN + 1}
          _                      -> {[tok | acc], dropN}
        end
      end)

    kept_rev = Enum.reverse(kept)

    ev = %{
      stage: :lexicon_stage,
      ts_ms: now_ms(),
      meta: %{
        looked_up: length(phrases),
        dropped_tokens: dropped,
        kept_tokens: length(kept_rev),
        latency_ms: max(now_ms() - t0, 0)
      }
    }

    %SI{si | tokens: kept_rev, trace: si.trace ++ [ev]}
  end

  def run(%SI{} = si), do: si

  # ——— helpers ———

  defp phrase_of(%{phrase: p}) when is_binary(p), do: String.trim(p)
  defp phrase_of(p) when is_binary(p), do: String.trim(p)
  defp phrase_of(_), do: nil

  defp norm_phrase(nil), do: nil
  defp norm_phrase(<<>>), do: <<>>
  defp norm_phrase(s) when is_binary(s), do: s |> String.trim() |> String.downcase()

  defp enumerate_senses(entries) do
    defs =
      for entry <- entries,
          meaning <- List.wrap(entry["meanings"] || []),
          defn <- List.wrap(meaning["definitions"] || []) do
        %{
          word: entry["word"] || "",
          pos: meaning["partOfSpeech"],
          definition: defn["definition"],
          example: defn["example"]
        }
      end

    defs
    |> Enum.group_by(fn d -> {normalize_word(d.word), normalize_pos(d.pos)} end)
    |> Enum.flat_map(fn {{w, p}, group} ->
      group
      |> Enum.with_index()
      |> Enum.map(fn {d, idx} ->
        %{
          id: build_id(w, p, idx),
          word: w,
          pos: p,
          idx: idx,
          definition: d.definition,
          example: d.example
        }
      end)
    end)
  end

  defp normalize_word(w), do: w |> String.trim() |> String.downcase()
  defp normalize_pos(nil), do: "unk"
  defp normalize_pos(pos) do
    case String.downcase(String.trim(pos)) do
      "adjective"    -> "adj"
      "adverb"       -> "adv"
      "interjection" -> "interj"
      "preposition"  -> "prep"
      "conjunction"  -> "conj"
      "pronoun"      -> "pron"
      other          -> other  # keep "noun", "verb", etc.
    end
  end

  defp build_id(word_norm, pos_norm, idx), do: "#{word_norm}|#{pos_norm}|#{idx}"

  defp upsert_and_maybe_start(%{id: id, word: w, pos: pos} = s) do
    attrs = %{
      id: id,  # PK "word|pos|idx"
      word: w,
      pos: pos,
      definition: Map.get(s, :definition),
      example: Map.get(s, :example),
      type: "lexicon",
      status: "inactive"
    }

    cs = BrainCell.changeset(%BrainCell{}, attrs)

    cell =
      case Db.insert(
             cs,
             on_conflict: {:replace, [:definition, :example, :pos, :updated_at]},
             conflict_target: :id,
             returning: true
           ) do
        {:ok, %BrainCell{} = c} -> c
        {:error, _}             -> Db.get!(BrainCell, id)
      end

    start_cell(cell)
    :ok
  end

  defp start_cell(%BrainCell{} = cell) do
    try do
      cond do
        function_exported?(BrainCellProc, :start, 1) ->
          case BrainCellProc.start(cell) do
            {:ok, _pid}                        -> :ok
            {:error, {:already_started, _pid}} -> :ok
            _                                  -> :ok
          end

        function_exported?(BrainCellProc, :start_link, 1) ->
          case BrainCellProc.start_link(cell) do
            {:ok, _pid}                        -> :ok
            {:error, {:already_started, _pid}} -> :ok
            _                                  -> :ok
          end

        true ->
          :ok
      end
    rescue
      _ -> :ok
    catch
      _, _ -> :ok
    end
  end

  defp now_ms() do
    try do
      System.monotonic_time(:millisecond)
    rescue
      _ -> System.system_time(:millisecond)
    end
  end
end

