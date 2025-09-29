defmodule Core.Lexicon do
  @moduledoc false
  alias Core.NegCache
  alias Db.BrainCell

  # Ensure BrainCell rows exist for a list of phrases/norms.
  # Reuses all/1 to insert; returns the rows inserted/found.
  @spec ensure_cells([String.t()]) :: [map()]
  def ensure_cells(phrases) when is_list(phrases) do
    tokens =
      phrases
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(fn p ->
        %{
          phrase: p,
          # harmless defaults; unused by all/1
          span: {0, 1},
          n: max(1, length(String.split(p))),
          mw: String.contains?(p, " "),
          instances: []
        }
      end)

    si = all(%{tokens: tokens})
    Map.get(si, :active_cells, [])
  end

  def all(%{tokens: tokens} = si) do
    now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

    cells =
      tokens
      |> Enum.flat_map(fn t ->
        case lookup(t.phrase) do
          {:ok, list} when is_list(list) -> list
          list when is_list(list) -> list
          _ -> []
        end
      end)
      # accept %Db.BrainCell{} or plain maps
      |> Enum.map(fn
        %Db.BrainCell{} = s ->
          s |> Map.from_struct() |> Map.drop([:__meta__, :__struct__])

        m when is_map(m) ->
          m
      end)
      # force norm + timestamps (NOT put_new)
      |> Enum.map(fn m ->
        word = m[:word] || m["word"] || ""

        norm =
          case m[:norm] || m["norm"] do
            nil -> word |> String.downcase() |> String.replace(~r/\s+/, " ") |> String.trim()
            "" -> word |> String.downcase() |> String.replace(~r/\s+/, " ") |> String.trim()
            v -> v
          end

        m
        |> Map.put(:norm, norm)
        # force-set
        |> Map.put(:inserted_at, now)
        # force-set
        |> Map.put(:updated_at, now)
        |> Map.put_new(:activation, 0.0)
        |> Map.put_new(:modulated_activation, 0.0)
        |> Map.put_new(:dopamine, 0.0)
        |> Map.put_new(:serotonin, 0.0)
        |> Map.put_new(:connections, [])
      end)
      # dedup by id (keep last)
      |> Enum.reduce(%{}, fn m, acc ->
        key = m[:id] || m["id"]
        Map.put(acc, key, m)
      end)
      |> Map.values()

    {_, rows} =
      Db.insert_all(
        Db.BrainCell,
        cells,
        on_conflict:
          {:replace, [:definition, :example, :synonyms, :antonyms, :status, :updated_at]},
        conflict_target: :id,
        returning: [
          :id,
          :word,
          :norm,
          :pos,
          :definition,
          :example,
          :synonyms,
          :antonyms,
          :status,
          :inserted_at,
          :updated_at
        ]
      )

    # FIX: use Map.put/3 so we don't require :active_cells to pre-exist
    Map.put(si, :active_cells, rows || cells)
  end

  @spec lookup(String.t()) :: {:ok, [map()]} | {:error, term} | []
  def lookup(phrase) when is_binary(phrase) do
    phrase = String.trim(phrase)

    cond do
      phrase == "" ->
        {:error, :empty}

      NegCache.exists?(phrase) ->
        # skip lookups we’ve previously marked as absent
        []

      true ->
        case GenServer.call(Lexicon, {:fetch_word, phrase}, 7_000) do
          {:ok, %Tesla.Env{status: 200, body: body}} when is_list(body) ->
            case body do
              [first | _] when is_map(first) ->
                (first["meanings"] || [])
                |> Enum.reduce([], fn d, acc ->
                  defs = Enum.with_index(d["definitions"] || [])

                  acc ++
                    Enum.reduce(defs, [], fn {map, indx}, accum ->
                      accum ++
                        [
                          %BrainCell{
                            id: Enum.join([phrase, d["partOfSpeech"], indx], "|"),
                            word: phrase,
                            pos: d["partOfSpeech"],
                            definition: map["definition"],
                            example: map["example"],
                            synonyms: d["synonyms"] || [],
                            antonyms: d["antonyms"] || [],
                            status: "inactive"
                          }
                        ]
                    end)
                end)
                |> List.flatten()

              _ ->
                {:error, :unexpected_body}
            end

          {:ok, %Tesla.Env{status: 404}} ->
            NegCache.put(phrase)
            {:error, :not_found}

          {:ok, %Tesla.Env{status: s, body: b}} ->
            {:error, {:http_error, s, b}}

          {:error, %Tesla.Error{reason: r}} ->
            {:error, {:transport, r}}

          {:error, reason} ->
            {:error, reason}

          other ->
            {:error, {:unexpected, other}}
        end
    end
  end
end
