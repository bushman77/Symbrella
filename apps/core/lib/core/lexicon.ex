defmodule Core.Lexicon do
  @moduledoc false
  alias Core.NegCache
  alias Db.BrainCell

def all(%{tokens: tokens} = si) do
  now = NaiveDateTime.utc_now() |> NaiveDateTime.truncate(:second)

  cells =
    tokens
    |> Enum.flat_map(fn t ->
      case lookup(t.phrase) do
        {:ok, list} when is_list(list) -> list
        list when is_list(list)        -> list
        _                              -> []
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
        case (m[:norm] || m["norm"]) do
          nil -> word |> String.downcase() |> String.replace(~r/\s+/, " ") |> String.trim()
          ""  -> word |> String.downcase() |> String.replace(~r/\s+/, " ") |> String.trim()
          v   -> v
        end

      m
      |> Map.put(:norm, norm)
      |> Map.put(:inserted_at, now)   # <<— force-set
      |> Map.put(:updated_at,  now)   # <<— force-set
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
      on_conflict: {:replace, [:definition, :example, :synonyms, :antonyms, :status, :updated_at]},
      conflict_target: :id,
      returning: [:id, :word, :norm, :pos, :definition, :example, :synonyms, :antonyms, :status, :inserted_at, :updated_at]
    )

  %{si | active_cells: rows || cells}
end
 
  @spec lookup(String.t()) :: {:ok, [map()]} | {:error, term}
  def lookup(phrase) when is_binary(phrase) do
    phrase = phrase |> String.trim()
    (NegCache.exists?(phrase) && Regex.match?(~r/\s/u, phrase))
    |> case do
      true -> []
      _ -> 
        if phrase == "", do: {:error, :empty}
        case GenServer.call(Lexicon, {:fetch_word, phrase}, 7_000) do
          {:ok, %Tesla.Env{status: 200, body: body}} when is_list(body) ->
          Enum.at(body, 0)["meanings"]
          |> Enum.reduce([], fn d, acc ->
            acc ++ [
            Enum.with_index(d["definitions"])
            |>Enum.reduce([], fn e, accum ->
              {map, indx} = e
              accum ++ [
              %BrainCell{
                id: Enum.join([phrase, d["partOfSpeech"], indx], "|"),
                word: phrase,
                pos: d["partOfSpeech"],
                definition: map["definition"],
                example: map["example"],
                synonyms: d["synonyms"],
                antonyms: d["antonyms"],
                status: "inactive",
              }]
            end)]
          end)
          |> List.flatten

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

