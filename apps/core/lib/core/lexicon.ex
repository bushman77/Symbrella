defmodule Core.Lexicon do
  @moduledoc false
  alias Core.NegCache
  alias Db.BrainCell

  def all(si) do
    si.tokens
    |>Enum.each(fn token ->
      lookup(token.phrase)
    end)
  end

  @spec lookup(String.t()) :: {:ok, [map()]} | {:error, term}
  def lookup(phrase) when is_binary(phrase) do
    phrase = phrase |> String.trim()
    (NegCache.exists?(phrase) && Regex.match?(~r/\s/u, phrase))
    |> case do
      true -> {:error, :not_found}
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
          end)|> List.flatten
          |> IO.inspect
            {:ok, body}

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

