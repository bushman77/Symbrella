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
            d["definitions"]
            |>Enum.reduce([], fn e, accum ->
#              e|> IO.inspect
%BrainCell{
  id: Enum.join([phrase, d["partOfSpeech"]], "|"),
  word: phrase,
  norm: nil,
  pos: d["partOfSpeech"],
  definition: e["definition"],
  example: e["example"],
  gram_function: nil,
  synonyms: d["synonyms"],
  antonyms: d["antonyms"],
  semantic_atoms: [],
  type: nil,
  status: "inactive",
  activation: 0.0,
  modulated_activation: 0.0,
  dopamine: 0.0,
  serotonin: 0.0,
  position: nil,
  connections: [],
  last_dose_at: nil,
  last_substance: nil,
  token_id: nil,
  inserted_at: nil,
  updated_at: nil
}
|>IO.inspect

            end)
          end)
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

