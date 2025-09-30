defmodule Lexicon do
  use Tesla

  plug Tesla.Middleware.BaseUrl, "https://api.dictionaryapi.dev/api/v2"
  plug Tesla.Middleware.JSON
  plug Tesla.Middleware.Timeout, timeout: 5_000

  adapter Tesla.Adapter.Finch, name: Lexicon.Finch

  @spec lookup(String.t()) :: %{word: String.t(), senses: list()}
  def lookup(word) when is_binary(word) do
    case get("/entries/en/#{URI.encode(word)}") do
      {:ok, %Tesla.Env{status: 200, body: body}} ->
        %{word: word, senses: normalize(body)}
      _ ->
        %{word: word, senses: []}
    end
  end

  defp normalize(_body) do
    # map API payload into [%{pos, definition, example, synonyms, antonyms}]
    # …(your existing normalizer here)…
    []
  end
end

