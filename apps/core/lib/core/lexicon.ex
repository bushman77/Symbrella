defmodule Core.Lexicon do
  @moduledoc false

  @spec lookup(String.t()) :: {:ok, [map()]} | {:error, term}
  def lookup(phrase) when is_binary(phrase) do
    phrase = phrase |> String.trim()
    if phrase == "", do: {:error, :empty}

    case GenServer.call(Lexicon, {:fetch_word, phrase}, 7_000) do
      {:ok, %Tesla.Env{status: 200, body: body}} when is_list(body) ->
        {:ok, body}

      {:ok, %Tesla.Env{status: 404}} ->
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

