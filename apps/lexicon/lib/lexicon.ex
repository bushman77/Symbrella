defmodule Lexicon do
  @moduledoc """
  Minimal Lexicon client.

  Purpose:
    - `enrich/1` fetches all senses/entries for a word from a remote lexicon API.
    - Uses Req for HTTP requests.
    - Decodes JSON if Jason is available; otherwise returns raw body.
  """

  # Placeholder-friendly: swap later if you move off dictionaryapi.dev
  @base_url "https://api.dictionaryapi.dev/api/v2"

  @receive_timeout 5_000

  @type enrich_ok :: %{
          status: non_neg_integer(),
          url: String.t(),
          data: any()
        }

  @doc """
  Fetch all entries/senses for `word`.

  Mirrors the old Tesla route:
    GET #{@base_url}/entries/en/<word>

  Returns:
    - {:ok, %{status, url, data}}
    - {:error, reason}
  """
  @spec enrich(String.t(), keyword()) :: {:ok, enrich_ok()} | {:error, any()}
  def enrich(word, opts \\ []) when is_binary(word) do
    word = String.trim(word)

    if word == "" do
      {:error, :empty_word}
    else
      url = build_url(word)

      req_opts = [
        headers: Keyword.get(opts, :headers, [{"accept", "application/json"}]),
        receive_timeout: Keyword.get(opts, :timeout, @receive_timeout),
        retry: Keyword.get(opts, :retry, false),
        redirect: Keyword.get(opts, :redirect, true)
      ]

      case Req.get(url, req_opts) do
        {:ok, %{status: status, body: body}} ->
          {:ok, %{status: status, url: url, data: body}}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp build_url(word) do
    "#{@base_url}/entries/en/#{URI.encode(word)}"
  end

  def hello, do: :world
end
