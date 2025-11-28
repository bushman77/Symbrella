defmodule Lexicon do
  @moduledoc """
  Minimal Lexicon client.

  Purpose:
    - `enrich/1` fetches all senses/entries for a word from a remote lexicon API.
    - Uses :hackney directly (no Tesla).
    - Decodes JSON if Jason is available; otherwise returns raw body.
  """

  # Placeholder-friendly: swap later if you move off dictionaryapi.dev
  @base_url "https://api.dictionaryapi.dev/api/v2"

  @connect_timeout 3_000
  @recv_timeout 5_000

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

      headers =
        Keyword.get(opts, :headers, [
          {"accept", "application/json"}
        ])

      hackney_opts =
        Keyword.get(opts, :hackney_opts, [])
        |> Keyword.put_new(:follow_redirect, true)
        |> Keyword.put_new(:connect_timeout, @connect_timeout)
        |> Keyword.put_new(:recv_timeout, @recv_timeout)

      with {:ok, status, _resp_headers, ref} <- :hackney.request(:get, url, headers, "", hackney_opts),
           {:ok, body} <- :hackney.body(ref) do
        {:ok, %{status: status, url: url, data: maybe_decode_json(body)}}
      else
        {:error, reason} -> {:error, reason}
        other -> {:error, other}
      end
    end
  end

  defp build_url(word) do
    "#{@base_url}/entries/en/#{URI.encode(word)}"
  end

  defp maybe_decode_json(body) when is_binary(body) do
    if Code.ensure_loaded?(Jason) and function_exported?(Jason, :decode, 1) do
      case Jason.decode(body) do
        {:ok, decoded} -> decoded
        _ -> body
      end
    else
      body
    end
  end
end

