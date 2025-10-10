defmodule Lexicon do
  @moduledoc """
  Tiny client for dictionaryapi.dev. Returns:
  %{word: "...", senses: [%{pos, definition, example, synonyms, antonyms}, ...]}
  """

  @endpoint "https://api.dictionaryapi.dev/api/v2/entries/en/"

  @spec lookup(String.t()) :: map()
  def lookup(word) when is_binary(word) do
    url = @endpoint <> URI.encode(word)

    req =
      Finch.build(:get, url, [{"accept", "application/json"}])

    case Finch.request(req, Lexicon.Finch) do
      {:ok, %Finch.Response{status: 200, body: body}} ->
        case Jason.decode(body) do
          {:ok, items} when is_list(items) ->
            %{word: norm(word), senses: flatten_senses(items)}

          _ ->
            %{word: norm(word), senses: []}
        end

      # 404 is a common “no results” for that API
      {:ok, %Finch.Response{status: 404}} ->
        %{word: norm(word), senses: []}

      _other ->
        %{word: norm(word), senses: []}
    end
  rescue
    _ -> %{word: norm(word), senses: []}
  catch
    _, _ -> %{word: norm(word), senses: []}
  end

  # ——— helpers ———

  defp flatten_senses(items) do
    items
    |> Enum.flat_map(fn entry ->
      meanings = entry["meanings"] || []

      Enum.flat_map(meanings, fn m ->
        pos = m["partOfSpeech"] |> down()
        syn_m = m["synonyms"] || []
        ant_m = m["antonyms"] || []
        defs = m["definitions"] || []

        Enum.map(defs, fn d ->
          %{
            pos: pos,
            definition: d["definition"],
            example: d["example"],
            synonyms: (d["synonyms"] || []) ++ syn_m,
            antonyms: (d["antonyms"] || []) ++ ant_m
          }
        end)
      end)
    end)
  end

  defp down(nil), do: nil
  defp down(v) when is_binary(v), do: String.downcase(v)

  defp norm(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
end
