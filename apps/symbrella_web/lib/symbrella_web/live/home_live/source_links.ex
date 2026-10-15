defmodule SymbrellaWeb.ChatLive.SourceLinks do
  @moduledoc """
  Safe presentation of HTTP(S) links in assistant chat messages.

  Source labels and site-origin favicons are rendered by HEEx. The original
  assistant reply and persisted Explain data are not altered.
  """

  @url_pattern ~r/(?:Source:\s*)?https?:\/\/[^\s<>"']+/iu
  @trailing_punctuation ~r/[.,;!?]+$/u
  @host_pattern ~r/\A[a-z0-9.-]+\z/iu
  @max_url_bytes 2_048

  @spec parts(String.t()) :: [map()]
  def parts(text) when is_binary(text) do
    {reversed, cursor} =
      @url_pattern
      |> Regex.scan(text, return: :index)
      |> Enum.reduce({[], 0}, fn [{start, length}], {acc, previous} ->
        prefix = binary_part(text, previous, start - previous)
        token = binary_part(text, start, length)

        case link_part(token) do
          {:ok, link, trailing} ->
            {[
               %{kind: :text, text: trailing},
               link,
               %{kind: :text, text: prefix}
               | acc
             ], start + length}

          :invalid ->
            {[%{kind: :text, text: prefix <> token} | acc], start + length}
        end
      end)

    final_text = binary_part(text, cursor, byte_size(text) - cursor)

    [%{kind: :text, text: final_text} | reversed]
    |> Enum.reverse()
    |> Enum.reject(&(&1.kind == :text and &1.text == ""))
    |> Enum.reduce([], fn
      %{kind: :text, text: text}, [%{kind: :text, text: previous} | rest] ->
        [%{kind: :text, text: previous <> text} | rest]

      part, acc ->
        [part | acc]
    end)
    |> Enum.reverse()
  end

  def parts(_), do: []

  defp link_part(token) do
    candidate = Regex.replace(~r/\ASource:\s*/iu, token, "")
    url = Regex.replace(@trailing_punctuation, candidate, "")

    with true <- byte_size(url) <= @max_url_bytes,
         %URI{scheme: scheme, host: host, userinfo: nil} = parsed <- URI.parse(url),
         true <- scheme in ["https", "http"],
         true <- is_binary(host) and Regex.match?(@host_pattern, host),
         true <- String.contains?(host, ".") or host == "localhost" do
      favicon =
        parsed
        |> Map.put(:path, "/favicon.ico")
        |> Map.put(:query, nil)
        |> Map.put(:fragment, nil)
        |> URI.to_string()

      trailing =
        binary_part(candidate, byte_size(url), byte_size(candidate) - byte_size(url))

      {:ok, %{kind: :source, url: url, favicon: favicon}, trailing}
    else
      _ -> :invalid
    end
  rescue
    _ -> :invalid
  end
end
