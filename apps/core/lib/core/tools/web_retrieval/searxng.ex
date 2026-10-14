defmodule Core.Tools.WebRetrieval.Searxng do
  @moduledoc """
  Optional operator-configured SearXNG JSON search provider. Only search
  snippets are consumed. Linked webpages are never fetched by this adapter.
  """

  @user_agent "Symbrella/0.1 (https://github.com/bushman77/Symbrella)"

  @spec search(String.t(), keyword()) :: {:ok, list(map())} | {:error, term()}
  def search(query, opts \\ []) when is_binary(query) do
    url = Keyword.get(opts, :endpoint, Application.get_env(:core, :searxng_url))
    fetch = Keyword.get(opts, :fetch, &Req.get/2)
    timeout = Keyword.get(opts, :timeout_ms, 3_500)

    if allowed_endpoint?(url) do
      case fetch.(String.trim_trailing(url, "/") <> "/search",
             params: [q: query, format: "json", categories: "general"],
             headers: [{"user-agent", @user_agent}, {"accept", "application/json"}],
             receive_timeout: timeout,
             retry: false
           ) do
        {:ok, %{status: 200, body: %{"results" => results}}} when is_list(results) ->
          {:ok, results |> Enum.take(3) |> Enum.flat_map(&source/1)}

        {:ok, %{status: status}} ->
          {:error, {:search_http, status}}

        {:error, reason} ->
          {:error, reason}

        _ ->
          {:error, :unexpected_search_response}
      end
    else
      {:error, :no_safe_endpoint}
    end
  end

  defp allowed_endpoint?(url) when is_binary(url) do
    case URI.parse(url) do
      %URI{scheme: "https", host: host} when is_binary(host) -> true
      %URI{scheme: "http", host: host} when host in ["localhost", "127.0.0.1"] -> true
      _ -> false
    end
  end

  defp allowed_endpoint?(_), do: false

  defp source(%{"title" => title, "url" => url, "content" => content})
       when is_binary(title) and is_binary(url) and is_binary(content) do
    if String.starts_with?(url, "https://") do
      [%{
        title: String.slice(title, 0, 160),
        url: url,
        excerpt: content |> String.replace(~r/<[^>]*>/u, " ") |> String.replace(~r/\s+/u, " ") |> String.slice(0, 850),
        provider: :searxng
      }]
    else
      []
    end
  end

  defp source(_), do: []
end
