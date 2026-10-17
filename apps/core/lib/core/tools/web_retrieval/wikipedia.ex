defmodule Core.Tools.WebRetrieval.Wikipedia do
  @moduledoc """
  Keyless, fixed-host Wikimedia provider.

  Search snippets are supplemented by bounded plaintext article excerpts.
  No user-controlled result URL is fetched; article text uses the same fixed
  en.wikipedia.org API host. HTTP is injectable for offline tests.
  """

  @search_url "https://en.wikipedia.org/w/rest.php/v1/search/page"
  @article_url "https://en.wikipedia.org/w/api.php"
  @user_agent "Symbrella/0.1 (https://github.com/bushman77/Symbrella)"
  @max_results 3

  @spec search(String.t(), keyword()) :: {:ok, list(map())} | {:error, term()}
  def search(query, opts \\ []) when is_binary(query) do
    timeout = Keyword.get(opts, :timeout_ms, 3_500)
    fetch = Keyword.get(opts, :fetch, &Req.get/2)
    args = [
      params: [q: query, limit: @max_results],
      headers: [{"user-agent", @user_agent}, {"accept", "application/json"}],
      receive_timeout: timeout,
      retry: false
    ]

    with {:ok, %{status: 200, body: %{"pages" => pages}}} when is_list(pages) <-
           fetch.(@search_url, args) do
      sources =
        pages
        |> Enum.take(@max_results)
        |> Enum.flat_map(&to_source/1)

      # General reference requests need an article lead, not a sentence cut
      # from the middle of its population section. Population requests retain
      # the focused census snippet used by the numeric grounding validator.
      excerpt_focus =
        if Regex.match?(~r/\b(?:population|census|inhabitants)\b/iu, query),
          do: :population,
          else: :lead

      sources =
        case sources do
          [first | rest] -> [supplement(first, fetch, timeout, excerpt_focus) | rest]
          [] -> []
        end

      {:ok, sources}
    else
      {:ok, %{status: status}} -> {:error, {:search_http, status}}
      {:error, reason} -> {:error, reason}
      _ -> {:error, :unexpected_search_response}
    end
  end

  defp supplement(source, fetch, timeout, excerpt_focus) do
    args = [
      params: [
        action: "query", prop: "extracts", format: "json",
        explaintext: 1, redirects: 1, titles: source.title
      ],
      headers: [{"user-agent", @user_agent}, {"accept", "application/json"}],
      receive_timeout: timeout,
      retry: false
    ]

    case fetch.(@article_url, args) do
      {:ok, %{status: 200, body: %{"query" => %{"pages" => pages}}}} when is_map(pages) ->
        text =
          pages
          |> Map.values()
          |> Enum.find_value("", fn page -> Map.get(page, "extract") end)
          |> to_string()
          |> String.slice(0, 100_000)

        selected = relevant_excerpt(text, source.excerpt, excerpt_focus)
        %{source | excerpt: selected}

      _ ->
        source
    end
  rescue
    _ -> source
  end

  defp to_source(%{"title" => title} = result) when is_binary(title) do
    safe_title = title |> String.replace(" ", "_") |> URI.encode(&URI.char_unreserved?/1)
    desc = clean(Map.get(result, "description", ""))
    excerpt = clean(Map.get(result, "excerpt", ""))

    [%{
      title: String.slice(title, 0, 160),
      url: "https://en.wikipedia.org/wiki/" <> safe_title,
      excerpt: String.slice(Enum.join(Enum.reject([desc, excerpt], &(&1 == "")), ". "), 0, 750),
      provider: :wikipedia
    }]
  end

  defp to_source(_), do: []

  defp relevant_excerpt(text, existing, focus) when is_binary(text) do
    start =
      if focus == :population do
        case Regex.run(~r/\bpopulation\b/iu, text, return: :index) do
          [{pos, _} | _] -> max(0, pos - 160)
          _ -> 0
        end
      else
        0
      end

    tail =
      text
      |> String.slice(start, 1_150)
      |> clean()

    case tail do
      "" -> existing
      _ -> String.slice(tail <> " " <> existing, 0, 1_450)
    end
  end

  defp clean(value) when is_binary(value) do
    value
    |> String.replace(~r/<[^>]*>/u, " ")
    |> String.replace(~r/[\p{Cc}\p{Cf}]/u, " ")
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp clean(_), do: ""
end
