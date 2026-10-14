defmodule Core.Tools.WebRetrieval.NewsRss do
  @moduledoc """
  Keyless recent-news headline discovery from Google News RSS search.

  These are dated *headlines*, not verified article contents. We do not fetch
  arbitrary publisher pages, assume headlines are correct, or classify a news
  item as independent corroboration. The endpoint is fixed; user text enters
  only as an encoded query parameter.
  """

  @url "https://news.google.com/rss/search"
  @user_agent "Symbrella/0.1 (https://github.com/bushman77/Symbrella)"
  @max_results 3
  @max_feed_bytes 500_000
  # Real-time questions should not default to a one-year-old headline.
  # Historical research can explicitly request a larger bounded window.
  @max_age_hours 365 * 24
  @default_age_hours 7 * 24

  @doc "Hard cap for explicitly requested news lookback windows."
  @spec max_age_hours() :: pos_integer()
  def max_age_hours, do: @max_age_hours

  @doc "Default freshness requirement for present-day news discovery."
  @spec default_age_hours() :: pos_integer()
  def default_age_hours, do: @default_age_hours

  @doc "Verify timestamp freshness at retrieval and evidence acceptance boundaries."
  @spec recent_source?(map(), DateTime.t(), pos_integer()) :: boolean()
  def recent_source?(%{published_at: stamp}, %DateTime{} = now, hours)
      when is_binary(stamp) and is_integer(hours) and hours > 0 do
    recent?(stamp, now, min(hours, @max_age_hours))
  end

  def recent_source?(_, _, _), do: false

  @spec search(String.t(), keyword()) :: {:ok, list(map())} | {:error, term()}
  def search(query, opts \\ [])
  def search(query, opts) when is_binary(query) do
    timeout = Keyword.get(opts, :timeout_ms, 3_500)
    fetch = Keyword.get(opts, :fetch, &Req.get/2)
    now = Keyword.get(opts, :now, DateTime.utc_now())
    max_age =
      case Keyword.get(opts, :max_age_hours, @default_age_hours) do
        hours when is_integer(hours) and hours > 0 -> min(hours, @max_age_hours)
        _ -> @default_age_hours
      end

    args = [
      params: [q: String.slice(query, 0, 160) <> " " <> lookback_filter(max_age),
               hl: "en-CA", gl: "CA", ceid: "CA:en"],
      headers: [{"user-agent", @user_agent},
                {"accept", "application/rss+xml, application/xml"}],
      receive_timeout: timeout,
      retry: false
    ]

    case fetch.(@url, args) do
      {:ok, %{status: 200, body: body}} when is_binary(body) and byte_size(body) <= @max_feed_bytes ->
        items =
          Regex.scan(~r/<item(?:\s[^>]*)?>(.*?)<\/item>/isu, body, capture: :all_but_first)
          |> Enum.take(20)
          |> Enum.flat_map(fn [xml] -> source(xml, now, max_age) end)
          |> Enum.take(@max_results)

        {:ok, items}

      {:ok, %{status: 200}} -> {:error, :invalid_feed}
      {:ok, %{status: code}} -> {:error, {:news_http, code}}
      {:error, reason} -> {:error, reason}
      _ -> {:error, :unexpected_news_response}
    end
  end

  def search(_, _), do: {:error, :invalid_query}

  defp lookback_filter(hours) when hours <= 24, do: "when:1d"
  defp lookback_filter(hours) when hours <= 72, do: "when:3d"
  defp lookback_filter(hours) when hours <= 168, do: "when:7d"
  defp lookback_filter(_), do: "when:1y"

  defp source(xml, now, max_age) do
    title = field(xml, "title")
    url = field(xml, "link")
    publisher = field(xml, "source")
    date = field(xml, "pubDate")
    published_at = parse_date(date)

    if is_binary(published_at) and recent?(published_at, now, max_age) and
         title != "" and
         String.starts_with?(url, "https://news.google.com/") and
         publisher != "" do
      label = if publisher == "", do: "unattributed", else: publisher

      [%{
        provider: :news_rss,
        title: String.slice(title, 0, 220),
        url: String.slice(url, 0, 800),
        excerpt: "Dated headline only; reported by #{label} at #{published_at}: " <>
                   String.slice(title, 0, 220),
        publisher: String.slice(publisher, 0, 120),
        published_at: published_at
      }]
    else
      []
    end
  end

  defp field(xml, name) do
    pattern = ~r/<#{name}(?:\s[^>]*)?>(.*?)<\/#{name}>/isu

    case Regex.run(pattern, xml, capture: :all_but_first) do
      [value] ->
        value
        |> String.replace(~r/^<!\[CDATA\[(.*?)\]\]>$/su, "\\1")
        |> String.replace("&amp;", "&")
        |> String.replace("&quot;", "\"")
        |> String.replace("&apos;", "'")
        |> String.replace("&#39;", "'")
        |> String.replace("&lt;", "<")
        |> String.replace("&gt;", ">")
        |> String.replace(~r/<[^>]+>/u, " ")
        |> String.replace(~r/\s+/u, " ")
        |> String.trim()

      _ -> ""
    end
  end

  # Google News pubDate uses RFC 822 GMT. Reject unknown zones rather than
  # silently assuming local time and incorrectly claiming an item is recent.
  defp parse_date(date) do
    pattern =
      ~r/^(?:[A-Za-z]{3},\s*)?(\d{1,2})\s+([A-Za-z]{3})\s+(\d{4})\s+(\d{2}):(\d{2}):(\d{2})\s+(?:GMT|UTC|\+0000)$/u

    months = %{"Jan" => 1, "Feb" => 2, "Mar" => 3, "Apr" => 4,
               "May" => 5, "Jun" => 6, "Jul" => 7, "Aug" => 8,
               "Sep" => 9, "Oct" => 10, "Nov" => 11, "Dec" => 12}

    case Regex.run(pattern, date, capture: :all_but_first) do
      [day, mon, year, hour, min, sec] ->
        with month when is_integer(month) <- Map.get(months, mon),
             {:ok, d} <- Date.new(String.to_integer(year), month, String.to_integer(day)),
             {:ok, t} <- Time.new(String.to_integer(hour), String.to_integer(min),
                                  String.to_integer(sec)),
             {:ok, dt} <- DateTime.new(d, t, "Etc/UTC") do
          DateTime.to_iso8601(dt)
        else
          _ -> nil
        end

      _ -> nil
    end
  end

  defp recent?(stamp, %DateTime{} = now, hours) when is_integer(hours) and hours > 0 do
    with {:ok, dt, _} <- DateTime.from_iso8601(stamp) do
      age = DateTime.diff(now, dt, :second)
      age >= -3600 and age <= hours * 3600
    else
      _ -> false
    end
  end

  defp recent?(_, _, _), do: false
end
