defmodule Core.Tools.CurrentEventsRetrievalTest do
  use ExUnit.Case, async: true

  alias Core.Tools.WebRetrieval
  alias Core.Tools.WebRetrieval.NewsRss
  alias Core.Response.LlmPrompt
  alias Core.Response.ReflectionLoop

  defmodule DatedNews do
    def search(_query, _opts) do
      timestamp = DateTime.utc_now() |> DateTime.to_iso8601()

      {:ok,
       [
         %{
           provider: :news_rss,
           title: "Diplomats discuss Strait of Hormuz proposal",
           publisher: "Example Wire",
           published_at: timestamp,
           url: "https://news.google.com/rss/articles/example-wire-1",
           excerpt: "Dated headline only: diplomatic talks were reported."
         },
         %{
           provider: :news_rss,
           title: "Shipping disruptions remain a regional concern",
           publisher: "Second Wire",
           published_at: timestamp,
           url: "https://news.google.com/rss/articles/second-wire-2",
           excerpt: "Dated headline only: shipping disruption reported."
         }
       ]}
    end
  end

  defmodule MissingNews do
    def search(_query, _opts), do: {:error, :connection_failed}
  end

  defmodule YearWindowNews do
    def search(_query, _opts) do
      now = DateTime.utc_now()
      in_range = DateTime.add(now, -364 * 24 * 3_600, :second) |> DateTime.to_iso8601()
      too_old = DateTime.add(now, -366 * 24 * 3_600, :second) |> DateTime.to_iso8601()

      {:ok,
       [
         %{provider: :news_rss, title: "Japan conducts railgun trial",
           publisher: "Example Wire", published_at: in_range,
           url: "https://news.google.com/rss/articles/railgun-in-year",
           excerpt: "Dated headline about a Japanese railgun trial"},
         %{provider: :news_rss, title: "Historic Japanese railgun trial",
           publisher: "Example Wire", published_at: too_old,
           url: "https://news.google.com/rss/articles/railgun-too-old",
           excerpt: "Headline older than a year"}
       ]}
    end
  end

  defmodule EmptyNews do
    def search(_query, _opts), do: {:ok, []}
  end

  defmodule NewsOnRetry do
    def search(query, _opts) do
      if String.contains?(query, "latest") do
        DatedNews.search(query, [])
      else
        {:ok, []}
      end
    end
  end

  defp question(text \\ "what are your thoughts on the US Iran war currently happening") do
    %{user_text: text, session_id: "current-events-test",
      features: %{intent: :ask, confidence: 0.9},
      decision: %{action: :answer}}
  end

  test "the Iran war question routes to current-events retrieval" do
    assert {"the US Iran war", :current_events} =
      WebRetrieval.query_for(question())
  end

  test "explicit requests for latest news use the news provider" do
    assert {"latest news about Iran", :current_events} =
      WebRetrieval.query_for(question("Search the web for latest news about Iran"))
  end

  test "a geographic population question still uses fact grounding" do
    frame = %{type: :location_question, subject: "Maple Ridge, BC",
              relation: :population, grounding_status: :unverified}

    assert {"Maple Ridge, BC population census", :population} =
      WebRetrieval.query_for(%{
        user_text: "What is its population currently?",
        features: %{symbolic_frame: frame}
      })
  end

  test "routing is generic, not hardcoded to the named war" do
    assert {"the Canadian election", :current_events} =
      WebRetrieval.query_for(question("What are your thoughts on the Canadian election currently happening?"))

    assert {"the drought in Alberta", :current_events} =
      WebRetrieval.query_for(question("What are your thoughts on the drought in Alberta currently happening?"))

    assert WebRetrieval.query_for(question("I currently live in Maple Ridge.")) == nil
    assert WebRetrieval.query_for(question("Tell me about my sister.")) == nil
  end

  test "dated headlines are returned as unverified news discovery, not fact support" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: DatedNews, max_attempts: 2)

    evidence = result.web_evidence

    assert evidence.relation == :current_events
    assert evidence.provider == DatedNews
    assert evidence.status == :retrieved
    assert evidence.resolution == :headlines_available
    assert evidence.support == nil
    assert evidence.attempted_count == 1
    assert [%{kind: :news, status: :headlines_available}] = evidence.attempts
    assert Enum.map(evidence.sources, & &1.publisher) == ["Example Wire", "Second Wire"]

    prompt = LlmPrompt.build_system_prompt(result)
    assert prompt =~ "RECENT NEWS HEADLINES"
    assert prompt =~ "\n[1] "
    assert prompt =~ "\n[2] "
    refute prompt =~ "example-wire-1[2]"
    assert prompt =~ Date.to_iso8601(Date.utc_today())
    assert prompt =~ "Example Wire"
    assert prompt =~ "independently"
    assert prompt =~ "give a substantive measured assessment"
  end

  test "a missing first feed retries and uses the next dated news search" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: NewsOnRetry, max_attempts: 2)

    assert result.web_evidence.resolution == :headlines_available
    assert result.web_evidence.attempted_count == 2
    assert Enum.map(result.web_evidence.attempts, & &1.status) ==
             [:not_found, :headlines_available]
  end

  test "a failed feed does not lead to invented breaking-news claims" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: MissingNews, max_attempts: 2)

    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.support == nil
    assert result.web_evidence.attempted_count == 2
    assert result.web_evidence.status == :unavailable
  end

  test "false 'no live news' refusal is repaired only when dated sources exist" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: DatedNews, max_attempts: 2)

    draft = "I don't have live news access in this runtime, so I can't give current updates."
    {:ok, text, review} = ReflectionLoop.review(question().user_text, draft, result)

    assert review.status == :repair
    assert :news_access_refusal in review.issues
    assert text =~ "Example Wire"
    assert text =~ "Second Wire"
    assert text =~ "https://news.google.com/rss/articles/example-wire-1"
    assert text =~ "My assessment:"
    refute text =~ "I don't have live news access"
  end

  test "uncited single-headline answer from the live Iran report is repaired" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: DatedNews, max_attempts: 2)

    draft =
      "I'm observing the current situation with the US Iran war. " <>
        "It's reported that nuclear sites are being fortified for possible strikes."

    {:ok, final, review} =
      ReflectionLoop.review(question().user_text, draft, result)

    assert review.status == :repair
    assert :news_evidence_not_cited in review.issues
    assert :news_assessment_missing in review.issues
    assert review.applied?
    assert final =~ "Example Wire"
    assert final =~ "Second Wire"
    assert final =~ "https://news.google.com/rss/articles/example-wire-1"
    assert final =~ Date.to_iso8601(Date.utc_today())
    assert final =~ "My assessment:"
    assert final =~ "not independently verified"
    refute final =~ "nuclear sites"
  end

  test "complete outlet, date, matching link and assessment can pass" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: DatedNews)

    source = hd(result.web_evidence.sources)
    date = String.slice(source.published_at, 0, 10)

    draft =
      "#{source.publisher} reported on #{date}: #{source.title}. " <>
        "Source: #{source.url}\n" <>
        "My assessment: the evidence remains limited to headlines."

    {:ok, final, review} =
      ReflectionLoop.review(question().user_text, draft, result)

    assert review.status == :accept
    refute :news_evidence_not_cited in review.issues
    refute :news_assessment_missing in review.issues
    assert final == draft
  end

  test "an unrelated citation cannot satisfy news provenance checks" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: DatedNews)

    source = hd(result.web_evidence.sources)
    date = String.slice(source.published_at, 0, 10)

    draft =
      "#{source.publisher} reported on #{date}: #{source.title}. " <>
        "Source: https://news.google.com/rss/articles/UNRELATED\n" <>
        "My assessment: details need verification."

    {:ok, final, review} =
      ReflectionLoop.review(question().user_text, draft, result)

    assert review.status == :repair
    assert :news_evidence_not_cited in review.issues
    assert final =~ "https://news.google.com/rss/articles/example-wire-1"
    refute final =~ "UNRELATED"
  end

  test "requests for headlines need citations but not a personal assessment" do
    user = question("What is the latest news happening today?")
    result = WebRetrieval.enrich(user, enabled?: true, news_provider: DatedNews)
    source = hd(result.web_evidence.sources)

    draft =
      "#{source.publisher} (#{String.slice(source.published_at, 0, 10)}): " <>
        "#{source.title}. Source: #{source.url}"

    {:ok, final, review} =
      ReflectionLoop.review(user.user_text, draft, result)

    assert review.status == :accept
    assert final == draft
  end

  test "no headline data means reflection does not falsely claim news access" do
    result = WebRetrieval.enrich(question(),
      enabled?: true, news_provider: MissingNews, max_attempts: 2)

    draft = "I don't have live news access in this runtime, so I can't give current updates."
    {:ok, _text, review} = ReflectionLoop.review(question().user_text, draft, result)
    refute :news_access_refusal in review.issues
  end

  test "an empty dated-news search repairs a false claim of no web capability" do
    user = question("Search the web for the latest developments about Japanese railguns")
    result = WebRetrieval.enrich(user,
      enabled?: true, news_provider: EmptyNews, max_attempts: 2)

    assert result.web_evidence.relation == :current_events
    assert result.web_evidence.status == :not_found
    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.attempted_count == 2

    draft = "I'm sorry, but I currently don't have the capability to search the web for the latest developments."
    {:ok, final, review} = ReflectionLoop.review(user.user_text, draft, result)

    assert review.status == :repair
    assert :news_lookup_misrepresented in review.issues
    assert final =~ "I searched for dated news"
    assert final =~ "Japanese railguns"
    assert final =~ "no qualifying headlines were returned"
    refute final =~ "don't have the capability"
  end

  test "generic can't-assist refusal is repaired after a real, empty news lookup" do
    user = question("Search the web for the latest developments about Japanese railguns")
    result = WebRetrieval.enrich(user,
      enabled?: true, news_provider: EmptyNews, max_attempts: 2)

    {:ok, final, review} =
      ReflectionLoop.review(
        user.user_text,
        "I'm sorry, but I can't assist with that.",
        result
      )

    assert review.status == :repair
    assert :news_lookup_misrepresented in review.issues
    assert final =~ "I searched for dated news"
    assert final =~ "no qualifying headlines were returned"
    refute final =~ "can't assist with that"
  end

  test "a search promise after completed empty retrieval is corrected" do
    user = question("Search the web for the latest developments about Japanese railguns")

    result =
      WebRetrieval.enrich(user,
        enabled?: true, news_provider: EmptyNews, max_attempts: 2)

    draft = "I'll search the web for the latest developments in Japan's railgun technology."
    {:ok, final, review} = ReflectionLoop.review(user.user_text, draft, result)

    assert review.status == :repair
    assert :news_lookup_misrepresented in review.issues
    assert final =~ "no qualifying headlines were returned"
    refute final =~ "I'll search"
  end

  test "no retrieved results cannot become a claim that new reports were found" do
    user = question("What do you think about those developments?")

    result =
      WebRetrieval.enrich(user,
        enabled?: true, news_provider: EmptyNews,
        history: [
          %{"role" => "user", "content" => "What do you know about Japan's railgun development?"},
          %{"role" => "assistant", "content" => "I could not verify that programme."}
        ],
        max_attempts: 2)

    assert result.web_evidence.query == "Japan's railgun development"
    assert result.web_evidence.sources == []

    drafts = [
      "I found some information about the latest developments in Japan's railgun technology.",
      "Based on the information I found, it seems that there have been some recent developments in Japan's railgun technology."
    ]

    for draft <- drafts do
      {:ok, final, review} = ReflectionLoop.review(user.user_text, draft, result)

      assert review.status == :repair
      assert :news_lookup_misrepresented in review.issues
      assert final =~ "no qualifying headlines were returned"
      refute final =~ "I found some information"
    end
  end

  test "provider outage is not described as an empty search or verified news" do
    user = question("Search the web for the latest developments about Japanese railguns")
    result = WebRetrieval.enrich(user,
      enabled?: true, news_provider: MissingNews, max_attempts: 2)

    assert result.web_evidence.status == :unavailable

    draft = "I cannot browse the web to answer that."
    {:ok, final, review} = ReflectionLoop.review(user.user_text, draft, result)

    assert review.status == :repair
    assert :news_lookup_misrepresented in review.issues
    assert final =~ "news provider was unavailable or failed"
    refute final =~ "no qualifying headlines"
  end

  test "a genuine no-headline response is accepted and does not invent sources" do
    user = question("Search the web for the latest developments about Japanese railguns")
    result = WebRetrieval.enrich(user,
      enabled?: true, news_provider: EmptyNews, max_attempts: 2)

    response = "I searched for dated news but found no qualifying headlines for this topic."
    {:ok, ^response, review} = ReflectionLoop.review(user.user_text, response, result)
    refute :news_lookup_misrepresented in review.issues
    refute :news_evidence_not_cited in review.issues
  end

  test "Google News RSS requires dated attributable HTTPS headlines" do
    fixed = ~U[2026-10-13 17:00:00Z]

    xml = """
    <?xml version="1.0" encoding="UTF-8"?>
    <rss><channel>
      <item>
        <title>Strait of Hormuz update &amp; diplomacy</title>
        <link>https://news.google.com/rss/articles/abc123</link>
        <pubDate>Tue, 13 Oct 2026 15:30:00 GMT</pubDate>
        <source url="https://example.org">Example Wire</source>
      </item>
      <item>
        <title>A much older story</title>
        <link>https://news.google.com/rss/articles/older</link>
        <pubDate>Wed, 01 Apr 2020 12:00:00 GMT</pubDate>
        <source>Old Source</source>
      </item>
      <item>
        <title>No trustworthy date</title>
        <link>https://news.google.com/rss/articles/undated</link>
        <source>Unknown Source</source>
      </item>
      <item>
        <title>Attacker-controlled HTTP URL</title>
        <link>http://example.org/unsafe</link>
        <pubDate>Tue, 13 Oct 2026 15:30:00 GMT</pubDate>
        <source>Unknown Source</source>
      </item>
    </channel></rss>
    """

    fetch = fn url, opts ->
      assert url == "https://news.google.com/rss/search"
      assert opts[:params][:gl] == "CA"
      assert opts[:params][:q] =~ "when:7d"
      {:ok, %{status: 200, body: xml}}
    end

    assert {:ok, [source]} =
      NewsRss.search("US Iran war", fetch: fetch, now: fixed)

    assert source.provider == :news_rss
    assert source.publisher == "Example Wire"
    assert source.title == "Strait of Hormuz update & diplomacy"
    assert source.published_at == "2026-10-13T15:30:00Z"
    assert source.url == "https://news.google.com/rss/articles/abc123"
    refute String.contains?(source.excerpt, "older")
  end

  test "explicit historical RSS lookback is capped at one year" do
    now = ~U[2026-10-13 17:00:00Z]

    xml = """
    <rss><channel>
      <item>
        <title>Japan railgun sea trials within one year</title>
        <link>https://news.google.com/rss/articles/within-year</link>
        <pubDate>14 Oct 2025 17:00:00 GMT</pubDate>
        <source>Example Science Wire</source>
      </item>
      <item>
        <title>Railgun event older than one year</title>
        <link>https://news.google.com/rss/articles/beyond-year</link>
        <pubDate>12 Oct 2025 17:00:00 GMT</pubDate>
        <source>Old Science Wire</source>
      </item>
    </channel></rss>
    """

    fetch = fn _url, opts ->
      assert opts[:params][:q] =~ "when:1y"
      {:ok, %{status: 200, body: xml}}
    end

    assert NewsRss.max_age_hours() == 365 * 24

    assert {:ok, [%{url: "https://news.google.com/rss/articles/within-year"}]} =
             NewsRss.search("Japan railgun", fetch: fetch, now: now,
               max_age_hours: 365 * 24)

    # Even an oversized per-call lookback cannot extend past one year.
    assert {:ok, [%{url: "https://news.google.com/rss/articles/within-year"}]} =
             NewsRss.search("Japan railgun", fetch: fetch, now: now,
               max_age_hours: 2 * 365 * 24)
  end

  test "resolver rejects stale headlines even when a custom provider returns them" do
    result =
      WebRetrieval.enrich(
        question("Search the web for the latest developments about Japan's railgun"),
        enabled?: true, news_provider: YearWindowNews, max_attempts: 2
      )

    assert result.web_evidence.resolution == :exhausted
    assert result.web_evidence.status == :not_found
    assert result.web_evidence.sources == []
    assert Enum.map(result.web_evidence.attempts, & &1.status) == [:not_found, :not_found]
  end

  test "current-event RSS discovery excludes March headlines from an October question" do
    now = ~U[2026-10-14 18:00:00Z]
    xml = """
    <rss><channel>
      <item>
        <title>Outdated March assessment</title>
        <link>https://news.google.com/rss/articles/march</link>
        <pubDate>Thu, 26 Mar 2026 12:00:00 GMT</pubDate>
        <source>Example Wire</source>
      </item>
      <item>
        <title>Latest October report</title>
        <link>https://news.google.com/rss/articles/october</link>
        <pubDate>Wed, 14 Oct 2026 12:00:00 GMT</pubDate>
        <source>Example Wire</source>
      </item>
    </channel></rss>
    """

    fetch = fn _url, opts ->
      assert opts[:params][:q] =~ "when:7d"
      {:ok, %{status: 200, body: xml}}
    end

    assert NewsRss.default_age_hours() == 7 * 24
    assert {:ok, [%{title: "Latest October report"}]} =
             NewsRss.search("Iran war currently happening", fetch: fetch, now: now)
  end

  test "RSS network failure remains explicit and safe" do
    fail = fn _url, _opts -> {:error, :econnrefused} end
    assert {:error, :econnrefused} = NewsRss.search("US Iran war", fetch: fail)
  end

  test "RSS with only stale sources does not count as fresh news" do
    xml = """
    <rss><channel><item>
    <title>Old headline</title>
    <link>https://news.google.com/rss/articles/old</link>
    <pubDate>Fri, 01 Jan 2021 13:00:00 GMT</pubDate>
    <source>Old Wire</source>
    </item></channel></rss>
    """
    fetch = fn _url, _opts -> {:ok, %{status: 200, body: xml}} end
    assert {:ok, []} =
      NewsRss.search("election",
        fetch: fetch, now: ~U[2026-10-13 17:00:00Z])
  end
end
