defmodule Core.Response.NewsLookupTest do
  use ExUnit.Case, async: false

  alias Core.Response
  alias Core.Response.NewsLookup
  alias Core.Tools.WebRetrieval

  defmodule FakeNews do
    def search(query, _opts) do
      if pid = Application.get_env(:core, :news_lookup_test_pid) do
        send(pid, {:news_lookup_query, query})
      end

      date = DateTime.utc_now() |> DateTime.to_iso8601()
      source = %{
        provider: :news_rss,
        publisher: "Test Newsroom",
        title: "Publisher reports a new diplomatic development",
        excerpt: "Dated headline only; article contents not verified",
        published_at: date,
        url: "https://news.google.com/rss/articles/test-news-one"
      }

      case Application.get_env(:core, :news_lookup_test_mode, :dated) do
        :dated -> {:ok, [source]}
        :unavailable -> {:error, :blocked_network}
        :stale ->
          old = DateTime.utc_now() |> DateTime.add(-10 * 86_400, :second)
          {:ok, [%{source | published_at: DateTime.to_iso8601(old)}]}
        :invalid_url -> {:ok, [%{source | url: "https://example.net/forged"}]}
      end
    end
  end

  setup do
    keys = [:llm_inference_enabled?, :web_retrieval,
            :news_lookup_test_pid, :news_lookup_test_mode]
    before = Map.new(keys, fn key -> {key, Application.get_env(:core, key, :unset)} end)

    Application.put_env(:core, :llm_inference_enabled?, false)
    Application.put_env(:core, :web_retrieval, [
      enabled?: true, news_provider: FakeNews, timeout_ms: 500
    ])
    Application.put_env(:core, :news_lookup_test_pid, self())
    Application.put_env(:core, :news_lookup_test_mode, :dated)

    on_exit(fn ->
      for {key, value} <- before do
        if value == :unset,
          do: Application.delete_env(:core, key),
          else: Application.put_env(:core, key, value)
      end
    end)

    :ok
  end

  defp input(text) do
    %{
      source: :user, sentence: text, text: text,
      intent: :ask_info, confidence: 0.72,
      session_id: "news-lookup-#{System.unique_integer([:positive])}"
    }
  end

  defp news(text) do
    NewsLookup.generate(text,
      %{risk_bucket: :low, intent: :ask_info},
      %{action: :answer}, input(text))
  end

  test "the exact live misspelling resolves to generic current-events discovery" do
    for text <- [
      "what do you think if the U.S.  iranian war",
      "what do you think of the U.S. -Iran war",
      "what do you think of the U.S. iran war",
      "what do you think of the us iran war",
      "what do you think of the iranian american war",
      "What do you think about the Russia Ukraine war?"
    ] do
      assert %{relation: :current_events, request_type: :current_events,
               trigger: :conflict_opinion} = WebRetrieval.route_for(%{user_text: text})

      assert {:ok, response, meta} = news(text), text
      assert response =~ "Recent headlines:"
      assert response =~ "Test Newsroom"
      assert response =~ "Publisher reports a new diplomatic development"
      assert response =~ "https://news.google.com/rss/articles/test-news-one"
      assert response =~ "not verified facts"
      assert meta.status == :symbolic_news_discovery
      assert meta.outcome == :headlines_available
      assert meta.source_count == 1
      assert meta.article_contents_verified? == false
      assert meta.external_facts_verified? == false
      assert meta.llm_called? == false
      assert_receive {:news_lookup_query, _query}
    end
  end

  test "the exact lowercase live question is a current-events route" do
    text = "what do you think of the U.S. iran war"

    assert %{relation: :current_events, request_type: :current_events,
             trigger: :conflict_opinion} = WebRetrieval.route_for(%{user_text: text})

    assert {:ok, answer, metadata} = news(text)
    assert answer =~ "Recent headlines:"
    assert metadata.source_count == 1
    assert metadata.llm_called? == false
    assert_receive {:news_lookup_query, _query}
  end

  test "the live no-LLM response planner uses fetched news rather than lexical repair" do
    text = "what do you think if the U.S. iranian war"
    {_tone, response, meta} = Response.plan(input(text), %{})

    assert response =~ "Recent headlines:"
    assert meta.response_source == :symbolic_news_discovery
    assert meta.response_reflection.status == :symbolic_news_discovery
    assert meta.response_reflection.source_count == 1
    assert meta.response_reflection.content_scope == :headlines_only
    assert meta.response_reflection.article_contents_verified? == false
    assert meta.response_reflection.external_facts_verified? == false
    assert meta.response_reflection.llm_called? == false
    assert meta.curiosity_probe == nil
    assert_receive {:news_lookup_query, query}
    assert query =~ "iranian war"
  end

  test "unavailable or stale news reports only a failed lookup, not invented facts" do
    text = "What do you think about the Russia Ukraine war?"
    for mode <- [:unavailable, :stale, :invalid_url] do
      Application.put_env(:core, :news_lookup_test_mode, mode)
      assert {:ok, answer, meta} = news(text)
      assert answer =~ "returned no usable, dated headlines"
      assert meta.outcome == :no_usable_headlines
      assert meta.source_count == 0
      assert meta.external_facts_verified? == false
      assert_receive {:news_lookup_query, _query}
    end
  end

  test "disabled retrieval, unsafe actions and non-user sources never call providers" do
    text = "what do you think if the U.S. iranian war"
    Application.put_env(:core, :web_retrieval, [enabled?: false, news_provider: FakeNews])

    assert :not_applicable == news(text)
    refute_receive {:news_lookup_query, _}

    Application.put_env(:core, :web_retrieval, [enabled?: true, news_provider: FakeNews])
    si = input(text)

    for {features, decision, candidate} <- [
      {%{risk_bucket: :high}, %{action: :answer}, si},
      {%{risk_bucket: :low}, %{action: :execute}, si},
      {%{risk_bucket: :low}, %{action: :answer}, %{si | source: :assistant}},
      {%{risk_bucket: :low}, %{action: :answer}, %{si | sentence: "different text"}}
    ] do
      assert :not_applicable == NewsLookup.generate(text, features, decision, candidate)
    end

    refute_receive {:news_lookup_query, _}
  end

  test "news retrieval doesn't trigger for personal, hypothetical, historical or ordinary turns" do
    for text <- [
      "How are you today?",
      "The bird is flying into the tree.",
      "What do you think about my family war?",
      "What do you think about a fictional US Iran war?",
      "What do you think of the Russia Ukraine war in 2021?",
      "What is your name?"
    ] do
      assert :not_applicable == news(text), text
    end

    refute_receive {:news_lookup_query, _}
  end
end
