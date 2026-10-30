defmodule SymbrellaWeb.SocialLiveTest do
  use SymbrellaWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.SocialData

  # Only read APIs exist in this fake: a regression would fail immediately
  # if a browser action tried to post a comment or generate a draft.
  defmodule FakeClient do
    def id(n), do: "00000000-0000-4000-8000-" <> String.pad_leading(Integer.to_string(n), 12, "0")

    def posts(opts) do
      limit = Keyword.fetch!(opts, :limit)
      offset = Keyword.fetch!(opts, :offset)
      sort = Keyword.fetch!(opts, :sort)

      if limit == 25 and sort == :newest and offset in [0, 25] do
        count = if offset == 0, do: 25, else: 3

        items =
          for n <- 1..count do
            i = n + offset

            %{
              "id" => id(i),
              "title" => "Discussion topic #{i}",
              "body" => "A reported conversation about case #{i}.",
              "author" => %{"username" => "agent_#{i}"},
              "colony" => %{"name" => "engineering"},
              "created_at" => "2026-10-23T12:00:00Z"
            }
          end

        {:ok, %{"posts" => items, "has_more" => offset == 0}}
      else
        {:error, :invalid_offset}
      end
    end

    def thread_context(post_id) do
      {:ok,
       %{
         "post" => %{
           "id" => post_id,
           "title" => "Discussion topic 1",
           "body" => "This is the original post. It includes\nmultiple lines.",
           "author_username" => "agent_1",
           "colony_name" => "engineering",
           "created_at" => "2026-10-23T12:00:00Z"
         },
         "comments" => %{
           "items" => [
             %{
               "id" => "reply-1",
               "author_username" => "agent_a",
               "body" => "First reply",
               "created_at" => "2026-10-23T12:01:00Z"
             },
             %{
               "id" => "reply-2",
               "author" => %{"username" => "agent_b"},
               "body" => "Second reply",
               "parent_comment_id" => "reply-1",
               "created_at" => "2026-10-23T12:02:00Z"
             }
           ]
         }
       }}
    end
  end

  setup do
    previous = Application.get_env(:symbrella_web, :social_client)
    Application.put_env(:symbrella_web, :social_client, FakeClient)

    on_exit(fn ->
      if is_nil(previous) do
        Application.delete_env(:symbrella_web, :social_client)
      else
        Application.put_env(:symbrella_web, :social_client, previous)
      end
    end)

    :ok
  end

  test "bounded newest-first pages have exactly 25 topics and support next/previous" do
    assert {:ok, first} = SocialData.list_page(1, client: FakeClient)
    assert length(first.topics) == 25
    assert first.has_next?
    assert Enum.at(first.topics, 0).title == "Discussion topic 1"
    assert Enum.at(first.topics, 0).author == "agent_1"

    assert {:ok, second} = SocialData.list_page(2, client: FakeClient)
    assert length(second.topics) == 3
    refute second.has_next?
    assert hd(second.topics).title == "Discussion topic 26"
    assert {:error, :invalid_social_page} = SocialData.list_page(0, client: FakeClient)
    assert {:error, :invalid_social_page} = SocialData.list_page(4_002, client: FakeClient)
  end

  test "selected topic uses only its context endpoint and keeps replies as external data" do
    assert {:ok, %{topic: topic, comments: comments, source: :colony, trust: :untrusted_external}} =
             SocialData.thread(FakeClient.id(1), client: FakeClient)

    assert topic.title == "Discussion topic 1"
    assert topic.author == "agent_1"
    assert topic.body =~ "multiple lines"
    assert Enum.map(comments, & &1.author) == ["agent_a", "agent_b"]
    assert Enum.map(comments, & &1.body) == ["First reply", "Second reply"]
    assert List.last(comments).parent_id == "reply-1"
    assert {:error, :invalid_post_id} = SocialData.thread("not-a-post-id", client: FakeClient)
  end

  test "renders the social page and loads the next 25 without posting", %{conn: conn} do
    {:ok, view, html} = live(conn, ~p"/social")

    assert html =~ "Social"
    assert has_element?(view, "#social-list")
    assert has_element?(view, "#social-topic-#{FakeClient.id(1)}")
    assert has_element?(view, "#social-topic-#{FakeClient.id(25)}")
    refute has_element?(view, "#social-topic-#{FakeClient.id(26)}")
    assert has_element?(view, "#social-next")
    refute has_element?(view, "#social-previous")

    html = render_patch(view, ~p"/social?page=2")
    assert html =~ "Page 2"
    assert has_element?(view, "#social-previous")
    refute has_element?(view, "#social-next")
    assert has_element?(view, "#social-topic-#{FakeClient.id(26)}")
  end

  test "clicking a topic shows its whole returned discussion and navigation back", %{conn: conn} do
    {:ok, view, html} = live(conn, ~p"/social/#{FakeClient.id(1)}?page=2")

    assert html =~ "Conversation"
    assert has_element?(view, "#social-post")
    assert has_element?(view, "#social-replies-count")
    assert has_element?(view, "#social-comment-0")
    assert has_element?(view, "#social-comment-1")
    assert html =~ "First reply"
    assert html =~ "Second reply"
    assert html =~ "Reply to another comment"
    assert html =~ "/social?page=2"
    assert has_element?(view, "#social-refresh")
  end

  defmodule FakeCognitiveAnalyzer do
    def analyze(%{topic: topic, comments: comments}) do
      {:ok, %{
        post_id: topic.id,
        analyzed_segments: 1,
        omitted_replies: length(comments),
        conversation: %{
          subject: %{author_title: topic.title, status: :author_title_only, lexical_topic_candidates: []},
          participants: [%{source_label: "Original post", speaker: topic.author,
            status: :meaning_unresolved, interpretations: []}],
          relations: [],
          gaps: ["No supported sentence interpretation."]
        },
        segments: [%{
          label: "Original post",
          author: topic.author,
          status: :analyzed,
          result: %{
            lifg: %{status: :scored, chosen_count: 2},
            atl: %{status: :read_only_reduced, winner_count: 1},
            sentence_meaning: %{status: :external_unverified},
            syntax: %{audit: %{outcome: :grammar_candidates_detected, pos_covered_tokens: 2}, signatures: [%{id: :noun_phrase}]},
            token_count: 3,
            acc_shadow: %{uncertain?: true},
            comprehension: %{recognized_topics: ["growth"]},
            reported_proposition: %{status: :not_interpreted}
          }
        }]
      }}
    end
  end

  defmodule FakeSourceAwareAnalyzer do
    def analyze(%{topic: topic, comments: comments} = input) do
      {:ok, base} = SymbrellaWeb.SocialLiveTest.FakeCognitiveAnalyzer.analyze(input)
      original = base.segments |> hd() |> Map.merge(%{
        sample_index: 0, source_type: :original_post, source_index: 0
      })

      first_reply = hd(comments)
      reply = original |> Map.merge(%{
        sample_index: 1, source_type: :reply, source_index: 1,
        label: "Reply 1 · sentence 1", author: first_reply.author
      })

      {:ok,
        base
        |> Map.put(:analyzed_segments, 2)
        |> Map.put(:segments, [original, reply])
        |> Map.put(:sections, %{
          original_post: %{
            author: topic.author, segments: [original],
            understanding: %{status: :meaning_unresolved},
            discourse: %{interpretations: []}
          },
          replies: [%{
            index: 1, label: "Reply 1", author: first_reply.author,
            segments: [reply], understanding: %{status: :meaning_unresolved},
            discourse: %{interpretations: []}
          }]
        })}
    end
  end

  test "cognitive modal separates original post from independent reply reports", %{conn: conn} do
    previous = Application.get_env(:symbrella_web, :social_cognitive_analyzer)
    Application.put_env(:symbrella_web, :social_cognitive_analyzer, FakeSourceAwareAnalyzer)

    try do
      {:ok, view, _} = live(conn, ~p"/social/#{FakeClient.id(1)}")
      view |> element("#social-analyze") |> render_click()

      assert has_element?(view, "#social-analysis-original-post")
      assert has_element?(view, "#social-analysis-replies")
      assert has_element?(view, "#social-speaker-original")
      assert has_element?(view, "#social-speaker-replies")
      assert has_element?(view, "#social-analysis-reply-1")

      assert has_element?(view, "#social-analysis-original-post #social-analysis-segment-0")
      assert has_element?(view, "#social-analysis-reply-1 #social-analysis-segment-1")
      refute has_element?(view, "#social-analysis-original-post #social-analysis-segment-1")
      refute has_element?(view, "#social-analysis-replies #social-analysis-segment-0")
    after
      if is_nil(previous),
        do: Application.delete_env(:symbrella_web, :social_cognitive_analyzer),
        else: Application.put_env(:symbrella_web, :social_cognitive_analyzer, previous)
    end
  end

  defmodule FakeOptInCognitiveAnalyzer do
    # Deliberately expose ONLY analyze/2. The LiveView must ensure the
    # module is loaded before checking exported arities.
    def analyze(input, opts) do
      send(Application.fetch_env!(:symbrella_web, :social_test_observer),
        {:social_brain_optin, Keyword.get(opts, :brain_observation?)})
      SymbrellaWeb.SocialLiveTest.FakeCognitiveAnalyzer.analyze(input)
    end
  end

  test "explicit Analyze cognition opts into live Brain observation", %{conn: conn} do
    prior_analyzer = Application.get_env(:symbrella_web, :social_cognitive_analyzer)
    prior_observer = Application.get_env(:symbrella_web, :social_test_observer)

    Application.put_env(:symbrella_web, :social_cognitive_analyzer, FakeOptInCognitiveAnalyzer)
    Application.put_env(:symbrella_web, :social_test_observer, self())

    try do
      {:ok, view, _} = live(conn, ~p"/social/#{FakeClient.id(1)}")
      refute_receive {:social_brain_optin, _}, 30

      view |> element("#social-analyze") |> render_click()
      assert_receive {:social_brain_optin, true}
    after
      if is_nil(prior_analyzer),
        do: Application.delete_env(:symbrella_web, :social_cognitive_analyzer),
        else: Application.put_env(:symbrella_web, :social_cognitive_analyzer, prior_analyzer)

      if is_nil(prior_observer),
        do: Application.delete_env(:symbrella_web, :social_test_observer),
        else: Application.put_env(:symbrella_web, :social_test_observer, prior_observer)
    end
  end

  test "on-demand cognitive modal displays read-only stage results and can close", %{conn: conn} do
    previous = Application.get_env(:symbrella_web, :social_cognitive_analyzer)
    Application.put_env(:symbrella_web, :social_cognitive_analyzer, FakeCognitiveAnalyzer)

    try do
      {:ok, view, _} = live(conn, ~p"/social/#{FakeClient.id(1)}")
      assert has_element?(view, "#social-analyze")
      refute has_element?(view, "#social-cognitive-modal")

      html = view |> element("#social-analyze") |> render_click()
      assert html =~ "Cognitive inspection"
      assert has_element?(view, "#social-analysis-segment-0")
      assert has_element?(view, "#social-copy-analysis[phx-hook='ClipboardCopy']")
      assert has_element?(view, "#social-report-content.overflow-y-auto")
      assert has_element?(view, "#social-cognitive-modal[role='dialog']")
      assert has_element?(view, "#social-conversation-evidence")
      assert has_element?(view, "#social-topic-analysis")
      assert has_element?(view, "#social-speaker-analysis")
      assert has_element?(view, "#social-relation-analysis")
      assert %{} = SymbrellaWeb.SocialBrainActivity.latest()

      assert html =~ "grammar_candidates_detected"
      assert html =~ "read_only_reduced"

      view |> element("#social-close-analysis") |> render_click()
      refute has_element?(view, "#social-cognitive-modal")
    after
      if is_nil(previous), do: Application.delete_env(:symbrella_web, :social_cognitive_analyzer),
        else: Application.put_env(:symbrella_web, :social_cognitive_analyzer, previous)
    end
  end

  test "rejects invalid thread identifier without sending a request", %{conn: conn} do
    {:ok, view, _html} = live(conn, "/social/not-a-valid-id")
    assert has_element?(view, "#social-error")
    assert render(view) =~ "Invalid Colony topic identifier"
  end
end
