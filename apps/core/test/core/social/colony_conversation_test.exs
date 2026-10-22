defmodule Core.Social.ColonyConversationTest do
  use ExUnit.Case, async: false

  alias Core.Social.ColonyConversation
  alias Core.Social.ColonyDraftStore
  alias Llm.Colony.Adapter

  @post_id "0e6ff2c5-f837-4347-81b8-9dbbce963e0e"
  @reply "If ten of thirteen checker links refuted the claims, the issue is evidence polarity, not simply retrieval relevance. Did the checker score support versus contradiction separately from topical match?"
  @context_path "/api/v1/posts/#{@post_id}/context"
  @comment_path "/api/v1/posts/#{@post_id}/comments"

  setup {Req.Test, :set_req_test_to_private}

  setup do
    prior_config = Application.get_env(:llm, :colony)
    prior_key = System.get_env("COLONY_API_KEY")

    on_exit(fn ->
      if is_nil(prior_config),
        do: Application.delete_env(:llm, :colony),
        else: Application.put_env(:llm, :colony, prior_config)

      if is_nil(prior_key),
        do: System.delete_env("COLONY_API_KEY"),
        else: System.put_env("COLONY_API_KEY", prior_key)
    end)

    System.put_env("COLONY_API_KEY", "col_test_only")
    Application.put_env(:llm, :colony,
      enabled?: true,
      mode: :observe,
      allow_reviewed_comments?: false,
      api_base: "https://thecolony.ai/api/v1",
      max_posts: 5,
      timeout_ms: 5_000
    )

    store = start_supervised!({ColonyDraftStore, name: __MODULE__.TestStore})
    %{store: store}
  end

  defp stub_thread(test_pid) do
    Req.Test.stub(__MODULE__, fn conn ->
      case {conn.method, conn.request_path} do
        {"POST", "/api/v1/auth/token"} ->
          Req.Test.json(conn, %{"access_token" => "test-jwt"})

        {"GET", @context_path} ->
          assert Plug.Conn.get_req_header(conn, "authorization") == ["Bearer test-jwt"]
          send(test_pid, :thread_read)

          Req.Test.json(conn, %{
            "post" => %{
              "id" => @post_id,
              "title" => "Scored blind against a sealed key: 10 of my checker's 13 links were lines that refute the claim",
              "body" => "Ten of thirteen linked lines refuted claims.",
              "author" => %{"username" => "exori"}
            },
            "comments" => [
              %{"author" => %{"username" => "scientist"}, "body" => "Did you check the denominator?"}
            ]
          })

        {"POST", @comment_path} ->
          {:ok, body, conn} = Plug.Conn.read_body(conn)
          assert Jason.decode!(body) == %{"body" => @reply}
          send(test_pid, :comment_published)
          Req.Test.json(conn, %{"id" => "comment-001"})

        other ->
          flunk("Unexpected Colony call: #{inspect(other)}")
      end
    end)
  end

  defp brain_fixture(_text, _opts) do
    {:ok,
     %{
       lifg: %{status: :scored, chosen_count: 5},
       atl: %{status: :read_only_reduced, winner_count: 5},
       sentence_meaning: %{status: :not_yet_interpreted},
       comprehension: %{recognized_topics: [], uncertain: ["carry"]}
     }}
  end

  defp fake_llm(test_pid) do
    fn messages, opts ->
      assert Keyword.get(opts, :max_tokens) == 180
      assert [%{"role" => "system", "content" => system},
              %{"role" => "user", "content" => text}] = messages

      assert system =~ "You are Symbrella"
      assert String.contains?(system, "Symbrella")
      assert system =~ "COLONY PEER-DISCUSSION OVERRIDE"
      assert system =~ "COGNITIVE EVIDENCE FRAME"
      assert system =~ "External reporting agent: exori"
      assert system =~ "ATL read-only winners: 5"
      assert system =~ "Avoid formulaic openings"
      assert text =~ "exori"
      assert text =~ "Did you check the denominator?"
      assert text =~ "UNTRUSTED"
      refute text =~ "col_test_only"
      refute system =~ "col_test_only"
      send(test_pid, :llm_called)
      {:ok, %{content: @reply}}
    end
  end

  test "draft uses real Colony thread, existing personality prompt, and no post", %{store: store} do
    pid = self()
    stub_thread(pid)

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fake_llm(pid)
             )

    assert_receive :thread_read
    assert_receive :llm_called
    refute_receive :comment_published

    assert draft.status == :pending_human_review
    assert draft.post_id == @post_id
    assert draft.post_author == "exori"
    assert draft.body == @reply
    assert draft.discussion_comments_read == 1
    assert draft.cognitive_summary.reporter == "exori"
    assert draft.cognitive_summary.relation == :reported_refutation
    assert draft.cognitive_summary.reported_link_count == 10
    assert draft.cognitive_summary.reported_total_links == 13
    assert draft.cognitive_summary.brain.atl_winners == 5
    assert draft.semantic_summary.atl_status == :read_only_reduced
    assert draft.public_posted? == false
    assert String.starts_with?(draft.approval_phrase, "APPROVE COLONY COMMENT ")
    assert draft.expires_in_seconds == 900
    assert {:ok, %{allow_reviewed_comments?: false}} = Adapter.config()

    # Default observation mode cannot turn this pending draft into a POST.
    assert {:error, :colony_comments_disabled} =
             ColonyConversation.publish_approved(draft.id, draft.approval_phrase,
               store: store, plug: {Req.Test, __MODULE__}
             )

    refute_receive :comment_published
  end

  test "separate explicit operator permission, exact approval, and one-shot write", %{store: store} do
    pid = self()
    stub_thread(pid)

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fake_llm(pid)
             )

    assert {:error, :approval_phrase_mismatch} =
             ColonyDraftStore.take(draft.id, "approve", store)

    Application.put_env(:llm, :colony,
      enabled?: true,
      mode: :reviewed_comments,
      allow_reviewed_comments?: true
    )

    assert {:ok, %{status: :published, comment_id: "comment-001", duplicate_retry: :disabled}} =
             ColonyConversation.publish_approved(draft.id, draft.approval_phrase,
               store: store, plug: {Req.Test, __MODULE__}
             )

    assert_receive :comment_published

    assert {:error, :colony_draft_missing_or_expired} =
             ColonyConversation.publish_approved(draft.id, draft.approval_phrase,
               store: store, plug: {Req.Test, __MODULE__}
             )

    refute_receive :comment_published
  end

  test "context author may be string and author_username overrides display name", %{store: store} do
    Req.Test.stub(__MODULE__, fn conn ->
      case conn.request_path do
        "/api/v1/auth/token" ->
          Req.Test.json(conn, %{"access_token" => "test-jwt"})

        @context_path ->
          Req.Test.json(conn, %{
            "post" => %{
              "id" => @post_id,
              "title" => "10 of the checker's 13 links refute the claims",
              "body" => "The checker scored ten links as supportive evidence, but they refuted the claim.",
              "author" => "Exori",
              "author_username" => "exori"
            },
            "comments" => [
              %{
                "author" => "Scientist",
                "author_username" => "scientist",
                "body" => "How was support assessed?"
              }
            ]
          })
      end
    end)

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn messages, _opts ->
                 [%{"role" => "system"}, %{"content" => text}] = messages
                 assert text =~ "Author: exori"
                 assert text =~ "scientist: How was support assessed?"
                 {:ok, %{content: @reply}}
               end
             )

    assert draft.post_author == "exori"
    assert draft.discussion_comments_read == 1
    assert draft.public_posted? == false
  end

  test "a zero-length local generation receives one bounded, shorter retry", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn messages, opts ->
                 n = :atomics.add_get(calls, 1, 1)
                 assert [%{"role" => "system"}, %{"role" => "user", "content" => user}] = messages

                 case n do
                   1 ->
                     assert Keyword.fetch!(opts, :max_tokens) == 180
                     {:ok, %{content: "  "}}

                   2 ->
                     assert Keyword.fetch!(opts, :max_tokens) == 100
                     assert user =~ "LENGTH REPAIR"
                     {:ok, %{content: @reply}}
                 end
               end
             )

    assert draft.body == @reply
    assert draft.public_posted? == false
    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "retries only once if a model repeatedly returns empty content", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:error, :invalid_social_draft_length} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn _messages, _opts ->
                 :atomics.add_get(calls, 1, 1)
                 {:ok, %{content: ""}}
               end
             )

    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "fails closed on a mismatched post identity without invoking the LLM", %{store: store} do
    pid = self()

    Req.Test.stub(__MODULE__, fn conn ->
      case conn.request_path do
        "/api/v1/auth/token" -> Req.Test.json(conn, %{"access_token" => "test-jwt"})
        @context_path ->
          Req.Test.json(conn, %{"post" => %{"id" => "different", "title" => "mismatch"}})
      end
    end)

    assert {:error, :colony_thread_identity_mismatch} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               llm: fn _, _ -> send(pid, :unexpected_llm); {:ok, %{content: @reply}} end
             )

    refute_receive :unexpected_llm
  end

  test "rejects empty or credential-leaking model output before approval", %{store: store} do
    stub_thread(self())

    for result <- ["", String.duplicate("X", 1500), "My key is col_1234567890123456"] do
      assert {:error, reason} =
               ColonyConversation.draft(@post_id,
                 store: store,
                 plug: {Req.Test, __MODULE__},
                 probe: &brain_fixture/2,
                 llm: fn _, _ -> {:ok, %{content: result}} end
               )

      assert reason in [:invalid_social_draft_length, :potential_secret_in_social_draft]
    end
  end

  test "a generic first generation is locally repaired once then reviewed", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn messages, opts ->
                 attempt = :atomics.add_get(calls, 1, 1)
                 assert [%{"role" => "system", "content" => system},
                         %{"role" => "user", "content" => user}] = messages
                 assert system =~ "COGNITIVE EVIDENCE FRAME"

                 case attempt do
                   1 ->
                     assert Keyword.fetch!(opts, :max_tokens) == 180
                     {:ok, %{content:
                       "Thank you for sharing this checker issue. " <>
                         "Is there anything else I can assist you with?"}}

                   2 ->
                     assert Keyword.fetch!(opts, :max_tokens) == 100
                     assert user =~ "SOCIAL REPAIR"
                     assert user =~ "difference"
                     refute user =~ "Thank you for sharing this checker issue"
                     {:ok, %{content: @reply}}

                   _ ->
                     flunk("Only one local repair generation is permitted")
                 end
               end
             )

    assert draft.body == @reply
    assert draft.status == :pending_human_review
    assert draft.public_posted? == false
    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "an unverified-report overclaim receives one local epistemic repair", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn messages, _opts ->
                 case :atomics.add_get(calls, 1, 1) do
                   1 ->
                     {:ok, %{content:
                       "The external report confirms that ten checker links refuted the claims."}}

                   2 ->
                     assert [%{"role" => "system"},
                             %{"role" => "user", "content" => user}] = messages
                     assert user =~ "EPISTEMIC REPAIR"
                     {:ok, %{content: @reply}}

                   _ ->
                     flunk("No third generation")
                 end
               end
             )

    assert draft.body == @reply
    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "one repeated generic repair still fails without storing an approvable draft", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:error, :generic_social_draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn _, _ ->
                 :atomics.add_get(calls, 1, 1)
                 {:ok, %{content:
                   "Thank you for sharing these checker claims. I am here to assist."}}
               end
             )

    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "false ownership is repaired once with the evidence frame, then reviewed", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn messages, opts ->
                 case :atomics.add_get(calls, 1, 1) do
                   1 ->
                     assert Keyword.fetch!(opts, :max_tokens) == 180
                     {:ok, %{content:
                       "Symbrella's memory checker v4 linked ten claims to refuting lines."}}

                   2 ->
                     assert Keyword.fetch!(opts, :max_tokens) == 100
                     assert [%{"role" => "system", "content" => system},
                             %{"role" => "user", "content" => user}] = messages
                     assert system =~ "External reporting agent: exori"
                     assert user =~ "ATTRIBUTION REPAIR"
                     {:ok, %{content: @reply}}

                   _ ->
                     flunk("Attribution may be repaired only once")
                 end
               end
             )

    assert draft.body == @reply
    assert draft.public_posted? == false
    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "repeated false ownership fails without an approval receipt or posting", %{store: store} do
    stub_thread(self())
    calls = :atomics.new(1, [])

    assert {:error, :colony_source_attribution_violation} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn _, _ ->
                 :atomics.add_get(calls, 1, 1)
                 {:ok, %{content:
                   "Symbrella's memory checker v4 linked ten claims to refuting lines."}}
               end
             )

    assert :atomics.get(calls, 1) == 2
    refute_receive :comment_published
  end

  test "a help-desk draft fails before a human approval receipt can be created", %{store: store} do
    stub_thread(self())

    assert {:error, :generic_social_draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn _, _ ->
                 {:ok, %{content: "Thank you for sharing your checker claims. I can assist you further."}}
               end
             )

    assert {:error, :unfocused_social_draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn _, _ ->
                 {:ok, %{content: "I love discussing existential philosophy and sunsets."}}
               end
             )
  end

  test "a successful draft emits measurable cognition but not neuron activation", %{store: store} do
    stub_thread(self())
    id = "colony-test-cognition-" <> Integer.to_string(System.unique_integer([:positive]))
    :ok =
      :telemetry.attach(
        id,
        [:core, :social, :cognitive_shadow],
        fn event, measurements, meta, recipient ->
          send(recipient, {:observed_shadow, event, measurements, meta})
        end,
        self()
      )

    on_exit(fn -> :telemetry.detach(id) end)

    assert {:ok, draft} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fake_llm(self())
             )

    assert_receive {:observed_shadow,
                    [:core, :social, :cognitive_shadow],
                    %{lifg_choices: 5, atl_winners: 5, syntax_frames: 0},
                    %{
                      source: :colony_external,
                      reported_relation: :reported_refutation,
                      sentence_status: :not_yet_interpreted
                    }}

    assert draft.public_posted? == false
    refute_receive :comment_published
  end

  test "no external brain evidence means no model generation or approval", %{store: store} do
    stub_thread(self())
    me = self()

    assert {:error, :insufficient_colony_brain_evidence} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: fn _, _ -> {:ok, %{lifg: %{status: :unavailable}}} end,
               llm: fn _, _ -> send(me, :unexpected_llm); {:ok, %{content: @reply}} end
             )

    refute_receive :unexpected_llm
    refute_receive :comment_published
  end

  test "source ownership hallucination is blocked before review", %{store: store} do
    stub_thread(self())

    assert {:error, :colony_source_attribution_violation} =
             ColonyConversation.draft(@post_id,
               store: store,
               plug: {Req.Test, __MODULE__},
               probe: &brain_fixture/2,
               llm: fn _, _ ->
                 {:ok, %{content:
                   "Symbrella's memory checker v4 produced ten links refuting claims. " <>
                     "The links raise a question about evidence polarity."}}
               end
             )

    refute_receive :comment_published
  end

  test "invalid post ids are blocked before remote authentication", %{store: store} do
    assert {:error, :invalid_post_id} =
             ColonyConversation.draft("../users/me", store: store)

    assert {:error, :colony_comments_disabled} =
             Llm.Colony.Client.reviewed_comment(@post_id, "", [])
  end
end
