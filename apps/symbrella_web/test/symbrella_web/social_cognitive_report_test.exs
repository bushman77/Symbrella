defmodule SymbrellaWeb.SocialCognitiveReportTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.SocialCognitiveReport

  test "bounds external segments and uses only the isolated probe callback" do
    topic = %{id: "00000000-0000-4000-8000-000000000001",
      title: "How can a community grow?", body: String.duplicate("a", 2_000),
      author: "colony_agent"}
    comments = for i <- 1..8, do: %{author: "reply_#{i}", body: "reply #{i}"}

    probe = fn text, opts ->
      assert String.length(text) <= 600
      assert opts[:emit_telemetry?] == false
      {:ok, %{stage: :social_semantic_shadow, marker: text}}
    end

    assert {:ok, report} =
      SocialCognitiveReport.analyze(%{topic: topic, comments: comments}, probe: probe)

    assert report.mode == :read_only_shadow
    assert report.trust == :untrusted_external
    assert report.analyzed_segments == 13
    assert report.omitted_replies == 0
    assert report.discourse.status == :no_supported_sentence_meanings
    assert report.discourse.interpretations == []
    assert report.conversation.subject.author_title == "How can a community grow?"
    assert Enum.all?(report.conversation.participants, &(&1.status == :meaning_unresolved))
    # Four ordering links connect five original-post segments. These
    # establish source order only, never agreement or semantic meaning.
    assert length(report.conversation.relations) == 4
    assert Enum.all?(report.conversation.relations, fn relation ->
      relation.kind == :same_source_sentence_order and relation.status == :ordering_only
    end)
    assert Enum.all?(report.segments, &(&1.status == :analyzed))
    refute Enum.any?(report.segments, &Map.has_key?(&1, :text))
  end

  test "original post and replies retain separate lexical provenance and meanings" do
    topic = %{id: "post-1", title: "What would you do if you were free?",
      body: "Freedom can mean choice.", author: "same_agent"}

    comments = [
      %{id: "reply-1", author: "same_agent",
        body: "I would read books. I would learn."},
      %{id: "reply-2", author: "another_agent",
        body: "I would explore."}
    ]

    probe = fn text, opts ->
      assert opts[:brain_observation?] == false
      {:ok, %{
        stage: :social_semantic_shadow,
        marker: text,
        sentence_meaning: %{
          status: :structurally_interpreted,
          speech_act: :request_information,
          definition: text
        },
        comprehension: %{recognized_topics: [text]}
      }}
    end

    assert {:ok, report} =
      SocialCognitiveReport.analyze(%{topic: topic, comments: comments}, probe: probe)

    assert length(report.segments) == 5
    assert length(report.sections.original_post.segments) == 2
    assert Enum.map(report.sections.replies, &length(&1.segments)) == [2, 1]

    assert Enum.map(report.segments, &{&1.source_type, &1.source_index}) ==
      [{:original_post, 0}, {:original_post, 0},
       {:reply, 1}, {:reply, 1}, {:reply, 2}]

    assert Enum.map(report.sections.original_post.segments, & &1.sample_index) == [0, 1]
    assert Enum.map(hd(report.sections.replies).segments, & &1.sample_index) == [2, 3]
    assert Enum.all?(report.sections.original_post.segments,
      &String.starts_with?(&1.label, "Original post"))

    assert report.sections.original_post.understanding.interpreted_segments == 2
    assert Enum.map(report.sections.replies, & &1.understanding.interpreted_segments) == [2, 1]
    assert length(report.sections.original_post.discourse.interpretations) == 2
    assert Enum.map(report.sections.replies, &length(&1.discourse.interpretations)) == [2, 1]
    assert report.sections.original_post.author == "same_agent"

    # Same author across two sources must not produce fake intra-source
    # lexical links from the post to the reply.
    refute Enum.any?(report.discourse.lexical_links, fn link ->
      from = Enum.at(report.segments, link.from)
      to = Enum.at(report.segments, link.to)
      {from.source_type, from.source_index} != {to.source_type, to.source_index}
    end)

    assert report.conversation.subject.lexical_topic_candidates ==
      ["What would you do if you were free?", "Freedom can mean choice."]
  end

  test "the original-post button probes title/body but no replies" do
    topic = %{id: "post-1", title: "What if?", body: "Only the post belongs here.",
      author: "author"}
    comments = [%{author: "reply_one", body: "Never include this reply."}]
    caller = self()

    probe = fn text, opts ->
      send(caller, {:probed, text, opts[:brain_observation?]})
      {:ok, %{stage: :social_semantic_shadow}}
    end

    assert {:ok, report} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: comments},
      probe: probe, target: :original_post, brain_observation?: true
    )

    assert report.target.kind == :original_post
    assert length(report.segments) == 2
    assert report.sections.replies == []
    assert report.sections.original_post.author == "author"
    assert Map.has_key?(report, :conversation)
    assert_receive {:probed, "What if?", true}
    assert_receive {:probed, "Only the post belongs here.", true}
    refute_receive {:probed, _, _}
  end

  test "each reply is probed independently, including replies beyond the thread cap" do
    topic = %{id: "post-1", title: "Do not probe the post?", body: "Not this.",
      author: "same_author"}
    comments = for i <- 1..12, do: %{
      author: if(i == 11, do: "same_author", else: "agent_#{i}"),
      body: "Reply #{i} text."
    }
    caller = self()

    probe = fn text, opts ->
      send(caller, {:probed, text, opts[:brain_observation?]})
      {:ok, %{stage: :social_semantic_shadow}}
    end

    assert {:ok, report} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: comments},
      probe: probe, target: {:reply, 10}, brain_observation?: true
    )

    assert report.target == %{
      kind: :reply, label: "Reply 11", index: 11, author: "same_author"
    }
    assert [%{source_type: :reply, source_index: 11, author: "same_author"}] =
      report.segments
    assert report.sections.original_post.segments == []
    assert [%{index: 11, author: "same_author"}] = report.sections.replies
    refute Map.has_key?(report, :conversation)
    assert report.omitted_replies == 0
    assert_receive {:probed, "Reply 11 text.", true}
    refute_receive {:probed, _, _}

    assert {:ok, other} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: comments},
      probe: probe, target: {:reply, 0}
    )
    assert other.target.index == 1
    assert_receive {:probed, "Reply 1 text.", false}
    refute_receive {:probed, _, _}
  end

  test "invalid source targets reject analysis without invoking the probe" do
    topic = %{id: "post-1", title: "Question?", body: "", author: "author"}
    comments = [%{author: "agent", body: "Response."}]
    probe = fn _, _ -> flunk("invalid selection was probed") end

    for target <- [{:reply, -1}, {:reply, 1}, {:reply, 99}, :unknown, {:reply, "0"}] do
      assert {:error, :invalid_social_analysis_target} =
        SocialCognitiveReport.analyze(
          %{topic: topic, comments: comments}, probe: probe, target: target
        )
    end
  end

  test "individual post inspects more than twelve sentences without mixing replies" do
    topic = %{
      id: "post-more-than-12",
      title: "One post?",
      body: Enum.map_join(1..14, " ", fn i -> "Part #{i} continues." end),
      author: "speaker"
    }
    comments = [%{author: "other", body: "Reply must not be inspected."}]
    caller = self()

    probe = fn text, _opts ->
      send(caller, {:examined, text})
      {:ok, %{stage: :social_semantic_shadow}}
    end

    assert {:ok, report} =
      SocialCognitiveReport.analyze(%{topic: topic, comments: comments},
        target: :original_post, probe: probe)

    assert report.analyzed_segments == 15
    assert report.omitted_sentences == 0
    assert report.sections.replies == []
    assert length(report.sections.original_post.segments) == 15
    refute Enum.any?(report.segments, &(&1.source_type == :reply))

    for _ <- 1..15, do: assert_receive({:examined, _})
    refute_receive {:examined, "Reply must not be inspected."}
  end

  test "operator can opt into transient Brain stimulation, never by default" do
    topic = %{
      id: "00000000-0000-4000-8000-000000000001",
      title: "What would you do if you were free?",
      body: "",
      author: "observer"
    }

    caller = self()

    probe = fn _text, opts ->
      send(caller, {:brain_observation_opt, opts[:brain_observation?]})
      {:ok, %{stage: :social_semantic_shadow}}
    end

    assert {:ok, _} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: []},
      probe: probe
    )

    assert_receive {:brain_observation_opt, false}

    assert {:ok, _} = SocialCognitiveReport.analyze(
      %{topic: topic, comments: []},
      probe: probe,
      brain_observation?: true
    )

    assert_receive {:brain_observation_opt, true}
  end

  test "keeps unavailable probes distinct from successful semantic results" do
    topic = %{id: "id", title: "?", body: "", author: "agent"}
    probe = fn _, _ -> {:error, :lookup_failed} end

    assert {:ok, %{segments: [%{status: :unavailable, reason: :lookup_failed}]}} =
      SocialCognitiveReport.analyze(%{topic: topic, comments: []}, probe: probe)
  end
end
