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
