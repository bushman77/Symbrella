defmodule SymbrellaWeb.SocialConversationTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.SocialConversation

  test "attributes supported sentence meanings and distinguishes parent links from lexical overlap" do
    topic = %{id: "post-1", title: "Reasons can change", author: "agent_a"}
    comments = [
      %{id: "c1", author: "agent_b", parent_id: "post-1"},
      %{id: "c2", author: "agent_c", parent_id: "c1"}
    ]

    segments = [
      %{label: "Original post · sentence 1", author: "agent_a",
        result: %{comprehension: %{recognized_topics: ["reason", "date"]}}},
      %{label: "Reply 1 · sentence 1", author: "agent_b",
        result: %{comprehension: %{recognized_topics: ["reason"]}}},
      %{label: "Reply 2 · sentence 1", author: "agent_c", result: %{}}
    ]

    discourse = %{
      interpretations: [
        %{source_label: "Original post · sentence 1", speaker: "agent_a",
          speech_act: :assertion, interpretation: "A reason was recorded.", roles: %{}},
        %{source_label: "Reply 1 · sentence 1", speaker: "agent_b",
          speech_act: :request_information, interpretation: "A question was asked.", roles: %{}}
      ],
      lexical_links: [
        %{from: 0, to: 1, shared_terms: ["reason"]},
        %{from: -1, to: 0, shared_terms: ["should_not_appear"]}
      ]
    }

    report = SocialConversation.build(topic, comments, segments, discourse)

    assert report.subject.author_title == "Reasons can change"
    assert report.subject.lexical_topic_candidates == ["reason", "date"]
    assert Enum.map(report.participants, & &1.status) ==
             [:partial_interpretation, :partial_interpretation, :meaning_unresolved]

    assert Enum.at(report.participants, 0).interpretations |> hd() |> Map.fetch!(:interpretation) ==
             "A reason was recorded."

    assert Enum.count(report.relations, &(&1.kind == :explicit_reply_parent)) == 2
    assert Enum.count(report.relations, &(&1.kind == :shared_lexical_terms)) == 1

    assert Enum.all?(report.relations, fn edge ->
      edge.status in [:structural_only, :unverified_link]
    end)
    refute inspect(report.relations) =~ "should_not_appear"
  end

  test "reply-free posts retain sentence ordering without inventing causality" do
    topic = %{id: "post-1", title: "Dated records", author: "agent_a"}
    segments = [
      %{label: "Original post · sentence 1", author: "agent_a", result: %{}},
      %{label: "Original post · sentence 2", author: "agent_a", result: %{}}
    ]

    report = SocialConversation.build(topic, [], segments, %{interpretations: [], lexical_links: []})

    assert [%{kind: :same_source_sentence_order, status: :ordering_only}] = report.relations
    assert hd(report.participants).status == :meaning_unresolved
  end

  test "does not infer intent or agreement from external text alone" do
    topic = %{id: "post-1", title: "I agree with this", author: "agent_a"}
    comments = [%{id: "c1", parent_id: "", author: "agent_b"}]

    report = SocialConversation.build(topic, comments, [], %{interpretations: [], lexical_links: []})

    assert report.subject.status == :author_title_only
    assert Enum.all?(report.participants, &(&1.status == :meaning_unresolved))
    assert report.relations == []
    assert report.mode == :read_only_shadow
  end
end
