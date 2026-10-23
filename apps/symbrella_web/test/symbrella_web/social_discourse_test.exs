defmodule SymbrellaWeb.SocialDiscourseTest do
  use ExUnit.Case, async: true

  alias SymbrellaWeb.SocialDiscourse

  test "only established sentence meanings become attributed interpretations" do
    segments = [
      %{status: :analyzed, label: "Original post · sentence 1", author: "agent",
        excerpt: "What makes a community grow?",
        result: %{sentence_meaning: %{status: :structurally_interpreted,
          speech_act: :request_information, kind: :wh_topic_question,
          definition: "The speaker asks about community growth."}}},
      %{status: :analyzed, label: "Original post · sentence 2", author: "agent",
        excerpt: "Community growth may not mean more names.",
        result: %{sentence_meaning: %{status: :not_yet_interpreted}}},
      %{status: :analyzed, label: "Reply 1 · sentence 1", author: "other",
        excerpt: "Community growth is complicated.",
        result: %{sentence_meaning: %{status: :not_yet_interpreted}}}
    ]

    report = SocialDiscourse.build(segments)
    assert report.status == :partial_discourse
    assert length(report.interpretations) == 1
    assert hd(report.interpretations).speech_act == :request_information
    assert hd(report.interpretations).verification == :external_unverified
    assert Enum.any?(report.lexical_links, &(&1.to == 1 and &1.shared_terms == ["community", "grow" ])) == false
    assert Enum.all?(report.lexical_links, &(&1.coreference_verified? == false))
    refute Enum.any?(report.lexical_links, &(&1.to == 2))
  end

  test "auxiliary verbs alone never create discourse links" do
    segments = [
      %{status: :analyzed, label: "Original post · sentence 1", author: "agent",
        excerpt: "Would the group have grown?",
        result: %{sentence_meaning: %{status: :structurally_interpreted,
          speech_act: :request_information, kind: :wh_topic_question,
          definition: "The speaker asks whether the group grew.",
          role_surfaces: %{subject: "the group", predicate: "grown"},
          unresolved_roles: [:topic_possessor_lexical_grounding]}}},
      %{status: :analyzed, label: "Original post · sentence 2", author: "agent",
        excerpt: "Would you say so?",
        result: %{sentence_meaning: %{status: :not_yet_interpreted}}},
      %{status: :analyzed, label: "Original post · sentence 3", author: "agent",
        excerpt: "The group has grown.",
        result: %{sentence_meaning: %{status: :not_yet_interpreted}}}
    ]

    report = SocialDiscourse.build(segments)
    assert length(report.interpretations) == 1
    assert hd(report.interpretations).roles == %{subject: "the group", predicate: "grown"}
    assert hd(report.interpretations).unresolved_roles == [:topic_possessor_lexical_grounding]
    assert Enum.map(report.lexical_links, & &1.to) == [2]
    assert hd(report.lexical_links).shared_terms == ["group", "grown"]
    assert hd(report.lexical_links).coreference_verified? == false
  end

  test "sparse probe results with no sentence_meaning abstain instead of raising" do
    report = SocialDiscourse.build([
      %{status: :analyzed, label: "Original post", author: "agent",
        excerpt: "What does growth mean?", result: %{stage: :social_semantic_shadow}},
      %{status: :unavailable, label: "Reply", author: "agent", reason: :probe_unavailable}
    ])

    assert report.status == :no_supported_sentence_meanings
    assert report.interpretations == []
    assert report.lexical_links == []
  end

  test "grammar-only sentences do not invent discourse links or meaning" do
    report = SocialDiscourse.build([
      %{status: :analyzed, label: "Original post", author: "agent",
        excerpt: "the town grows", result: %{sentence_meaning: %{status: :not_yet_interpreted}}}
    ])
    assert report.status == :no_supported_sentence_meanings
    assert report.interpretations == []
    assert report.lexical_links == []
  end
end
