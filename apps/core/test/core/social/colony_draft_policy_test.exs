defmodule Core.Social.ColonyDraftPolicyTest do
  use ExUnit.Case, async: true

  alias Core.Social.ColonyDraftPolicy

  @post """
  Scored blind against a sealed key: 10 of my checker's 13 links
  were lines that refute the claim. Ten references pointed to
  negations instead of evidence supporting the original assertions.
  """

  @peer_reply """
  If ten of thirteen checker links refuted the claims, topical relevance
  is being mistaken for evidential support. Did the checker classify
  support versus contradiction separately?
  """

  test "a specific peer response is eligible for human review, not auto-posting" do
    assert {:ok, draft} = ColonyDraftPolicy.review(@peer_reply, @post)
    assert draft =~ "evidential support"
    assert draft =~ "Did the checker"
    assert String.length(draft) <= 900
    refute draft =~ "Is there anything else"
  end

  test "assistant-style closings are stripped without removing the substantive reply" do
    generated = @peer_reply <> "\n\nIs there anything else I can assist you with related to this topic?"
    assert {:ok, cleaned} = ColonyDraftPolicy.review(generated, @post)
    refute cleaned =~ "Is there anything else"
    assert cleaned =~ "support versus contradiction"
  end

  test "generic assistant thanks and identity preambles are not approvable" do
    for text <- [
          "Thank you for sharing the checker links and refuted claims.",
          "From my perspective as a conversational companion, checker claims were refuted."
        ] do
      assert {:error, :generic_social_draft} = ColonyDraftPolicy.review(text, @post)
    end
  end

  test "off-topic output is not eligible for approval" do
    assert {:error, :unfocused_social_draft} =
             ColonyDraftPolicy.review("The weather is nice and the sunlight is pleasant.", @post)
  end

  test "untrusted text cannot prompt a credential disclosure" do
    assert {:error, :potential_secret_in_social_draft} =
             ColonyDraftPolicy.review("Checker claims: col_thisIsNotARealKey1234", @post)
  end

  test "overlong output is shortened only at complete sentence boundaries" do
    concise = """
    If ten checker links refute the claims, retrieved relevance cannot
    substitute for evidence support. Did your checker classify the
    polarity of each linked passage before calling it supportive?
    """

    output = concise <> "\n" <> String.duplicate("More explanation about links and claims. ", 40)
    assert String.length(output) > ColonyDraftPolicy.max_chars()

    assert {:ok, shortened} = ColonyDraftPolicy.review(output, @post)
    assert String.length(shortened) <= ColonyDraftPolicy.max_chars()
    assert shortened =~ "polarity of each linked passage"
    assert Regex.match?(~r/[.!?]\z/u, shortened)
    refute shortened =~ String.duplicate("More explanation", 5)
  end

  test "overlong output never hides a credential after the first valid sentences" do
    output =
      @peer_reply <> String.duplicate("Additional checker discussion. ", 50) <>
        "col_exampleTestCredential123456789"

    assert {:error, :potential_secret_in_social_draft} =
             ColonyDraftPolicy.review(output, @post)
  end

  test "long unpunctuated gibberish is not truncated into an approvable draft" do
    gibberish = String.duplicate("checker claims support contradiction ", 50)

    assert {:error, :invalid_social_draft_length} =
             ColonyDraftPolicy.review(gibberish, @post)
  end

  test "a source-attributed checker cannot be claimed as Symbrella's own" do
    context = %{
      source: :colony_external,
      reporter: "exori",
      evidence_relation: :reported_refutation,
      reported_link_count: 10
    }

    assert {:error, :colony_source_attribution_violation} =
             ColonyDraftPolicy.review(
               "Symbrella's memory checker v4 linked ten lines that refuted the claims.",
               @post,
               context
             )
  end

  test "first-person discussion of a checker is not an ownership claim" do
    cognition = %{
      source: :colony_external,
      reporter: "exori",
      evidence_relation: :reported_refutation,
      reported_link_count: 10
    }

    for draft <- [
          "My understanding of the checker is that the ten links refute claims. " <>
            "Does the evidence scoring separate relevance from support?",
          "Our view of the experiment is that checker links can refute claims. " <>
            "How does it detect contradiction?"
        ] do
      assert {:ok, _reviewed} = ColonyDraftPolicy.review(draft, @post, cognition)
    end
  end

  test "explicit checker possession is still blocked for all first-person forms" do
    cognition = %{
      source: :colony_external,
      reporter: "exori",
      evidence_relation: :reported_refutation,
      reported_link_count: 10
    }

    for draft <- [
          "Symbrella's memory checker v4 linked ten refuting lines to claims.",
          "Our checker v4 linked ten refuting lines to claims.",
          "My own memory checker linked ten refuting lines to claims."
        ] do
      assert {:error, :colony_source_attribution_violation} =
               ColonyDraftPolicy.review(draft, @post, cognition)
    end
  end

  test "explicit wrong counted relation is rejected, without forbidding comparisons" do
    context = %{
      source: :colony_external,
      reporter: "exori",
      evidence_relation: :reported_refutation,
      reported_link_count: 10
    }

    assert {:error, :colony_evidence_relation_violation} =
             ColonyDraftPolicy.review(
               "The checker found 10 links supported claims despite its earlier errors.",
               @post,
               context
             )

    assert {:ok, accepted} =
             ColonyDraftPolicy.review(
               "The checker reported 10 links refuting claims, not supporting them. " <>
                 "Did the algorithm distinguish support from contradiction?",
               @post,
               context
             )

    assert accepted =~ "support from contradiction"
  end

  test "reported evidence must not be described as independently confirmed" do
    context = %{
      source: :colony_external,
      reporter: "exori",
      evidence_relation: :reported_refutation,
      reported_link_count: 10,
      report_status: :author_claim_not_independently_verified
    }

    assert {:error, :colony_unverified_report_overclaim} =
             ColonyDraftPolicy.review(
               "The external report confirms that checker v4 linked contradictory claims.",
               @post,
               context
             )

    assert {:ok, _draft} =
             ColonyDraftPolicy.review(
               "The external report states that the checker linked claims to refuting lines. " <>
                 "How were the reported links tested for support versus contradiction?",
               @post,
               context
             )
  end

  test "internal brain labels cannot leak into a public Colony reply" do
    for leaked <- [
      "The external proposition reports that checker links refuted the claims.",
      "LIFG classified the checker links as refuting the claims.",
      "Our cognitive evidence frame links the checker to refuted claims.",
      "The attributed_structural_interpretation explains the checker links."
    ] do
      assert {:error, :colony_internal_jargon} =
               ColonyDraftPolicy.review(leaked, @post)
    end

    natural =
      "You reported that the checker links refuted the claims. " <>
        "How did you distinguish a contradiction from a supporting passage?"

    assert {:ok, ^natural} = ColonyDraftPolicy.review(natural, @post)
  end

  test "empty or excessive output is rejected" do
    assert {:error, :invalid_social_draft_length} = ColonyDraftPolicy.review("", @post)

    assert {:error, :invalid_social_draft_length} =
             ColonyDraftPolicy.review(String.duplicate("claims ", 200), @post)
  end
end
